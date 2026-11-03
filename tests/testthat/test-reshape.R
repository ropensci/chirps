# Offline tests: the downloads are replaced by a small local raster where
# each value encodes its day and its cell, so the pairing of location, date
# and value can be checked exactly

dates = c("2017-12-15", "2017-12-17")
days = seq.Date(as.Date(dates[1]), as.Date(dates[2]), by = "day")

fake_raster = function(dates, ...) {
  r = terra::rast(nrows = 4, ncols = 4,
                  xmin = -56, xmax = -54, ymin = -4, ymax = -2,
                  nlyrs = 3, crs = "EPSG:4326")
  # value = day * 1000 + cell
  terra::values(r) = sapply(1:3, function(i) i * 1000 + seq_len(16))
  names(r) = paste0("chirps-v2.0.", gsub("-", ".", days))
  terra::time(r) = days
  r
}

lonlat = data.frame(lon = c(-55.8, -54.2), lat = c(-2.2, -3.8))
cells = terra::cellFromXY(fake_raster(), as.matrix(lonlat))

test_that(".to_long pairs each value with its id and date", {
  v = data.frame(d1 = c(1, 10), d2 = c(2, 20), d3 = c(3, 30))
  x = chirps:::.to_long(v, 1:2, c(-55, -54), c(-2, -3), days, "chirps")
  expect_equal(x$id, rep(1:2, each = 3))
  expect_equal(x$date, rep(days, times = 2))
  expect_equal(x$chirps, c(1, 2, 3, 10, 20, 30))
  expect_equal(x$lon, rep(c(-55, -54), each = 3))
})

test_that(".to_wide reverses .to_long", {
  v = data.frame(d1 = c(1, 10), d2 = c(2, 20), d3 = c(3, 30))
  x = chirps:::.to_long(v, 1:2, NA, NA, days, "chirps")
  m = chirps:::.to_wide(x, "chirps", 1:2, days, "d")
  expect_equal(unname(m), unname(as.matrix(v)))
})

test_that("get_chirps() data.frame method pairs dates and points", {
  local_mocked_bindings(get_chirps_raw = fake_raster)
  x = get_chirps(lonlat, dates, server = "CHC")
  expect_s3_class(x, c("chirps", "chirps_df", "data.frame"))
  expect_equal(nrow(x), 6)
  expect_equal(x$id, rep(1:2, each = 3))
  expect_equal(x$date, rep(days, times = 2))
  expect_equal(x$lon, rep(lonlat$lon, each = 3))
  expect_equal(x$chirps, c(cells[1] + (1:3) * 1000, cells[2] + (1:3) * 1000))
  
  m = get_chirps(lonlat, dates, server = "CHC", as.matrix = TRUE)
  expect_true(is.matrix(m))
  expect_equal(dim(m), c(2, 3))
  expect_equal(unname(m[2, ]), cells[2] + (1:3) * 1000)
})

test_that("get_chirps() sf method pairs dates and points", {
  local_mocked_bindings(get_chirps_raw = fake_raster)
  s = sf::st_as_sf(lonlat, coords = c("lon", "lat"), crs = 4326)
  x = get_chirps(s, dates, server = "CHC")
  expect_equal(x$date, rep(days, times = 2))
  expect_equal(x$chirps[x$id == 2], cells[2] + (1:3) * 1000)
  
  y = get_chirps(s, dates, server = "CHC", as.sf = TRUE)
  expect_s3_class(y, "sf")
  expect_equal(y[[paste0("day_", as.integer(days[3]))]], cells + 3000)
})

test_that("get_chirps() sf method takes one centroid per polygon", {
  local_mocked_bindings(get_chirps_raw = fake_raster)
  s = sf::st_as_sf(lonlat, coords = c("lon", "lat"), crs = 4326)
  p = sf::st_buffer(sf::st_transform(s, 3857), 100)
  p = sf::st_transform(p, 4326)
  p$name = c("a", "b")
  x = get_chirps(p, dates, server = "CHC")
  expect_equal(nrow(x), 6)
  expect_equal(x$lon, rep(lonlat$lon, each = 3), tolerance = 1e-4)
  expect_equal(x$chirps[x$id == 2], cells[2] + (1:3) * 1000)
})

test_that("get_chirps() SpatVector method pairs dates and points", {
  local_mocked_bindings(get_chirps_raw = fake_raster)
  v = terra::vect(as.matrix(lonlat), crs = "EPSG:4326")
  x = get_chirps(v, dates, as.raster = FALSE)
  expect_equal(x$date, rep(days, times = 2))
  expect_equal(x$lon, rep(lonlat$lon, each = 3))
  expect_equal(x$chirps[x$id == 1], cells[1] + (1:3) * 1000)
})

test_that("get_chirps() raster and extent methods crop", {
  local_mocked_bindings(get_chirps_raw = fake_raster)
  e = terra::ext(-56, -55, -3, -2)
  r = get_chirps(e, dates)
  expect_s4_class(r, "SpatRaster")
  expect_equal(terra::nlyr(r), 3)
  r2 = get_chirps(terra::rast(e, nrows = 2, ncols = 2), dates)
  expect_s4_class(r2, "SpatRaster")
  m = get_chirps(e, dates, as.raster = FALSE)
  expect_true(is.matrix(m))
})

test_that("get_chirts() returns temperature for rasters and extents", {
  called = NULL
  local_mocked_bindings(
    get_chirts_raw = function(dates, var, ...) {
      called <<- var
      fake_raster(dates)
    },
    get_chirps_raw = function(...) stop("CHIRPS should not be requested")
  )
  e = terra::ext(-56, -55, -3, -2)
  r = get_chirts(e, dates, var = "Tmax")
  expect_s4_class(r, "SpatRaster")
  expect_equal(called, "Tmax")
  r2 = get_chirts(terra::rast(e, nrows = 2, ncols = 2), dates, var = "Tmin")
  expect_equal(called, "Tmin")
})

test_that("get_chirts() data.frame method pairs dates and points", {
  local_mocked_bindings(get_chirts_raw = function(dates, var, ...) {
    fake_raster(dates)
  })
  x = get_chirts(lonlat, dates, var = "Tmax")
  expect_equal(x$date, rep(days, times = 2))
  expect_equal(x$chirts[x$id == 2], cells[2] + (1:3) * 1000)
})

test_that("get_chirps() accepts version as an argument", {
  local_mocked_bindings(get_chirps_raw = function(dates, version = "2.0", 
                                                  ...) {
    expect_equal(version, "3.0")
    fake_raster(dates)
  })
  x = get_chirps(lonlat, dates, server = "CHC", version = "3.0")
  expect_equal(nrow(x), 6)
})

test_that("raw URLs point to the CHC archive", {
  d = as.Date(c("2017-12-15", "2018-01-01"))
  u = chirps:::.chirps_raw_urls(d, version = "2.0", resolution = 0.05)
  expect_equal(u[1], paste0("https://data.chc.ucsb.edu/products/CHIRPS/v2.0/",
                            "global_daily/cogs/p05/2017/",
                            "chirps-v2.0.2017.12.15.cog"))
  u = chirps:::.chirps_raw_urls(d, version = "2.0", resolution = 0.25)
  expect_match(u[2], "/p25/2018/chirps-v2.0.2018.01.01.cog$")
  u = chirps:::.chirps_raw_urls(d, version = "3.0", type = "sat")
  expect_match(u[1], "v3.0/daily/final/sat/2017/chirps-v3.0.sat.2017.12.15.tif$")
  u = chirps:::.chirts_raw_urls(as.Date("2000-01-01"), var = "Tmax")
  expect_equal(u, paste0("https://data.chc.ucsb.edu/products/CHIRTS/daily/",
                         "v1.0/global_tifs_p05/Tmax/2000/Tmax.2000.01.01.tif"))
  expect_equal(chirps:::.chirps_raw_urls(d[1]),
               chirps:::.chirps_raw_urls(d[1], version = "3.0", type = "rnl"))
  expect_error(chirps:::.chirts_raw_urls(as.Date("2020-01-01"), var = "Tmax"))
})
