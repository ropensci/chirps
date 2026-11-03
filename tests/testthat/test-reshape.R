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

test_that(".as_dates accepts one or two dates and rejects other input", {
  expect_equal(chirps:::.as_dates("2010-12-15"), c("2010-12-15", "2010-12-15"))
  expect_equal(chirps:::.as_dates(as.Date(c("2010-12-15", "2010-12-31"))),
               c("2010-12-15", "2010-12-31"))
  expect_error(chirps:::.as_dates(data.frame(x = 1)), "dates")
  expect_error(chirps:::.as_dates(c("2010-12-15", "2010-12-16", "2010-12-17")),
               "one date")
  expect_error(chirps:::.as_dates("15/12/2010"), "YYYY-MM-DD")
  expect_error(chirps:::.validate_dates(c("1982-01-01", "1982-12-31")), NA)
})

test_that("get_chirps() and get_chirts() return a single day", {
  local_mocked_bindings(
    get_chirps_raw = function(dates, ...) fake_raster(dates)[[1]],
    get_chirts_raw = function(dates, var, ...) fake_raster(dates)[[1]]
  )
  x = get_chirps(lonlat, "2017-12-15", server = "CHC")
  expect_equal(nrow(x), 2)
  expect_equal(x$chirps, cells + 1000)
  y = get_chirts(lonlat, "2017-12-15", var = "Tmax")
  expect_equal(nrow(y), 2)
})

test_that("get_chirps() summarises sf polygons over their cells", {
  local_mocked_bindings(get_chirps_raw = fake_raster)
  # one polygon inside cells 1, 2, 5 and 6, one inside cell 1 only
  sq = function(x0, x1, y0, y1) {
    sf::st_polygon(list(rbind(c(x0, y0), c(x1, y0), c(x1, y1), c(x0, y1),
                              c(x0, y0))))
  }
  p = sf::st_sf(name = c("big", "small"),
                geometry = sf::st_sfc(sq(-55.9, -55.1, -2.9, -2.1),
                                      sq(-55.9, -55.8, -2.2, -2.1),
                                      crs = 4326))
  x = get_chirps(p, dates, server = "CHC")
  expect_equal(nrow(x), 6)
  expect_equal(x$chirps[x$id == 1], (1:3) * 1000 + mean(c(1, 2, 5, 6)))
  expect_equal(x$chirps[x$id == 2], (1:3) * 1000 + 1)
  expect_equal(x$lon[x$id == 1], rep(-55.5, 3), tolerance = 1e-6)
  
  s = get_chirps(p, dates, server = "CHC", operation = 4)
  expect_equal(s$chirps[s$id == 1], (1:3) * 4000 + sum(c(1, 2, 5, 6)))
  
  expect_error(get_chirps(p, dates, server = "CHC", operation = 3),
               "operation")
  
  # projected polygons are transformed to longitude and latitude
  pp = sf::st_transform(p, 3857)
  y = get_chirps(pp, dates, server = "CHC")
  expect_equal(y$chirps, x$chirps)
})

test_that("as.raster = TRUE crops to the points of a data.frame", {
  local_mocked_bindings(
    get_chirps_raw = fake_raster,
    get_chirts_raw = function(dates, var, ...) fake_raster(dates)
  )
  r = get_chirps(lonlat, dates, server = "CHC", as.raster = TRUE)
  expect_s4_class(r, "SpatRaster")
  expect_equal(terra::nlyr(r), 3)
  one = get_chirps(lonlat[1, ], dates, server = "CHC", as.raster = TRUE)
  expect_true(terra::ncell(one) >= 1)
  r2 = get_chirts(lonlat, dates, var = "Tmax", as.raster = TRUE)
  expect_s4_class(r2, "SpatRaster")
})

test_that("ClimateSERV operations map to summary functions", {
  expect_identical(chirps:::.operation_fun(5), mean)
  expect_identical(chirps:::.operation_fun(NULL), mean)
  expect_identical(chirps:::.operation_fun(0), max)
  expect_error(chirps:::.operation_fun(9), "operation")
})

test_that("matrices have one column per day, named by date", {
  local_mocked_bindings(get_chirps_raw = fake_raster)
  m = get_chirps(lonlat, dates, server = "CHC", as.matrix = TRUE)
  expect_equal(colnames(m), format(days))
  e = get_chirps(terra::ext(-56, -55, -3, -2), dates, as.raster = FALSE)
  expect_equal(colnames(e), format(days))
})

test_that("CHIRPS v3 accepts latitudes up to 60 degrees, v2 up to 50", {
  local_mocked_bindings(get_chirps_raw = fake_raster)
  north = data.frame(lon = -55, lat = 55)
  expect_error(chirps:::.validate_lonlat(north, ylim = chirps:::.chirps_ylim()), NA)
  expect_error(get_chirps(north, dates, server = "CHC", version = "2.0"),
               "latitude -50 to 50")
  expect_equal(chirps:::.chirps_ylim(), c(-60, 60))
  expect_equal(chirps:::.chirps_ylim("3.0", "ClimateSERV"), c(-50, 50))
})

test_that("SpatVector polygons are summarised like sf polygons", {
  local_mocked_bindings(get_chirps_raw = fake_raster)
  sq = function(x0, x1, y0, y1) {
    sf::st_polygon(list(rbind(c(x0, y0), c(x1, y0), c(x1, y1), c(x0, y1),
                              c(x0, y0))))
  }
  p = sf::st_sf(geometry = sf::st_sfc(sq(-55.9, -55.1, -2.9, -2.1),
                                      sq(-55.9, -55.8, -2.2, -2.1),
                                      crs = 4326))
  v = terra::vect(p)
  x = get_chirps(v, dates, as.raster = FALSE)
  expect_equal(nrow(x), 6)
  expect_equal(x$chirps[x$id == 1], (1:3) * 1000 + mean(c(1, 2, 5, 6)))
  expect_equal(x$chirps, get_chirps(p, dates, server = "CHC")$chirps)
  m = get_chirps(v, dates, as.raster = FALSE, as.matrix = TRUE)
  expect_equal(dim(m), c(2, 3))
  expect_equal(colnames(m), format(days))
})

test_that("get_chirts() and get_chirts_raw() read CHIRTS-ERA5 with product", {
  called = NULL
  local_mocked_bindings(
    get_chirts_raw = function(dates, var, product = "CHIRTS-daily", ...) {
      called <<- c(var, product)
      fake_raster(dates)
    }
  )
  x = get_chirts(lonlat, dates, var = "Tmin", product = "CHIRTS-ERA5")
  expect_equal(called, c("Tmin", "CHIRTS-ERA5"))
  expect_equal(x$chirts[x$id == 2], cells[2] + (1:3) * 1000)
  x = get_chirts(lonlat, c("1980-06-01", "1980-06-03"), var = "Tmax",
                 product = "CHIRTS-ERA5")
  expect_equal(nrow(x), 6)
})

test_that("get_chirts_raw() checks the variable and years of each product", {
  expect_error(get_chirts_raw(dates, var = "RHum", product = "CHIRTS-ERA5"),
               "Tmax")
  expect_error(chirps:::.chirts_raw_urls(as.Date("2020-01-01"), var = "Tmax"),
               "CHIRTS-ERA5")
})

test_that("notices are shown once per session", {
  assign("climateserv_noticed", FALSE, envir = chirps:::.chirps_env)
  expect_message(chirps:::.notice_climateserv(), "CHIRPS v2 only")
  expect_no_message(chirps:::.notice_climateserv())
  
  local_mocked_bindings(get_chirps_raw = fake_raster)
  x = get_chirps(lonlat, dates, server = "CHC")
  assign("indices_noticed", FALSE, envir = chirps:::.chirps_env)
  expect_message(precip_indices(x), "climatrends")
  expect_no_message(precip_indices(x))
})

test_that("sfc input works in get_chirps() and sf input in get_chirts()", {
  local_mocked_bindings(
    get_chirps_raw = fake_raster,
    get_chirts_raw = function(dates, var, ...) fake_raster(dates)
  )
  s = sf::st_as_sf(lonlat, coords = c("lon", "lat"), crs = 4326)
  x = get_chirps(sf::st_geometry(s), dates)
  expect_equal(x$chirps, get_chirps(s, dates)$chirps)
  y = get_chirts(s, dates, var = "Tmax")
  expect_equal(y$chirts[y$id == 2], cells[2] + (1:3) * 1000)
  expect_equal(y$lon, rep(lonlat$lon, each = 3))
})

test_that("as.matrix works for sf, and polygons without a CRS are read", {
  local_mocked_bindings(get_chirps_raw = fake_raster)
  s = sf::st_as_sf(lonlat, coords = c("lon", "lat"), crs = 4326)
  m = get_chirps(s, dates, as.matrix = TRUE)
  expect_equal(colnames(m), format(days))
  expect_equal(unname(m[2, ]), cells[2] + (1:3) * 1000)
  sq = sf::st_polygon(list(rbind(c(-55.9, -2.9), c(-55.1, -2.9),
                                 c(-55.1, -2.1), c(-55.9, -2.1),
                                 c(-55.9, -2.9))))
  p = sf::st_sf(geometry = sf::st_sfc(sq))
  x = get_chirps(p, dates)
  expect_equal(x$chirps, (1:3) * 1000 + mean(c(1, 2, 5, 6)))
})

test_that("each request says which data it downloads", {
  d = as.Date(c("2017-12-15", "2017-12-16"))
  old = options(chirps.quiet = FALSE)
  on.exit(options(old))
  expect_message(chirps:::.inform_request("CHIRPS v3.0 (rnl)",
                                          "daily rainfall (mm)", d),
                 "daily rainfall \\(mm\\) from CHIRPS v3.0 \\(rnl\\).*2 days")
  expect_message(chirps:::.inform_request("CHIRTS-daily",
                                          chirps:::.chirts_label("Tmax"), d[1]),
                 "maximum temperature.*2017-12-15 \\(1 day\\)")
  options(chirps.quiet = TRUE)
  expect_no_message(chirps:::.inform_request("CHIRTS-ERA5", "x", d))
})
