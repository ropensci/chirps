
# setup for tests
esi_lonlat <- data.frame(lon = c(-55.0281, -54.9857),
                         lat = c(-2.8094, -2.8756))
esi_dates <- c("2017-12-15", "2018-06-20")
esi_values <- c(NA, NA, 0.85, 0.54)

# Test get_esi() -----
test_that("get_esi() returns proper values", {
  x <- get_esi(esi_lonlat, esi_dates)
  expect_named(x, c("id", "lon", "lat", "date", "esi"))
  expect_equal(nrow(x), 4)
  expect_s3_class(x, c("chirps_df", "data.frame"))
  expect_equal(x$esi, esi_values, tolerance = 0.01)
})

# Test sf data frame method -----
coords <- sf::st_as_sf(esi_lonlat, coords = c("lon", "lat"),
                       crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

test_that("get_esi() sf method return_df", {
  y <- get_esi(coords, esi_dates)
  expect_named(y, c("id", "lon", "lat", "date", "esi"))
  expect_equal(nrow(y), 4)
  expect_s3_class(y, c("chirps_df", "data.frame"))
  expect_equal(y$esi, esi_values, tolerance = 0.01)
})


# Test sf `sf` method -----
test_that("get_esi() sf method return sf when 'sf' == TRUE", {
  y <- get_esi(coords,
               esi_dates,
               as.sf = TRUE)
  expect_named(y, c("day_17518", "day_17525", "geometry"))
  expect_equal(nrow(y), 2)
  expect_s3_class(y, c("sf", "data.frame"))
})


# Test geojson data frame method -----
gjson <- as.geojson(esi_lonlat)

test_that("get_esi() geojson method return df", {
  z <-
    get_esi(gjson,
            dates = c("2002-01-01", "2002-01-31"))
  expect_named(z, c("id", "lon", "lat", "date", "esi"))
  expect_equal(nrow(z), 10)
  expect_s3_class(z, c("chirps_df", "data.frame"))
})

# Test geojson `geojson` method -----
test_that("get_esi() geojson method return geojson", {
  z <-
    get_esi(gjson,
            dates = c("2002-01-01", "2002-01-31"),
            as.geojson = TRUE)
  expect_named(z, c("1", "2"))
  expect_equal(length(z), 2)
  expect_s3_class(z, c("geojson", "json", "character"))
})
