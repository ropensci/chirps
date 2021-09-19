# Test if get_chirps() returns a properly formatted object.
# for this we downloaded two points from https://climateserv.servirglobal.net/

# Test default method -----
test_that("default method", {
  x <- get_chirps(lonlat, dates)
  expect_named(x, c("id", "lon", "lat", "date", "chirps"))
  expect_equal(nrow(x), 10)
  expect_s3_class(x, c("chirps", "chirps_df", "data.frame"))
})

# Test `sf` method -----
test_that("sf method", {
  y <- get_chirps(coords, dates)
  expect_named(y, c("id", "lon", "lat", "date", "chirps"))
  expect_equal(nrow(y), 10)
  expect_s3_class(y, c("chirps", "chirps_df", "data.frame"))
})

# Test geojson method -----
test_that("geojson method", {
  z <- suppressWarnings(get_chirps(geojson, dates))
  expect_named(z, c("id", "lon", "lat", "date", "chirps"))
  expect_equal(nrow(z), 10)
  expect_s3_class(z, c("chirps", "chirps_df", "data.frame"))
})
