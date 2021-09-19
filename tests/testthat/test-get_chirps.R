# test_data.rda contains lat/lon and date values for the following test
load("../test_data.rda")

# Test get_chirps() -----
test_that("get_chirps() returns proper values", {
  vcr::use_cassette("CHIRPS_default", {
    x <- get_chirps(lonlat, dates)
  })
  expect_named(x, c("id", "lon", "lat", "date", "chirps"))
  expect_equal(nrow(x), 10)
  expect_s3_class(x, c("chirps_df", "data.frame"))
})

# Test sf data frame method -----
coords <- st_as_sf(lonlat, coords = c("lon", "lat"))
test_that("get_chirps() sf method return df", {
  vcr::use_cassette("CHIRPS_sf_method_return_df", {
    x <- get_chirps(coords, dates)
  })
  expect_named(x, c("id", "lon", "lat", "date", "chirps"))
  expect_equal(nrow(x), 10)
  expect_s3_class(x, c("chirps_df", "data.frame"))
})
