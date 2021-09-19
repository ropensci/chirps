
# test_data.rda contains lat/lon and date values for the following tests
load("../test_data.rda")

# Test get_chirps() -----
test_that("get_chirps() returns proper values", {
  vcr::use_cassette("CHIRPS_default", {
    x <- get_chirps(lonlat, dates)
  })
  expect_named(x, c("id", "lon", "lat", "date", "chirps"))
  expect_equal(nrow(x), 10)
  expect_s3_class(x, c("chirps", "chirps_df", "data.frame"))
})

# Test sf return data frame method -----
coords <- st_as_sf(lonlat, coords = c("lon", "lat"))
test_that("get_chirps() sf method return df", {
  vcr::use_cassette("CHIRPS_sf_method_return_df", {
    x <- get_chirps(coords, dates)
  })
  expect_named(x, c("id", "lon", "lat", "date", "chirps"))
  expect_equal(nrow(x), 10)
  expect_s3_class(x, c("chirps", "chirps_df", "data.frame"))
})

# Test sf return `sf` method -----
test_that("get_chirps() sf method return sf", {
  vcr::use_cassette("CHIRPS_sf_method_return_sf", {
    x <- get_chirps(coords, dates, as.sf = TRUE)
  })
  expect_named(x, c("day_10957",
                    "day_10958",
                    "day_10959",
                    "day_10960",
                    "day_10961",
                    "geometry"))
  expect_equal(nrow(x), 2)
  expect_s3_class(x, c("sf", "data.frame"))
})
