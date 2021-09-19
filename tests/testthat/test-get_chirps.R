# Test if get_chirps() returns a properly formatted object.
# for this we downloaded two points from https://climateserv.servirglobal.net/

load("../test_data.rda")

# Test default method -----
test_that("default_method returns proper values", {
  vcr::use_cassette("default_method", {
    x <- get_chirps(lonlat, dates)
  })
  expect_named(x, c("id", "lon", "lat", "date", "chirps"))
  expect_equal(nrow(x), 10)
  expect_s3_class(x, c("chirps", "chirps_df", "data.frame"))
})

# Test `sf` method -----
coords <- st_as_sf(lonlat, coords = c("lon", "lat"))
test_that("sf_method", {
  vcr::use_cassette("sf_method", {
    y <- get_chirps(coords, dates)
  })
  expect_named(y, c("id", "lon", "lat", "date", "chirps"))
  expect_equal(nrow(y), 10)
  expect_s3_class(y, c("chirps", "chirps_df", "data.frame"))
})

# Test geojson method -----
geojson <- as.geojson(lonlat)
test_that("geojson_method", {
  vcr::use_cassette("geojson_method", {
    z <- suppressWarnings(get_chirps(geojson, dates))
  })
  expect_named(z, c("id", "lon", "lat", "date", "chirps"))
  expect_equal(nrow(z), 10)
  expect_s3_class(z, c("chirps", "chirps_df", "data.frame"))
})
