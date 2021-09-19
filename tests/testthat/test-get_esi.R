
# test_data.rda contains lat/lon and date values for the following test
load("../test_data.rda")

# Test get_esi() -----
test_that("get_esi returns proper values", {
  vcr::use_cassette("ESI_default", {
    x <- get_esi(lonlat, dates = c("2002-01-01", "2002-01-31"))
  })
  expect_named(x, c("id", "lon", "lat", "date", "esi"))
  expect_equal(nrow(x), 4)
  expect_s3_class(x, c("chirps_df", "data.frame"))
})

# Test sf data frame method -----
coords <- st_as_sf(lonlat, coords = c("lon", "lat"))
test_that("get_esi() sf method return df", {
  vcr::use_cassette("ESI_sf_method_return_df", {
    y <- get_esi(coords, dates = c("2002-01-01", "2002-01-31"))
  })
  expect_named(y, c("id", "lon", "lat", "date", "esi"))
  expect_equal(nrow(y), 3)
  expect_s3_class(y, c("chirps_df", "data.frame"))
})

# Test sf `sf` method -----
coords <- st_as_sf(lonlat, coords = c("lon", "lat"))
test_that("get_esi() sf method return sf", {
  vcr::use_cassette("ESI_sf_method_return_sf", {
    y <- get_esi(coords,
                 dates = c("2002-01-01", "2002-01-31"),
                 as.sf = TRUE)
  })
  expect_named(y, c("day_11688", "day_11696", "day_11704", "geometry"))
  expect_equal(nrow(y), 2)
  expect_s3_class(y, c("sf", "data.frame"))
})

# Test geojson data frame method -----
geojson <- as.geojson(lonlat)
test_that("get_esi() geojson method return df", {
  vcr::use_cassette("ESI_geojson_method_return_df", {
    z <-
      suppressWarnings(get_esi(geojson,
                               dates = c("2002-01-01", "2002-01-31")))
  })
  expect_named(z, c("id", "lon", "lat", "date", "esi"))
  expect_equal(nrow(z), 3)
  expect_s3_class(z, c("chirps_df", "data.frame"))
})

# Test geojson `geojson` method -----
geojson <- as.geojson(lonlat)
test_that("get_esi() geojson method return geojson", {
  vcr::use_cassette("ESI_geojson_method_return_geojson", {
    z <-
      suppressWarnings(get_esi(
        geojson,
        dates = c("2002-01-01", "2002-01-31"),
        as.geojson = TRUE
      ))
  })
  expect_named(z, c("1", "2"))
  expect_equal(length(z), 2)
  expect_s3_class(z, c("geojson", "json", "character"))
})
