load(test_path("test_data.rda"))

# Test get_chirps() default method -----
test_that("get_chirps() returns proper values",
          {
            x_df <-
              get_chirps(data.frame(lonlat),
                         dates = dates,
                         server = "ClimateSERV")
            expect_equal(x_df, chirps_df)
            expect_named(x_df, names(chirps_df))
            expect_equal(nrow(x_df), nrow(chirps_df))
            expect_s3_class(x_df, class(chirps_df))
          })


# Test get_chirps() 'sf' return data frame method -----
test_that("get_chirps() sf method return df", {
  library("sf")
  coords <- st_as_sf(lonlat, coords = c("lon", "lat"))
  x_sf <- get_chirps(object = coords,
                     dates = dates,
                     server = "ClimateSERV")
  expect_equal(x_sf$chirps, chirps_sf$chirps, tolerance = 0.001)
  expect_named(x_sf, names(chirps_df))
  expect_equal(nrow(x_sf), nrow(chirps_df))
  expect_s3_class(x_sf, class(chirps_df))
})

# get chirps with geojson method
# geojson <- as.geojson(coords)
#
# test_that("geojson method", {
#   vcr::use_cassette("geojson_method", {
#     z <- suppressWarnings(get_chirps(geojson, dates, server = "ClimateSERV"))
#
#     z <- round(z$chirps, 2)
#
#     equal <- all(z == chirps$chirps)
#
#     expect_true(equal)
#   })
# })
