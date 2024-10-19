load(test_path("test_data.rda"))

# Test get_chirps() default method -----
test_that("get_chirps() returns proper values", {
  x_df <-
    get_chirps(data.frame(lonlat), dates = dates, server = "ClimateSERV")
  expect_equal(x_df, chirps_df)
  expect_named(x_df, names(chirps_df))
  expect_equal(nrow(x_df), nrow(chirps_df))
  expect_s3_class(x_df, class(chirps_df))
})


# Test get_chirps() 'sf' return data frame method -----
test_that("get_chirps() sf method return df", {
  library("sf")
  sf_coords <- st_as_sf(lonlat, coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  x_sf <- get_chirps(object = sf_coords,
                     dates = dates,
                     server = "ClimateSERV")
  expect_equal(x_sf$chirps, chirps_df$chirps, tolerance = 0.001)
  expect_named(x_sf, names(chirps_df))
  expect_equal(nrow(x_sf), nrow(chirps_df))
  expect_s3_class(x_sf, class(chirps_df))
})

# get chirps with geojson method
test_that("geojson method", {
  geojson_coords <- as.geojson(lonlat)
  x_gjson <- get_chirps(geojson_coords, dates, server = "ClimateSERV")
  expect_equal(x_gjson$chirps, chirps_df$chirps, tolerance = 0.001)
  expect_named(x_gjson, names(chirps_df))
  expect_equal(nrow(x_gjson), nrow(chirps_df))
  expect_s3_class(x_gjson, class(chirps_df))
})
