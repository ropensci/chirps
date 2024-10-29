

# setup for tests, expected values for all

imerg_lonlat <- data.frame(lon = c(-55.0281, -54.9857),
                           lat = c(-2.8094, -2.8756))
imerg_dates <- c("2017-12-15", "2017-12-31")
imerg_values <-
  c(
    3.70000004768372,
    0.699999988079071,
    29.2000007629395,
    12.6999998092651,
    20.6000003814697,
    1,
    0.600000023841858,
    0.900000035762787,
    2.10000014305115,
    3.90000009536743,
    3,
    8.60000038146973,
    88.0999984741211,
    14,
    0.300000011920929,
    16.7000007629395,
    11.4000005722046,
    2.60000014305115,
    0.5,
    32.5,
    7.5,
    18.2000007629395,
    0.900000035762787,
    0.300000011920929,
    0.5,
    3,
    2.5,
    2.70000004768372,
    11.4000005722046,
    74.5,
    17.3999996185303,
    0.300000011920929,
    15.4000005722046,
    8.40000057220459
  )
imerg_names <- c("id", "lon", "lat", "date", "imerg")

# Test get_imerg() default method -----
test_that("get_imerg() returns proper values", {
  x_df <- get_imerg(imerg_lonlat, dates = imerg_dates)
  expect_equal(x_df$imerg, imerg_values, tolerance = 0.01)
  expect_named(x_df, imerg_names)
  expect_equal(nrow(x_df), 34)
  expect_s3_class(x_df, "chirps_df")
})

sf_coords <- st_as_sf(imerg_lonlat,
                      coords = c("lon", "lat"),
                      crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

# Test get_imerg() 'sf' return data frame method -----
test_that("get_imerg() sf method return df", {
  library("sf")
  x_sf <- get_imerg(object = sf_coords, dates = imerg_dates)
  expect_equal(x_sf$imerg, imerg_values, tolerance = 0.01)
  expect_named(x_sf, imerg_names)
  expect_equal(nrow(x_sf), 34)
  expect_s3_class(x_sf, "chirps_df")
})

# get_imerg with sf method
test_that("geojson method return sf", {
  x_return_sf <- get_imerg(sf_coords, imerg_dates, as.sf = TRUE)
  expect_s3_class(x_return_sf, "sf")
})

gjson <- as.geojson(imerg_lonlat)
# get chirps with geojson method
test_that("geojson method", {
  x_gjson <- get_imerg(gjson, imerg_dates)
  expect_equal(x_gjson$imerg, imerg_values, tolerance = 0.01)
})

# get_imerg with geojson method
test_that("geojson method return sf", {
  x_return_gjson <- get_imerg(gjson, imerg_dates, as.geojson = TRUE)
  expect_s3_class(x_return_gjson, "geojson")
})
