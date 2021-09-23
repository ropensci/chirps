
# test_data.rda contains lat/lon and date values for the following tests
load(test_path("test_data.rda"))

# Test get_chirts() default method -----
test_that("get_chirts() returns proper values", {
  x <- get_chirts(object = lonlat,
                  dates = dates,
                  var = "Tmax")
  
  expect_named(x, c("id", "lon", "lat", "date", "chirts"))
  expect_equal(nrow(x), 10)
  expect_s3_class(x, c("chirts", "chirts_df", "data.frame"))
})
