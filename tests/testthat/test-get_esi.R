# load("tests/test_data.rda")
load("../test_data.rda")

# Test get_esi() -----
test_that("get_esi", {
  x <- get_esi(lonlat, dates = c("2002-01-01", "2002-01-31"))
  
  expect_named(x, c("id", "lon", "lat", "date", "esi"))
  expect_equal(nrow(x), 10)
  expect_s3_class(x, c("chirps", "chirps_df", "data.frame"))
})
