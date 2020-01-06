context("test-precip_incides")

library("chirps")

test_that("timespan FALSE", {
  skip_on_cran()
  lonlat <- data.frame(lon = c(-55.0281, -55.0714),
                       lat = c(-2.8094, -3.5279))
  
  dates <- c("2017-12-15", "2018-01-31")
  
  df <- get_chirps(lonlat, dates)
  # take the indices for the entire period
  p <- precip_indices(df, timeseries = FALSE)
  d <- dim(p)
  
  expect_equal(d, c(20, 6))
  
})


test_that("timespan TRUE", {
  skip_on_cran()
  # take the indices for the entire period
  p <- precip_indices(df, timeseries = TRUE, intervals = 10)
  d <- dim(p)
  
  expect_equal(d, c(80, 6))
  
})


test_that("accepts NAs", {
  skip_on_cran()
  df2 <- df
  
  df2[c(2, 7, 65, 78), "chirps"] <- NA
  
  # take the indices for the entire period
  p <- precip_indices(df2)
  
  p <- all(!is.na(p$value))
  
  expect_true(p)
  
})


test_that("non chirps data", {
  expect_error(precip_indices(object = airquality))
  
})
