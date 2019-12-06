context("test-get_chirps")

library("chirps")

test_that("two or more points", {
  
  lonlat <- data.frame(lon = runif(2, -55, -54),
                       lat = runif(2, -3, -2.7))

  dates <- c("2017-12-15","2018-01-20")


  df <- get_chirps(lonlat, dates)
  
  ok <- is.data.frame(df)
  
  expect_true(ok)
  
})


test_that("one point and other operation works", {
  
  lonlat <- data.frame(lon = runif(1, -55, -54),
                       lat = runif(1, -3, -2.7))
  
  dates <- c("2017-12-15","2018-01-20")
  
  
  df <- get_chirps(lonlat, dates, operation = 2)
  
  ok <- is.data.frame(df)
  
  expect_true(ok)
  
})


test_that("points beyond lims", {
  
  lonlat <- data.frame(lon = runif(1, -55, -54),
                       lat = runif(1, 55, 57))
  
  dates <- c("2017-12-15","2018-01-20")
  
  
  expect_error(
    get_chirps(lonlat, dates)
  ) 
  
})


test_that("wrong dates", {
  
  lonlat <- data.frame(lon = runif(1, -55, -54),
                       lat = runif(1, -3, -2))
  
  dates <- c("2018-12-15","2018-01-20")
  
  
  expect_error(
    get_chirps(lonlat, dates)
  ) 
  
})