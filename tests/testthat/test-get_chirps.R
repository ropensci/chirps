context("test-get_chirps")

library("chirps")
library("sf")

# Test default method
test_that("two or more points", {
  
  set.seed(123)
  lonlat <- data.frame(lon = runif(2, -55, -54),
                       lat = runif(2, -3, -2.7))

  dates <- c("2017-12-15","2018-01-20")


  df <- get_chirps(lonlat, dates)
  
  ok <- is.data.frame(df)
  
  expect_true(ok)
  
})


test_that("one point and other operation works", {
  
  set.seed(123)
  lonlat <- data.frame(lon = runif(1, -55, -54),
                       lat = runif(1, -3, -2.7))
  
  dates <- c("2017-12-15","2018-01-20")
  
  
  df <- get_chirps(lonlat, dates, operation = 2)
  
  ok <- is.data.frame(df)
  
  expect_true(ok)
  
})

# Test S3 method for 'sf'
test_that("sf method", {
  
  set.seed(123)
  lonlat <- data.frame(lon = runif(2, -55, -54),
                       lat = runif(2, -3, -2.7))
  
  lonlat <- st_as_sf(lonlat, coords = c("lon","lat"))
  
  dates <- c("2017-12-15","2018-01-20")
  
  df <- get_chirps(lonlat, dates)
  
  ok <- is.data.frame(df)
  
  expect_true(ok)
  
})

test_that("sf method return sf object", {
  
  set.seed(123)
  lonlat <- data.frame(lon = runif(2, -55, -54),
                       lat = runif(2, -3, -2.7))
  
  lonlat <- st_as_sf(lonlat, coords = c("lon","lat"))
  
  dates <- c("2017-12-15","2018-01-20")
  
  df <- get_chirps(lonlat, dates, as.sf = TRUE)
  
  ok <- "sf" %in% class(df)
  
  expect_true(ok)
  
})



# Test errors
test_that("points beyond lims", {
  
  set.seed(123)
  lonlat <- data.frame(lon = runif(1, -55, -54),
                       lat = runif(1, 55, 57))
  
  dates <- c("2017-12-15","2018-01-20")
  
  
  expect_error(
    get_chirps(lonlat, dates)
  ) 
  
})


test_that("wrong dates", {
  
  set.seed(123)
  lonlat <- data.frame(lon = runif(1, -55, -54),
                       lat = runif(1, -3, -2))
  
  dates <- c("2018-12-15","2018-01-20")
  
  
  expect_error(
    get_chirps(lonlat, dates)
  ) 
  
})