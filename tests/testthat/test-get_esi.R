context("test-get_esi")

library("chirps")
library("sf")

# Test default method
test_that("two or more points", {
  skip_on_cran()
  set.seed(123)
  lonlat <- data.frame(lon = runif(2, -55, -54),
                       lat = runif(2, -3, -2.7))
  
  dates <- c("2017-12-15", "2018-01-20")
  
  
  df <- get_esi(lonlat, dates)
  
  ok <- is.data.frame(df)
  
  expect_true(ok)
  
})


test_that("one point and other operation works", {
  skip_on_cran()
  set.seed(123)
  lonlat <- data.frame(lon = runif(1, -55, -54),
                       lat = runif(1, -3, -2.7))
  
  dates <- c("2017-12-01", "2018-01-20")
  
  
  df <- get_esi(lonlat, dates, dist = 0.5)
  
  ok <- is.data.frame(df)
  
  expect_true(ok)
  
})

# Test S3 method for 'sf'
test_that("sf method", {
  skip_on_cran()
  set.seed(123)
  lonlat <- data.frame(lon = runif(2, -55, -54),
                       lat = runif(2, -3, -2.7))
  
  lonlat <- st_as_sf(lonlat, coords = c("lon", "lat"))
  
  dates <- c("2017-12-15", "2018-01-20")
  
  df <- get_esi(lonlat, dates)
  
  ok <- is.data.frame(df)
  
  expect_true(ok)
  
})

test_that("sf method returns an sf object", {
  skip_on_cran()
  set.seed(123)
  lonlat <- data.frame(lon = runif(2, -55, -54),
                       lat = runif(2, -3, -2.7))
  
  lonlat <- st_as_sf(lonlat, coords = c("lon", "lat"))
  
  dates <- c("2017-12-15", "2018-01-20")
  
  df <- get_esi(lonlat, dates, as.sf = TRUE)
  
  ok <- "sf" %in% class(df)
  
  expect_true(ok)
  
})



# Test errors
test_that("cloudy data, need to increase buffer", {
  set.seed(123)
  lonlat <- data.frame(lon = runif(1, -55, -54),
                       lat = runif(1, -3, -2.7))
  
  dates <- c("2017-12-01", "2018-01-20")
  
  expect_error(get_esi(lonlat, dates))
  
})
