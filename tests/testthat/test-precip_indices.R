context("test-precip_indices")

# load("tests/test_data.rda")
load("../test_data.rda")

# Test the default behaviour
values <- c(3, 1, 1, 0, 
            18.51, 27.77, 18.51, 18.51, 
            27.77, 13.88, 2, 1, 0, 2, 
            38.35, 76.71, 0, 0, 76.71, 38.35)

test_that("timespan FALSE", {
  
  p <- precip_indices(precip, timeseries = FALSE)
  
  v <- round(p$value, 2)
  
  equal <- all(v == values)
  
  expect_true(equal)
  
})

# The function can handle timeseries intervals
# here it will return just one interval since we have only 5 days
values2 <- c(3, 1, 0, 0, 9.26, 9.26, 9.26, 9.26, 9.26,
             9.26, 1, 1, 0, 2, 38.35, 76.71, 0, 0, 76.71, 38.35)

test_that("timespan TRUE", {

  p <- precip_indices(precip, timeseries = TRUE, intervals = 4)
  
  v <- round(p$value, 2)
  
  equal <- all(v == values2)
  
  expect_true(equal)
  
})

# The function can handle NAs
values3 <- c(2, 1, 1, 0, 18.51, 27.77, 18.51, 18.51, 
             27.77, 9.26, 2, 1, 0, 2, 38.35, 76.71, 0, 0, 76.71, 25.57)

test_that("accepts NAs", {
  
  dt <- precip
  
  dt[c(2, 7), "chirps"] <- NA
  
  p <- precip_indices(dt)
  
  v <- round(p$value, 2)
  
  equal <- all(v == values3)
  
  expect_true(equal)
  
})

# Get an error with non chirps data
test_that("non chirps data", {
  
  expect_error(precip_indices(object = airquality))

})
