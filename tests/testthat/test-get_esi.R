context("test-get_esi")

# load("tests/test_data.rda")
load("../test_data.rda")

# Test if get_chirps fetch the correct values,
# for this we downloaded two points from 
# https://climateserv.servirglobal.net/
# and will compare it with the values retrieved by get_chirps

values <- c(NA, -1.89, -1.92, -2.12)

# Test default method
test_that("default method", {
  skip_on_cran()
  
  x <- get_esi(lonlat, dates = c("2002-01-01", "2002-01-31"))
  
  x <- round(x$esi, 2)
  
  equal <- all(x == values, na.rm = TRUE)
  
  expect_true(equal)
  
})