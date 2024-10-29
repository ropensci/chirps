
# Test .validate_dates() -----
test_that(".validate_dates() checks for backwards dates properly", {
  x <-  c("2015-01-15", "2015-01-14")
  expect_error(.validate_dates(x, availability = c("1981-01-01", "0")),
               regexp = "Please check your dates.*")
})

test_that(".validate_dates() allows for one day to be fetched", {
  x <-  c("2015-01-15", "2015-01-15")
  expect_error(.validate_dates(x, availability = c("1981-01-01", "0")), NA)
})
