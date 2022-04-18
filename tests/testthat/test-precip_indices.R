
# setup values for testing all
lonlat <-
  structure(list(
    lon = c(-60.03947, -54.7838828),
    lat = c(-3.031387,
            -2.4221716)
  ),
  class = "data.frame",
  row.names = c(NA, -2L))
dates <- c("2000-01-01", "2000-01-05")
chirps_df <-
  structure(
    list(
      id = c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L),
      lon = c(
        -60.03947,
        -60.03947,
        -60.03947,
        -60.03947,
        -60.03947,
        -54.7838828,
        -54.7838828,
        -54.7838828,
        -54.7838828,
        -54.7838828
      ),
      lat = c(
        -3.031387,
        -3.031387,
        -3.031387,
        -3.031387,
        -3.031387,
        -2.4221716,
        -2.4221716,
        -2.4221716,
        -2.4221716,
        -2.4221716
      ),
      date = structure(
        c(
          10957,
          10958,
          10959,
          10960,
          10961,
          10957,
          10958,
          10959,
          10960,
          10961
        ),
        class = "Date"
      ),
      chirps = c(
        0,
        0,
        0,
        0,
        20.1769542694092,
        38.3542251586914,
        0,
        38.3542251586914,
        0,
        0
      )
    ),
    class = c("chirps", "chirps_df", "data.frame"),
    row.names = c(NA,
                  -10L)
  )

# Test the default behaviour
values <-
  c(
    4.00000,
    1.00000,
    0.00000,
    1.00000,
    20.17695,
    20.17695,
    20.17695,
    20.17695,
    20.17695,
    20.17695,
    2.00000,
    1.00000,
    0.00000,
    2.00000,
    38.35423,
    76.70845,
    0.00000,
    0.00000,
    76.70845,
    38.35423
  )

test_that("timespan FALSE", {
  p <- precip_indices(chirps_df, timeseries = FALSE)
  expect_equal(p$value, values, tolerance = 0.01)
})

# The function can handle timeseries intervals
# here it will return just one interval since we have only 5 days
values2 <-
  c(
    4.00000,
    0.00000,
    0.00000,
    0.00000,
    0.00000,
    0.00000,
    0.00000,
    0.00000,
    0.00000,
    0.00000,
    1.00000,
    1.00000,
    0.00000,
    2.00000,
    38.35423,
    76.70845,
    0.00000,
    0.00000,
    76.70845,
    38.35423
  )

test_that("timespan TRUE", {
  p <- precip_indices(chirps_df, timeseries = TRUE, intervals = 4)
  expect_equal(p$value, values2, tolerance = 0.001)
})

# The function can handle NAs
values3 <-
  c(
    2.00000,
    1.00000,
    0.00000,
    1.00000,
    20.17695,
    20.17695,
    20.17695,
    20.17695,
    20.17695,
    10.08848,
    2.00000,
    1.00000,
    0.00000,
    2.00000,
    38.35423,
    76.70845,
    0.00000,
    0.00000,
    76.70845,
    25.56948
  )

test_that("accepts NAs", {
  chirps_df[c(2, 7), "chirps"] <- NA
  p <- precip_indices(chirps_df)
  expect_equal(p$value, values3, tolerance = 0.001)
})

# Get an error with non chirps data
test_that("non chirps data", {
  expect_error(precip_indices(object = airquality))
  
})
