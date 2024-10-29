
# setup values for testing all
lonlat <-
  structure(list(
    lon = c(-60.03947, -54.7838828),
    lat = c(-3.031387, -2.4221716)
  ),
  class = "data.frame",
  row.names = c(NA, -2L))
dates <- c("2000-01-01", "2000-01-05")
chirts_values <-
  c(
    29.4855480194092,
    29.5689563751221,
    29.5717449188232,
    32.0975608825684,
    29.1689014434814,
    29.5432586669922,
    29.4775066375732,
    28.201681137085,
    29.9562358856201,
    30.5279235839844
  )

# Test get_chirts() default method -----
test_that("get_chirts() returns proper values", {
  x <- get_chirts(object = lonlat,
                  dates = dates,
                  var = "Tmax")
  expect_equal(x$chirts, chirts_values, tolerance =  0.01)
  expect_named(x, c("id", "lon", "lat", "date", "chirts"))
  expect_equal(nrow(x), 10)
  expect_s3_class(x, c("chirts", "chirts_df", "data.frame"))
})
