# One known file from each CHC path. These fail when CHC moves its data.
skip_on_cran()
skip_if_offline("data.chc.ucsb.edu")

d = as.Date("2000-01-01")

urls = c(
  chirps_v2_p05 = chirps:::.chirps_raw_urls(d, version = "2.0",
                                            resolution = 0.05),
  chirps_v2_p25 = chirps:::.chirps_raw_urls(d, version = "2.0",
                                            resolution = 0.25),
  chirps_v3_rnl = chirps:::.chirps_raw_urls(d, version = "3.0", type = "rnl"),
  chirps_v3_sat = chirps:::.chirps_raw_urls(d, version = "3.0", type = "sat"),
  chirts_tmax = chirps:::.chirts_raw_urls(d, var = "Tmax"),
  chirts_era5_tmax = chirps:::.chirts_era5_raw_urls(d, var = "Tmax")
)

for (i in seq_along(urls)) {
  test_that(paste("CHC path exists:", names(urls)[i]), {
    r = httr::HEAD(urls[[i]], httr::user_agent(chirps:::.chirps_ua()),
                   httr::timeout(60))
    expect_equal(httr::status_code(r), 200, label = urls[[i]])
  })
}

test_that(".check_remote stops at a missing file", {
  bad = sub("2000", "1900", urls[["chirts_tmax"]])
  expect_error(chirps:::.check_remote(c(bad, bad)), "not found|refused|HTTP")
})
