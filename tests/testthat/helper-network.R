# Tests that read files from the CHC server. CHC rate-limits addresses that
# send many requests, so CI runs them on one job only, through the
# CHIRPS_CHC_TESTS environment variable. Locally they run unless offline.
skip_if_no_chc = function() {
  skip_on_cran()
  skip_if_offline("data.chc.ucsb.edu")
  if (identical(tolower(Sys.getenv("CHIRPS_CHC_TESTS")), "false")) {
    skip("CHC tests run on one CI job only")
  }
}
