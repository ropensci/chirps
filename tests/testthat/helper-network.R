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

# Run code that reads from CHC, and skip when the server rate-limits CI
# (HTTP 429), which says nothing about the package
with_chc = function(code) {
  tryCatch(code, error = function(e) {
    if (grepl("429|too many requests", conditionMessage(e))) {
      skip("CHC server busy (HTTP 429)")
    }
    stop(e)
  })
}
