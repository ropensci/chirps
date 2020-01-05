chirps 0.0.4 (2020-01-03)
=========================

### NEW FEATURES

* S3 methods for objects of class "geojson" in `get_chirps()` and `get_esi()`

* Package vignette

* Prepare for submission to ropensci

### ENHANCEMENTS

* Validations in internal functions to transform 'sf' into geojson

* Add properties features to geojson output in `get_chirps()` and `get_esi()` via `.add_geojson_properties()`


chirps 0.0.3 (2019-12-31)
=========================

### NEW FEATURES

* `get_esi` is added to retrieve Evaporative Stress Index with S3 methods for "data.frame" and "sf"

* S3 methods for objects of class "data.frame" and "sf" in `get_chirps`

### CHANGES IN BEHAVIOUR

* `.get_request_progress` and a `while` condition are added to check the progress of large requests and prevent the function to fail.

* `.GET` is added as a general function to retrieve other datasets from ClimateSERV

* improvements in internal functions documentation 


chirps 0.0.2 (2019-12-05)
=========================

### NEW FEATURES

* Calculate precipitation indices with `precip_indices` over a time span


chirps 0.0.1 (2019-12-03)
=========================

* GitHub-only release of prototype package.