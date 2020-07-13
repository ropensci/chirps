chirps 0.1.2 (2020-07-12)
=========================

* Add citation info for JOSS paper
* Fix vignette build
* A S3 method `as.geojson()` is added to replace the functions `data.frame_to_geojson()` and `sf_to_geojson()`

chirps 0.1.0 (2020-07-01)
=========================

* rOpenSci release version

### ENHANCEMENTS 

* Add `get_imerg()` to fetch IMERG data https://disasters.nasa.gov/instruments/imerg


chirps 0.0.8 (2020-05-22)
=========================

### CHANGES IN BEHAVIOUR

* Remove Imports of pkg 'tibble' which was basically to provide a "cool" print method. 
* A new `print()` method is added for objects that inherits the class 'chirps_df'
* Pkg 'methods' was moved from Imports to Depends
* Comments/suggestions given by Jake Zwart in rOpenSci pkg review are added


chirps 0.0.6 (2020-01-29)
=========================

### ENHANCEMENTS 

* Comments/suggestions given by Claudia Vitolo in rOpenSci pkg review are added
* `dataframe_to_geojson()`, `sf_to_geojson()` are added as exported functions avoiding `chirps:::`
* documentation for `tapajos` is given avoiding chirps:::


chirps 0.0.5 (2020-01-09)
=========================

### ENHANCEMENTS

* Fix comments given by good practice `goodpractice::gp()`. Avoid long code lines, it is bad for readability. Avoid 1:length(...), 1:nrow(...), 1:ncol(...), 1:NROW(...) and 1:NCOL(...) expressions. Not import packages as a whole, as this can cause name clashes between the imported packages. 


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