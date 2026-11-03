chirps (development version)
=========================

This release corrects errors found in a review of the package. Some returned values change.

### CHC DATA PATHS

The Climate Hazards Center moved its data to new paths in late 2026, so downloads from the CHC server failed. Thanks to Will Turner (CHC) for reporting the new paths.

* CHIRPS v2 files are now read from `products/CHIRPS/v2.0`, and CHIRTS-daily files from `products/CHIRTS/daily/v1.0`. Documentation links point to the new README files.

* `get_chirps_raw()`, and `get_chirps()` with `server = "CHC"`, now default to CHIRPS v3 (`version = "3.0"`, `type = "rnl"`). CHC retires CHIRPS v2 after the December 2026 release. Use `version = "2.0"` to keep the earlier results until then.

* Requests check the first and last file before reading the rest, and stop with a clear error when a file is missing. CHC blocks addresses that request many missing files, which is the most likely cause of the 403 errors in issue #56.

* Requests to CHC and ClimateSERV send a user agent that names the package and a contact address.

* A network test requests one known file from each CHC path, so a future move is caught by the checks.

### BUG FIXES

* `get_chirps()` and `get_chirts()` paired values with the wrong dates when more than one point was requested with an `sf`, geojson or `SpatVector` input (and, for `get_chirts()`, also with a data frame). Values are now matched to the right point and date.

* `get_chirts()` returned CHIRPS rainfall instead of temperature for `SpatRaster` and `SpatExtent` inputs. Both now return CHIRTS data.

* `get_chirps()` ignored `as.matrix` for `SpatRaster` inputs, and had two conflicting `SpatExtent` methods. Both raster inputs now crop to their extent.

* `get_chirps(..., version = "3.0")` failed with "formal argument matched by multiple actual arguments". `version`, `type` and `resolution` now reach `get_chirps_raw()`.

* `get_chirps()` with `sf` polygons used only the first polygon. Each polygon is now represented by its own centroid.

* `get_chirps()` with `server = "ClimateSERV"` failed to match coordinates when the input had row names other than 1 to n. For both servers, the `id` column is now the position of the row in the input, not its row name.

* `get_chirps()` with `sf` input now rejects MULTIPOINT geometries, which were silently misaligned, and accepts MULTIPOLYGON.

* In `precip_indices()`, SDII counted rain on days under 1 mm in the total, and counted missing days as wet days. Rtotal summed rain on all days instead of wet days, as documented. Both now use wet days only (rain >= 1 mm).

* `precip_indices()` failed when `intervals` was shorter than 4 days, and now gives a clear error when `intervals` is missing or longer than the series.

### CHANGES

* `get_chirts_raw()` and `get_chirts_era5_raw()` failed to open the remote files, because `use_vsicurl` defaulted to `FALSE`. It now defaults to `TRUE`, as in earlier versions, and `get_chirps_raw()` streams its files the same way.

* Fixed the vignette engine and an example that did not parse, both of which stopped `R CMD check`. The `tapajos` data are now lazy-loaded.

* The default `server` is now `"CHC"`, as documented.

* `as.matrix = TRUE` with the CHC server returns a matrix, as documented, instead of a data frame.

* ClimateSERV requests wait one second between progress checks instead of polling continuously.

* Tests now run in `R CMD check`. Tests that download data are skipped on CRAN and offline, and new offline tests check how values are paired with points and dates.

chirps 2.0.0 (2026-06-23)
=========================

### NEW FEATURES

* Added `get_chirps_raw()` to provide direct access to CHIRPS raster products as `terra::SpatRaster` objects without extracting values or reshaping outputs.

* Added support for CHIRPS v3, including both daily `rnl` (ERA5-based) and `sat` (IMERG-based) products.

* Added `get_chirts_raw()` to access CHIRTS-daily maximum temperature, minimum temperature, relative humidity, and heat index datasets as raster objects.

* Added `get_chirts_era5_raw()` to access CHIRTS-ERA5 daily temperature datasets extending from 1980 to near-present.

* Added support for retrieving raw climate datasets directly from Climate Hazards Center (CHC) archives through remote Cloud Optimized GeoTIFF (COG) and GeoTIFF access.

### ENHANCEMENTS

* Added raster time metadata to raw raster outputs using `terra::time()`.

* Improved internal URL handling for CHIRPS and CHIRTS products through version-specific data source management.

* Expanded package documentation with dataset descriptions, references, and citation guidance for CHIRPS v3, CHIRTS-daily, and CHIRTS-ERA5 products.

### INTERNAL CHANGES

* Refactored internal raster retrieval functions to support multiple dataset versions and products through a unified architecture.

* Improved compatibility with modern `terra` workflows by exposing native raster products for downstream processing with `crop()`, `extract()`, `mask()`, and related functions.


chirps 1.1 (2025-11-24) 
=========================

* Housekeeping version to refresh libraries and formats as part of CRAN guidelines updates.

chirps 0.1.4.001 (2022-01-15) 
=========================

### BUG FIXES

* Fixes bug where the first and last dates requested are not available as per <https://github.com/ropensci/chirps/issues/46> and <https://github.com/ropensci/chirps/issues/50> 

* Fixes mismatched brackets in documentation

* Fixes bug where geojson objects were not allowed, <https://github.com/ropensci/chirps/commit/1cfb099d94e1b61aed5a3d372ea49b745f4bcc92>

chirps 0.1.4.001 (2022-01-15) 
=========================

### ENHANCEMENTS  

* Methods for objects of class "SpatExtent" in `get_chirps()` and `get_chirts()` to return a raster within a given area 


chirps 0.1.4 (2022-01-13) 
=========================

### ENHANCEMENTS 

* Add new function `get_chirts()` to fetch temperature data from CHC server (https://data.chc.ucsb.edu/products/CHIRTSdaily/v1.0/global_cogs_p05/)
* Implement data fetching from CHC server in `get_chirps()` which offers a better alternative for requests with multiple data points using GoC files from CHC server (https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_daily/cogs/) and the `terra` package  
* New S3 methods in `get_chirps()` for objects of class 'SpatVector' and 'SpatRaster' from the `terra` package
* Data can be returned as an object of class 'matrix' when using the argument `as.matrix = TRUE` in the S3 methods for objects of class 'default', 'SpatVector' and 'SpatRaster'
* Updates the URL to request data from ClimateSERV
 

### CHANGES IN BEHAVIOUR

* New argument `server = ` is added to indicate from which server the function should send the request, either 'CHC' or 'ClimateSERV'. Please use the argument `server = "ClimateSERV"` for backward compatibility with previous versions of the package. 
* API requests to ClimateSERV use package httr instead of curl
* Argument `operation = ` in `get_chirps()` is only required when `server = "ClimateSERV"`
* Updates function `as.geojson()` to matches with the new requirements for ClimateSERV

chirps 0.1.3 (2021-07-10)
=========================

* GitHub version with ongoing updates and changes in behaviour. 


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
* documentation for `tapajos` is given avoiding `chirps:::`


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
