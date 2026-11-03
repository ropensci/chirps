chirps 2.0.0 (2026-11-03)
=========================

chirps 2.0.0 reads the Climate Hazards Center (CHC) data directly from the CHC server, adds CHIRPS v3 and CHIRTS-ERA5, and returns rasters or time series ready for climate indices with the climatrends package. It also corrects errors found in a review of the package, so some returned values change. Read the BUG FIXES and BREAKING CHANGES sections before you update code written for chirps 1.1.

### NEW DATASETS AND FUNCTIONS

* CHIRPS v3 is the default for `get_chirps()` and `get_chirps_raw()` (`version = "3.0"`), with the daily products `type = "rnl"` (ERA5-based, default) and `type = "sat"` (IMERG-based). It covers 60°S to 60°N. CHIRPS v2 remains available with `version = "2.0"` until CHC retires it after the December 2026 release, and a notice says so once per session.

* `get_chirps_raw()` and `get_chirts_raw()` return the CHC files as `terra::SpatRaster` objects with one layer per day and time metadata, for custom workflows with `crop()`, `extract()` and `mask()`.

* `get_chirts()` and `get_chirts_raw()` read CHIRTS-daily (default, 1983 to 2016, Tmax, Tmin, RHum and HeatIndex) or CHIRTS-ERA5 with `product = "CHIRTS-ERA5"` (1980 to near present, Tmax and Tmin). One function covers each variable, `get_chirps_raw()` for rainfall and `get_chirts_raw()` for temperature.

* `get_chirts_era5_raw()`, available on GitHub since June 2026, is deprecated in favour of `get_chirts_raw(product = "CHIRTS-ERA5")`. It still works and shows a notice once per session.

* Each request states what it downloads, for example "Downloading daily rainfall (mm) from CHIRPS v3.0 (rnl) on the CHC server, 2017-12-15 to 2017-12-31 (17 days)." This makes it clear whether `get_chirps()` (rainfall) or `get_chirts()` (temperature) was called. Set `options(chirps.quiet = TRUE)` to hide these messages.

### CHC FIRST, CLIMATRENDS FOR INDICES

* The CHC server is the default and recommended source in the documentation, README, vignette and examples. `server = "ClimateSERV"` serves CHIRPS v2 only and is kept for backward compatibility. It shows a notice once per session, and ClimateSERV support for CHIRPS may be removed in a future release. `get_esi()` and `get_imerg()` still use ClimateSERV.

* Matrices returned with `as.matrix = TRUE` have one column per day named "YYYY-MM-DD", for every input and server. This is the input of `climatrends::rainfall()`, `climatrends::temperature()` and `climatrends::GDD()`, and the documentation and vignette show how to pass the data.

* `precip_indices()` is documented as a simple example and shows a notice once per session that points to `climatrends::rainfall()`.

### CHC DATA PATHS

The Climate Hazards Center moved its data to new paths in late 2026, so downloads from the CHC server failed. Thanks to Will Turner (CHC) for reporting the new paths.

* CHIRPS v2 files are now read from `products/CHIRPS/v2.0`, and CHIRTS-daily files from `products/CHIRTS/daily/v1.0`. Documentation links point to the new README files.

* Requests check the first and last file before reading the rest, and stop with a clear error when a file is missing. CHC blocks addresses that request many missing files, which is the most likely cause of the 403 errors in issue #56.

* When the CHC server is busy (HTTP 429 or 5xx), the file check retries with growing pauses before it stops with a clear error.

* Tests that read from CHC run on one CI job only, to keep the load on the CHC server low.

* Requests to CHC and ClimateSERV send a user agent that names the package and a contact address.

* A network test requests one known file from each CHC path, so a future move is caught by the checks.

### MORE INPUT TYPES

* `dates` accepts a single date, for one day of data, in every function (#32, #36), and a Date vector as well as a character vector. A clear error says when `dates` is not a vector of dates, for example when an object is passed as the first argument of `get_chirps_raw()`, which takes no object (#58).

* With `sf`, `sfc`, `SpatVector` or geojson polygons, each day is summarised over the cells that touch each polygon (#37). The average is the default, and `operation` takes the ClimateSERV codes (0 max, 1 min, 2 median, 4 sum, 5 average). `lon` and `lat` give the centroid of each polygon. Projected input is transformed to WGS84, and geometries without a CRS are read as longitude and latitude.

* `get_chirps()` accepts `sfc` objects such as the `tapajos` data, and `get_chirts()` accepts `sf` and `sfc` points and polygons.

* `as.raster = TRUE` with a data frame of points returns the raster cropped to the points. It failed before.

### ISSUES ADDRESSED IN THIS RELEASE

* GDAL now reads the CHC files without listing the remote folder or looking for side files, and retries failed reads. This cuts the number of requests per file and avoids requests for files that do not exist (#51, #56).

* A clearer error when ClimateSERV returns no values, which points to `server = "CHC"` (#48). The help of `get_chirps()` explains why ClimateSERV and CHC values differ (#49).

### BREAKING CHANGES

* The default CHIRPS version is v3. Use `version = "2.0"` for the values of chirps 1.1.

* `as.matrix = TRUE` columns are named "YYYY-MM-DD". They were layer names or "chirps-v2.0.YYYY.MM.DD" before.

* `SpatVector` polygons with `as.raster = FALSE` return one row per polygon and day. They returned one row per cell before.

* The default `server` is `"CHC"`, as documented.

### BUG FIXES

* `get_chirps()` and `get_chirts()` paired values with the wrong dates when more than one point was requested with an `sf`, geojson or `SpatVector` input (and, for `get_chirts()`, also with a data frame). Values are now matched to the right point and date.

* `get_chirts()` returned CHIRPS rainfall instead of temperature for `SpatRaster` and `SpatExtent` inputs. Both now return CHIRTS data.

* `get_chirps()` ignored `as.matrix` for `SpatRaster` inputs, and had two conflicting `SpatExtent` methods. Both raster inputs now crop to their extent.

* `get_chirps(..., version = "3.0")` failed with "formal argument matched by multiple actual arguments". `version`, `type` and `resolution` now reach `get_chirps_raw()`.

* `get_chirps()` with `sf` polygons used only the first polygon. Each polygon now gets its own values.

* `get_chirps()` with `server = "ClimateSERV"` failed to match coordinates when the input had row names other than 1 to n. For both servers, the `id` column is now the position of the row in the input, not its row name.

* `get_chirps()` with `sf` input now rejects MULTIPOINT geometries, which were silently misaligned, and accepts MULTIPOLYGON.

* In `precip_indices()`, SDII counted rain on days under 1 mm in the total, and counted missing days as wet days. Rtotal summed rain on all days instead of wet days, as documented. Both now use wet days only (rain >= 1 mm).

* `precip_indices()` failed when `intervals` was shorter than 4 days, and now gives a clear error when `intervals` is missing or longer than the series.

* Clearer errors for coordinates outside the area a product covers, and for CHIRTS-daily dates outside 1983 to 2016, which point to CHIRTS-ERA5.

* Polygons without data return NA for every operation.

### OTHER CHANGES

* `as.matrix = TRUE` with the CHC server returns a matrix, as documented, instead of a data frame.

* ClimateSERV requests wait one second between progress checks instead of polling continuously.

* Tests now run in `R CMD check`. Tests that download data are skipped on CRAN and offline, and new offline tests check how values are paired with points and dates.

* The package description, README and vignette cover the new datasets, with citation guidance for CHIRPS v3, CHIRTS-daily and CHIRTS-ERA5.

* AGENTS.md gives AI agents the main tasks, the matching functions and the pitfalls of the package.


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
