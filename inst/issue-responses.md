# Replies to open issues in ropensci/chirps

Replies ready to paste into each issue in <https://github.com/ropensci/chirps/issues>. Each reply names the commit in AgrDataSci/chirps that addresses it. The first commits came in with PR AgrDataSci/chirps#3 (merged) and the rest with PR AgrDataSci/chirps#4. All reach CRAN with chirps 2.0.0.

Before pasting, check that PR #4 is merged and replace `remotes::install_github("AgrDataSci/chirps")` if users should install from ropensci/chirps instead.

| Issue | Action | Commits |
|---|---|---|
| #58 Subscript out of bounds with dates in 1982 | Close | 9f5d94b |
| #57 CRAN check error | Close | 0490442 |
| #56 Error 403 connection to CHC server | Close | 5bfcdd1, 9f5d94b, 8985c3d |
| #54 CHIRPS v3 | Close | 5bfcdd1 |
| #51 get_chirps is extremely slow | Close, invite to reopen | 9f5d94b, 8985c3d |
| #49 Check data from ClimateSERV against CHC | Close | 9f5d94b, e739ef2 |
| #48 Try to increase the buffer area with 'dist' | Close | 9f5d94b |
| #47 CHIRPS-GEFS and Prelim | Keep open | none yet |
| #45 Example case No 4, SpatExtent fails | Close | a2ba257 |
| #44 Extracting time series | Close | a2ba257, e739ef2 |
| #43 get_chirps using a SpatRaster | Close | a2ba257 |
| #41 CHC data not available on specific date | Close | 5bfcdd1 |
| #37 Using get_chirps over a SFC | Close | 69ea09b, 9f5d94b, e739ef2 |
| #36 get_chirts() does not allow fetching only one day | Close | 9f5d94b |
| #32 Not possible to get data for only one or two days | Close | 9f5d94b |
| #31 Tests fail | Close | a2ba257, 5bfcdd1 |

---

## #58 Error: Subscript out of bounds with dates in 1982

Thanks @ecor. The dates are fine. `get_chirps_raw()` takes no object, so its first argument is `dates`, and `ee` was read as the dates. chirps 2.0.0 gives a clear error in this case (AgrDataSci/chirps@9f5d94b).

To get the rasters and crop them to your area:

```r
r = get_chirps_raw(dates = c("1982-01-01", "1982-12-31"), version = "3.0")

r = terra::crop(r, ee)
```

To get values for points or polygons directly, use `get_chirps(ee, dates, server = "CHC")`, which now defaults to CHIRPS v3. Closing, please reopen if it still fails.

---

## #57 CRAN check error

Thanks @ecor. A doubled roxygen comment in the example of `get_chirts_raw()` left an unmatched `}`. Fixed in AgrDataSci/chirps@0490442. The tests now also run in `R CMD check`, which passes on Linux, macOS and Windows. Closing.

---

## #56 Error 403 connection to CHC server

The Climate Hazards Center moved its data to new paths, and Will Turner (CHC) kindly sent us the new layout. The 403 errors came from the CHC servers, which block addresses that request many files that do not exist. The old paths made each call request one missing file per day.

chirps 2.0.0 fixes this (AgrDataSci/chirps@5bfcdd1, AgrDataSci/chirps@9f5d94b, AgrDataSci/chirps@8985c3d).

- CHIRPS v2 reads from `products/CHIRPS/v2.0` and CHIRTS from `products/CHIRTS/daily/v1.0`.
- Each request checks the first and last file before reading the rest, and stops with a clear error if one is missing.
- GDAL no longer lists the remote folder or asks for side files, so each file takes fewer requests.
- When the server is busy (HTTP 429), the check waits and retries instead of sending more requests.
- Requests identify the package with a user agent, so CHC can tell it apart from bots.
- A test checks one file from each CHC path, so a future move is caught early.

If your address was blocked, wait a while before trying again. Closing, please reopen if the error comes back with chirps 2.0.0.

---

## #54 CHIRPS v3

CHIRPS v3 came in with @ecor's pull request, thank you. In chirps 2.0.0 it is also the default for `get_chirps_raw()` and `get_chirps(server = "CHC")`, since CHC retires CHIRPS v2 after December 2026 (AgrDataSci/chirps@5bfcdd1).

```r
dates = c("2017-12-15", "2017-12-31")

lonlat = data.frame(lon = c(-55.0281, -54.9857), lat = c(-2.8094, -2.8756))

rain = get_chirps(lonlat, dates, server = "CHC")

rain_sat = get_chirps(lonlat, dates, server = "CHC", type = "sat")
```

Use `version = "2.0"` to keep CHIRPS v2 until it is retired. Closing.

---

## #51 get_chirps is extremely slow

Thanks for the report. With `server = "CHC"` the package reads one file per day, so time grows with the number of days, not with the number of points. chirps 2.0.0 cuts the requests per file (AgrDataSci/chirps@9f5d94b). GDAL no longer lists the remote folder or looks for side files on each open, and it retries failed reads. When the server is busy, the file check waits and retries (AgrDataSci/chirps@8985c3d).

For long periods, request all points in one call, as you did, rather than one call per point. ClimateSERV remains slower for many points because it sends several requests per point.

Closing for now. If it is still slow with chirps 2.0.0, please reopen with your timing, and we will look at caching downloaded files.

---

## #49 Check data from ClimateSERV against CHC and prepare for new version

chirps 2.0.0 is ready (PR AgrDataSci/chirps#3 and AgrDataSci/chirps#4). ClimateSERV serves CHIRPS v2 averaged over a small buffer around each point, while CHC gives the value of the cell that holds the point, so the two can differ. This is now explained in `?get_chirps` (AgrDataSci/chirps@9f5d94b).

CHC is now the default server and uses CHIRPS v3, and the documentation and examples use CHC throughout (AgrDataSci/chirps@e739ef2). Since CHIRPS v2 is retired after December 2026, a full comparison of the v2 routes is no longer worth doing. ClimateSERV stays for ESI and IMERG, and for CHIRPS only for backward compatibility. Closing.

---

## #48 Error: try to increase the buffer area with 'dist'

ClimateSERV returned no values for these points. The CHC server reads the CHIRPS files directly and works for any point between 50°S and 50°N, including India.

```r
location = data.frame(lon = 78.96288, lat = 20.593684)

d = get_chirps(location, dates = c("2019-01-01", "2019-12-31"), server = "CHC")
```

The same works for your `sf` points from `st_sample()`. chirps 2.0.0 also gives a clearer error that points to `server = "CHC"` (AgrDataSci/chirps@9f5d94b). Closing, please reopen if CHC also fails.

---

## #47 Interest in adding CHIRPS-GEFS &/or Prelim?

Thanks @zackarno, and sorry for the long wait. CHC reorganised its data in late 2026, and we are confirming the new paths for CHIRPS-GEFS and the preliminary CHIRPS with the CHC team before adding them. Keeping this open until then. A pull request is welcome once the paths are settled.

---

## #45 Example case No 4: input SpatExtent fails

Fixed in AgrDataSci/chirps@a2ba257. The `SpatExtent` and `SpatRaster` methods now dispatch correctly and return the raster cropped to the area, with no workaround needed.

```r
area = terra::ext(c(-66, -64, -6, -4))

r4 = get_chirps(area, c("2017-12-15", "2017-12-31"))
```

Use `as.raster = FALSE` to get a matrix instead. Closing.

---

## #44 Extracting time series

Fixed in AgrDataSci/chirps@a2ba257. With more than one `sf` point, earlier versions paired values with the wrong dates. The default server is now `"CHC"`, so the call without `server` also works.

```r
data("tapajos", package = "chirps")

spt = sf::st_as_sf(sf::st_sample(tapajos, 30))

dft = get_chirps(spt, dates = c("2017-01-01", "2018-12-31"))

# rainfall indices in periods of 30 days, with climatrends
rain = get_chirps(spt, dates = c("2017-01-01", "2018-12-31"), as.matrix = TRUE)

p_ind = climatrends::rainfall(rain, day.one = "2017-01-01", span = ncol(rain),
                              timeseries = TRUE, intervals = 30)
```

`precip_indices()` had errors in SDII and Rtotal, fixed in the same commit, but it is now documented as a simple example. For analysis we recommend `climatrends::rainfall()`, which takes the matrix from `as.matrix = TRUE` directly (AgrDataSci/chirps@e739ef2). Closing.

---

## #43 get_chirps using a SpatRaster?

Fixed in AgrDataSci/chirps@a2ba257. `get_chirps()` with a `SpatRaster` now returns CHIRPS cropped to the extent of that raster. The same fix makes `get_chirts()` return temperature for rasters and extents, where it returned CHIRPS before.

```r
r = terra::rast(system.file("ex/elev.tif", package = "terra"))

x_raster = get_chirps(r, c("2017-12-15", "2017-12-31"))
```

Closing.

---

## #41 CHC data not available on specific date

The files exist. These errors came from old GDAL versions and, more recently, from CHC moving its data. chirps 2.0.0 uses the new CHC paths and checks the first and last file before reading, so a missing file now gives a clear error that names it (AgrDataSci/chirps@5bfcdd1). Use GDAL 3 or later, which reads these files without problems. Closing, please reopen with `terra::gdal()` output if it happens again.

---

## #37 Using get_chirps over a SFC, possible?

Yes, in chirps 2.0.0. `get_chirps()` accepts `sfc`, `sf` and `SpatVector` polygons. With `server = "CHC"` (the default), each day is summarised over the cells that touch each polygon, one row per polygon and day (AgrDataSci/chirps@69ea09b, AgrDataSci/chirps@9f5d94b, AgrDataSci/chirps@e739ef2). The average is the default, and `operation` takes the same codes as ClimateSERV (0 max, 1 min, 2 median, 4 sum, 5 average).

```r
districts = sf::st_read("ETHIOPIA.shp")

rain = get_chirps(districts, c("2017-12-15", "2017-12-31"))

rain_total = get_chirps(districts, c("2017-12-15", "2017-12-31"),
                        operation = 4)

# an sfc works too
rain_geom = get_chirps(sf::st_geometry(districts), c("2017-12-15", "2017-12-31"))
```

The polygons can be in any CRS, as they are transformed to WGS84. Closing.

---

## #36 get_chirts() does not allow fetching only one day

Fixed in AgrDataSci/chirps@9f5d94b. `dates` now takes a single date for one day of data, in every function.

```r
lonlat = data.frame(lon = c(-55.0281, -54.9857), lat = c(-2.8094, -2.8756))

temp1 = get_chirts(lonlat, "2010-12-15", var = "Tmax")
```

Closing.

---

## #32 Not possible to get data for only one or two days

Two-day requests were fixed in 800ba70. chirps 2.0.0 also accepts a single date, in every function (AgrDataSci/chirps@9f5d94b), with a test. Closing.

---

## #31 Tests fail

The expected values came from ClimateSERV, which changed them. The tests now run in `R CMD check` (they were excluded from the build before), with offline tests that check how values are paired with points and dates (AgrDataSci/chirps@a2ba257). A test also checks one file from each CHC path (AgrDataSci/chirps@5bfcdd1). Tests that download data are skipped on CRAN. Closing.
