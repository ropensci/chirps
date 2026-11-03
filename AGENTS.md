# Instructions for AI agents

chirps is an R package that gives access to datasets from the Climate Hazards Center (CHC), mainly daily rainfall (CHIRPS v2 and v3) and daily temperature (CHIRTS-daily and CHIRTS-ERA5), plus ESI and IMERG through ClimateSERV. People share this repository with an assistant either to **get climate data** with chirps or to **change the package**. Find out which before you start.

## Before you write any code

1. Check the installed version with `packageVersion("chirps")` and read the help of every function you use (`?get_chirps`). Arguments change between versions, so do not rely on memory.
2. Look for an existing function before writing your own. The `*_raw()` functions already return the CHC files as a `terra` SpatRaster.
3. For indices derived from these data (rainfall indices, growing degree days, dry spells, crop-sensitive temperature), use the climatrends package rather than writing new code here. `as.matrix = TRUE` returns its input, one column per day named "YYYY-MM-DD".
4. Use the CHC server (the default). ClimateSERV serves CHIRPS v2 only, is slow, and may lose CHIRPS support in a future release.

## Which function for which task

| Task | Function | Notes |
|---|---|---|
| Daily rainfall at points or polygons, as a data frame | `get_chirps(object, dates)` | CHC is the default. Avoid `server = "ClimateSERV"`. |
| Daily rainfall as a raster for an area | `get_chirps_raw()`, or `get_chirps()` with a SpatVector, SpatRaster or SpatExtent | `version = "3.0"` with `type = "rnl"` or `"sat"` for CHIRPS v3. |
| Daily maximum or minimum temperature, 1983 to 2016 | `get_chirts()`, `get_chirts_raw()` | Also relative humidity and heat index. |
| Daily temperature from 1980 to near present | `get_chirts(..., product = "CHIRTS-ERA5")`, `get_chirts_raw(..., product = "CHIRTS-ERA5")` | Experimental CHC product, Tmax and Tmin. |
| Evaporative stress index | `get_esi()` | ClimateSERV only, to be deprecated. |
| IMERG rainfall | `get_imerg()` | ClimateSERV only, to be deprecated. Prefer CHIRPS with `get_chirps()`. |
| Rainfall and temperature indices | `climatrends::rainfall()`, `climatrends::temperature()`, `climatrends::GDD()` | Pass `get_chirps(..., as.matrix = TRUE)`. `precip_indices()` is only a simple example. |

## Pitfalls

- **Coverage.** CHIRPS v3 covers 60°S to 60°N and CHIRPS v2 50°S to 50°N. CHIRTS covers 60°S to 70°N, and CHIRTS-daily ends in 2016.
- **Missing files.** CHC blocks addresses that request many files that do not exist. Keep dates within each product's period and never loop over single days when one call covers the period.
- **Latency.** Final CHIRPS data arrive weeks after the end of a month. Requests for recent dates fail.
- **Row order.** Data frames are in long format, ordered by `id` (the row of the input) and then by `date`.
- **Polygons.** With `sf`, `sfc`, `SpatVector` or geojson polygons, each day is summarised over the cells that touch each polygon (`operation`, average by default). `lon` and `lat` are the centroids.
- **Index definitions.** `precip_indices()` follows ETCCDI names but not all of its definitions. Prefer climatrends.

## Changing the package

- Use `=` for assignment, not `<-`. Use base R where possible.
- Document with roxygen and run `devtools::document()`.
- Tests that read from CHC must start with `skip_if_no_chc()` (in `tests/testthat/helper-network.R`), which runs them on one CI job only. Other tests that download data start with `skip_on_cran()` and `skip_if_offline()`. Test reshaping and URLs offline by mocking the `*_raw()` functions, as in `tests/testthat/test-reshape.R`.
- Every fix needs a test that fails before the fix and passes after it. Run `devtools::test()` and `devtools::check()`, and report what was actually run. Never claim a change works without running it.
- Changes that alter returned values always go in `NEWS.md`.
- ClimMob-analysis (https://github.com/AgrDataSci/ClimMob-analysis) and climatrends use chirps. Check them before changing outputs.
- Work on a branch and open a pull request. Do not merge without review and passing checks.

## Where to find more

- CHC data: https://www.chc.ucsb.edu/data
- Issues: https://github.com/AgrDataSci/chirps/issues
