# Instructions for AI agents

chirps is an R package that gives access to datasets from the Climate Hazards Center (CHC), mainly daily rainfall (CHIRPS v2 and v3) and daily temperature (CHIRTS-daily and CHIRTS-ERA5), plus ESI and IMERG through ClimateSERV. People share this repository with an assistant either to **get climate data** with chirps or to **change the package**. Find out which before you start.

## Before you write any code

1. Check the installed version with `packageVersion("chirps")` and read the help of every function you use (`?get_chirps`). Arguments change between versions, so do not rely on memory.
2. Look for an existing function before writing your own. The `*_raw()` functions already return the CHC files as a `terra` SpatRaster.
3. For indices derived from these data (growing degree days, dry spells, crop-sensitive temperature), use the climatrends package rather than writing new code here.

## Which function for which task

| Task | Function | Notes |
|---|---|---|
| Daily rainfall at points, as a data frame | `get_chirps(lonlat, dates, server = "CHC")` | `server = "ClimateSERV"` is slower and only suits a few points. |
| Daily rainfall as a raster for an area | `get_chirps_raw()`, or `get_chirps()` with a SpatVector, SpatRaster or SpatExtent | `version = "3.0"` with `type = "rnl"` or `"sat"` for CHIRPS v3. |
| Daily maximum or minimum temperature, 1983 to 2016 | `get_chirts()`, `get_chirts_raw()` | Also relative humidity and heat index. |
| Daily temperature from 1980 to near present | `get_chirts_era5_raw()` | Experimental CHC product. |
| Evaporative stress index | `get_esi()` | ClimateSERV only. |
| IMERG rainfall | `get_imerg()` | ClimateSERV only. |
| Rainfall indices (dry spells, Rx5day, SDII) | `precip_indices()` | Needs the data frame returned by `get_chirps()` or `get_imerg()`. |

## Pitfalls

- **Coverage.** CHIRPS covers 50°S to 50°N. CHIRTS-daily covers 60°S to 70°N and ends in 2016.
- **Latency.** Final CHIRPS data arrive weeks after the end of a month. Requests for recent dates fail.
- **Row order.** Data frames are in long format, ordered by `id` (the row of the input) and then by `date`.
- **Polygons.** For `sf` and geojson input with the CHC server, polygons are represented by their centroids. ClimateSERV summarises over the polygon.
- **Index definitions.** `precip_indices()` follows ETCCDI names but not all of its definitions. Read `?precip_indices` before comparing with other sources.

## Changing the package

- Use `=` for assignment, not `<-`. Use base R where possible.
- Document with roxygen and run `devtools::document()`.
- Tests that download data must start with `skip_on_cran()` and `skip_if_offline()`. Test reshaping and URLs offline by mocking the `*_raw()` functions, as in `tests/testthat/test-reshape.R`.
- Every fix needs a test that fails before the fix and passes after it. Run `devtools::test()` and `devtools::check()`, and report what was actually run. Never claim a change works without running it.
- Changes that alter returned values always go in `NEWS.md`.
- ClimMob-analysis (https://github.com/AgrDataSci/ClimMob-analysis) and climatrends use chirps. Check them before changing outputs.
- Work on a branch and open a pull request. Do not merge without review and passing checks.

## Where to find more

- CHC data: https://www.chc.ucsb.edu/data
- Issues: https://github.com/AgrDataSci/chirps/issues
