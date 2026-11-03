<!-- badges: start -->
[![peer-review](https://badges.ropensci.org/357_status.svg)](https://github.com/ropensci/software-review/issues/357)
[![status](https://joss.theoj.org/papers/3367fdbff2db55a60c1ab7d611017940/status.svg)](https://joss.theoj.org/papers/3367fdbff2db55a60c1ab7d611017940)
[![CRAN status](https://www.r-pkg.org/badges/version/chirps)](https://cran.r-project.org/package=chirps)
[![Project Status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![DOI](https://zenodo.org/badge/225693680.svg)](https://zenodo.org/badge/latestdoi/225693680)
[![R-CMD-check](https://github.com/ropensci/chirps/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ropensci/chirps/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/ropensci/chirps/graph/badge.svg)](https://app.codecov.io/gh/ropensci/chirps)
<!-- badges: end -->

# chirps: Access Climate Hazards Center Datasets <img align="right" src="man/figures/logo.png">

## Overview

**chirps** provides access to Climate Hazards Center (CHC) datasets, CHIRPS v3 and v2 for rainfall and CHIRTS-daily and CHIRTS-ERA5 for temperature. Data are read directly from the CHC server, either as native rasters for custom spatial workflows or as time series for points and polygons.

For climate indices computed from these data (dry spells, heavy rain days, growing degree days, crop sensitive indices), use the [climatrends](https://CRAN.R-project.org/package=climatrends) package, which takes the output of **chirps** directly.

## Available Datasets

| Dataset | Variable | Resolution | Coverage | Period |
|----------|----------|----------|----------|----------|
| CHIRPS v3 (default) | Precipitation | 0.05° | 60°S to 60°N | 1981 to near present |
| CHIRPS v2 | Precipitation | 0.05° / 0.25° | 50°S to 50°N | 1981 to December 2026 (retired) |
| CHIRTS-daily | Tmax, Tmin, RHum, HeatIndex | 0.05° | 60°S to 70°N | 1983 to 2016 |
| CHIRTS-ERA5 | Tmax, Tmin | 0.05° | 60°S to 70°N | 1980 to near present |

ESI and IMERG are also available through the ClimateSERV API with `get_esi()` and `get_imerg()`.

## Applications

These datasets are commonly used for:

- Crop modeling
- Climate risk assessment
- Agricultural monitoring
- Seasonal analysis
- Environmental characterization
- Ecological and biodiversity studies

## Quick start

### From CRAN

The stable version is available through CRAN.

```r
install.packages("chirps")
```

### From GitHub

A development version that may have new features or bug fixes is available through GitHub.

``` r
library("remotes")

install_github("ropensci/chirps", build_vignettes = TRUE)
```

## Time series for points and polygons

`get_chirps()` reads the CHIRPS v3 files from the CHC server and returns one row per location and day. Fetch rainfall for three points across the Tapajós National Forest (Brazil) in January 2017.

```r
library("chirps")

lonlat = data.frame(lon = c(-55.0281, -54.9857, -55.0714),
                    lat = c(-2.8094, -2.8756, -3.5279))

dates = c("2017-01-01", "2017-01-31")

rain = get_chirps(lonlat, dates)
```

Each call states what it downloads, for example daily rainfall from CHIRPS v3.0 or daily maximum temperature from CHIRTS, so a mix-up between `get_chirps()` and `get_chirts()` is easy to spot. Set `options(chirps.quiet = TRUE)` to hide these messages.

A single date returns one day. With `sf` polygons, each day is averaged over the cells of each polygon, and `operation` sets another summary (4 for the sum).

```r
data("tapajos", package = "chirps")

rain_area = get_chirps(tapajos, dates, operation = 4)
```

Temperature works the same way with `get_chirts()`. CHIRTS-daily ends in 2016, so use `product = "CHIRTS-ERA5"` for recent years.

```r
tmax = get_chirts(lonlat, c("2024-01-01", "2024-01-31"), var = "Tmax",
                  product = "CHIRTS-ERA5")
```

## Climate indices with climatrends

With `as.matrix = TRUE`, **chirps** returns one row per location and one column per day, named by date. This is the input of `climatrends::rainfall()` and `climatrends::temperature()`.

```r
library("climatrends")

rain = get_chirps(lonlat, dates, as.matrix = TRUE)

rainfall(rain, day.one = dates[1], span = ncol(rain))
```

`precip_indices()` remains in **chirps** as a simple example. Use climatrends for analysis.

## Raw raster products

The `*_raw()` functions return the CHC files as `terra::SpatRaster` objects, with one layer per day. This suits large areas and custom extraction workflows.

```r
library("terra")

dates = c("2017-11-15", "2017-11-20")

# CHIRPS v3 precipitation, "rnl" (default) or "sat"
chirps_v3 = get_chirps_raw(dates = dates, type = "sat")

# CHIRTS-ERA5 minimum and maximum temperature
tmin = get_chirts_raw(dates = dates, var = "Tmin", product = "CHIRTS-ERA5")
tmax = get_chirts_raw(dates = dates, var = "Tmax", product = "CHIRTS-ERA5")

pts = vect(lonlat, geom = c("lon", "lat"), crs = "EPSG:4326")

rain = extract(chirps_v3, pts)
```

## Servers

The CHC server is the default and the recommended source. `server = "ClimateSERV"` is kept for backward compatibility. It serves CHIRPS v2 only, which CHC retires after the December 2026 release, and it is slow for more than a few points. ClimateSERV support for CHIRPS may be removed in a future release.

CHC blocks addresses that request many files that do not exist. **chirps** checks the first and last file of each request before reading the rest and identifies itself with a user agent. Keep requests within the period each product covers.

## Going further

The full functionality of **chirps** is illustrated in the package vignette. The vignette can be found on the [package website](https://docs.ropensci.org/chirps/) or from within `R` once the package has been installed, e.g. via: 

``` r
vignette("Overview", package = "chirps")
```

## Citing Data

While *chirps* does not redistribute data, users should cite the original data providers when using CHIRPS, CHIRTS, CHIRTS-ERA5, or CHIRPS v3 products.

When using datasets obtained through this package, please cite the original data providers.

### CHIRPS v3
> Funk, C., Peterson, P., Harrison, L. et al. (2026). The Climate Hazards Center Infrared Precipitation with Stations, Version 3. Scientific Data, 13, 718. <https://doi.org/10.1038/s41597-026-07096-4>

### CHIRTS-ERA5
> CHIRTS-ERA5 Data Repository https://doi.org/10.15780/G2F08J (2025). Data was accessed on [DATE].

### CHIRPS v2
> Funk C., Peterson P., Landsfeld M., … Michaelsen J. (2015). The climate hazards infrared precipitation with stations—a new environmental record for monitoring extremes. *Scientific Data*, 2, 150066. <https://doi.org/10.1038/sdata.2015.66>

### CHIRTS
> Verdin, A., Funk, C., Peterson, P., Landsfeld, M., Tuholske, C., and Grace, K. (2020). Development and validation of the CHIRTS-daily quasi-global high-resolution daily temperature data set. Scientific Data, 7, 303.
<https://doi.org/10.1038/s41597-020-00643-7>

## Meta

  - Please [report any issues or bugs](https://github.com/ropensci/chirps/issues).

  - License: MIT.

  - Get citation information for *chirps* in R by typing `citation(package = "chirps")`.

  - You are welcome to contribute to the *chirps* project. Please read our [contribution guidelines](CONTRIBUTING.md).

  - Please note that the *chirps* project is released with a a [Contributor Code of Conduct](https://ropensci.org/code-of-conduct/). By contributing to this project, you agree to abide by its terms.
