# chirps

<!-- badges: start -->
[![peer-review](https://badges.ropensci.org/360_status.svg)](https://github.com/ropensci/software-review/issues/357)
[![CRAN status](https://www.r-pkg.org/badges/version/chirps)](https://cran.r-project.org/package=chirps)
[![Build Status](https://travis-ci.org/agrobioinfoservices/chirps.svg?branch=master)](https://travis-ci.org/agrobioinfoservices/chirps)
[![codecov](https://codecov.io/gh/agrobioinfoservices/chirps/master.svg)](https://codecov.io/github/agrobioinfoservices/chirps?branch=master)
[![Project Status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

# *chirps*: API Client for CHIRPS <img align="right" src="man/figures/logo.png">

## Overview

**chirps** provides the API Client for Climate Hazards Group InfraRed Precipitation with Station Data [(CHIRPS)](https://www.chc.ucsb.edu/data/chirps) via [ClimateSERV](https://climateserv.readthedocs.io/en/latest/index.html). [CHIRPS](https://www.chc.ucsb.edu/data/chirps) is a 35+ year quasi-global rainfall data set. Spanning 50°S-50°N (and all longitudes) and ranging from 1981 to near-present (normally with a 45 day lag), CHIRPS incorporates 0.05° resolution satellite imagery, and in-situ station data to create gridded rainfall time series for trend analysis and seasonal drought monitoring.

## Package website

<https://agrobioinfoservices.github.io/chirps/>

## Installation

The development version can be installed via

``` r
library("remotes")

install_github("agrobioinfoservices/chirps", build_vignettes = TRUE)
```

## Example

Fetch CHIRPS data from three points across the *Tapajós* National Forest (Brazil) from Jan-2017 to Dec-2017. Then calculate the precipitation indices over the timeseries using intervals of 30 days.

``` r
library("chirps")

lonlat <- data.frame(lon = c(-55.0281,-54.9857, -55.0714),
                     lat = c(-2.8094, -2.8756, -3.5279))

dates <- c("2017-01-01", "2017-12-31")

dat <- get_chirps(lonlat, dates)

p_ind <- precip_indices(dat, timeseries = TRUE, intervals = 30)
```

## Going further

The full functionality of **chirps** is illustrated in the package vignette. The vignette can be found on the [package website](https://agrobioinfoservices.github.io/chirps/) or from within `R` once the package has been installed, e.g. via: 

``` r
vignette("Overview", package = "chirps")
```

## Use of CHIRPS data

While *chirps* does not redistribute the data or provide it in any way, we encourage users to cite Funk et al. (2015) when using CHIRPS.

> Funk C., Peterson P., Landsfeld M., Pedreros D., Verdin J., Shukla S., … Michaelsen J. (2015). The climate hazards infrared precipitation with stations—a new environmental record for monitoring extremes. *Scientific Data*, 2, 150066. <https://doi.org/10.1038/sdata.2015.66>.

## Meta

  - Please [report any issues or bugs](https://github.com/agrobioinfoservices/chirps/issues).

  - License: MIT.

  - Get citation information for *chirps* in R by typing `citation(package = "chirps")`.

  - You are welcome to contribute to the *chirps* project. Please read our [contribution guidelines](CONTRIBUTING.md).

  - Please note that the *chirps* project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in the *chirps* project you agree to abide by its terms.
