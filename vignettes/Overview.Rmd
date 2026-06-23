---
title: "Introduction to chirps"
package: chirps
author:
- name: Kauê de Sousa
  affiliation: Department of Agricultural Sciences, University of Inland Norway, Hamar, Norway </br> Digital Inclusion, Bioversity International, Montpellier, France
- name: Adam H. Sparks 
  affiliation: Centre for Crop Health, University of Southern Queensland, Toowoomba, Australia
- name: Aniruddha Ghosh
  affiliation: Climate Action, International Center for Tropical Agriculture (CIAT), Nairobi, Kenya
output: html_document
vignette: >
  %\VignetteEngine{knitr::knitr}
  %\VignetteIndexEntry{Introduction to chirps}
  %\usepackage[UTF-8]{inputenc}
  %\VignetteEncoding{UTF-8}
bibliography: ["chirps.bib"]
csl: citation_style.csl
---

# Overview

The **chirps** package provides access to climate datasets produced by the Climate Hazards Center (CHC), University of California Santa Barbara. The package supports both high-level extraction of climate time series for points and polygons, as well as direct access to the original raster products through the **terra** package.

Supported datasets include:

| Dataset      | Variables                   | Period       |
| ------------ | --------------------------- | ------------ |
| CHIRPS v2    | Precipitation               | 1981–present |
| CHIRPS v3    | Precipitation               | 1981–present |
| CHIRTS-daily | Tmax, Tmin, RHum, HeatIndex | 1983–2016    |
| CHIRTS-ERA5  | Tmax, Tmin                  | 1980–present |

## Accessing raw raster products

The recommended workflow is to retrieve climate products as native `SpatRaster` objects and use functions from **terra** for subsequent extraction and spatial analysis.

### CHIRPS v2

```r
library(chirps)
library(terra)

dates = c("2017-11-15", "2017-11-20")

v2 = get_chirps_raw(dates = dates, version = "2.0")

v2
```

### CHIRPS v3

```r
dates = c("2017-11-15", "2017-11-20")

v3 = get_chirps_raw(dates = dates, version = "3.0", type = "sat")

v3
```

Available daily CHIRPS v3 products include:

* `"sat"`: daily totals derived using NASA IMERG
* `"rnl"`: daily totals derived using ERA5 reanalysis

### CHIRTS-ERA5

```r
dates = c("2017-11-15", "2017-11-20")

tmax = get_chirts_era5_raw(dates = dates, var = "Tmax")

tmin = get_chirts_era5_raw(dates = dates, var = "Tmin")
```

CHIRTS-ERA5 extends the CHIRTS temperature record from 1980 to near-present by combining CHIRTS with ERA5 reanalysis data.

### CHIRTS-daily

```r
dates = c("2010-01-01", "2010-01-10")

tmax = get_chirts_raw(dates = dates, var = "Tmax")

tmin = get_chirts_raw(dates = dates, var = "Tmin")
```

Available variables include:

* `"Tmax"`
* `"Tmin"`
* `"RHum"`
* `"HeatIndex"`


## Extracting values using terra

Once data are loaded as rasters, values can be extracted for locations of interest.

```r
lonlat = data.frame(lon = c(-55.0281, -54.9857, -55.0714),
                    lat = c(-2.8094, -2.8756, -3.5279))

pts = vect(lonlat,
           geom = c("lon", "lat"),
           crs = "EPSG:4326")

rain = extract(v3, pts)

head(rain)
```

The resulting object contains one row per location and one column per raster layer.

## Accessing climate time series

For users interested primarily in time series extraction, the package provides high-level functions that handle raster retrieval and extraction internally.

### Point locations

```r
lonlat = data.frame(lon = c(-55.0281, -54.9857, -55.0714),
                    lat = c(-2.8094, -2.8756, -3.5279))

dates = c("2017-01-01", "2017-01-31")

dat = get_chirps(lonlat,
                 dates,
                 server = "CHC")

head(dat)
```

### ClimateSERV

For small requests involving a limited number of locations, the ClimateSERV API can be used.

```r
dat = get_chirps(lonlat,
                 dates,
                 server = "ClimateSERV")
```

## Working with terra

Because the package returns native `SpatRaster` objects, users can directly leverage the `terra` ecosystem.

```r
# crop to area of interest
r_crop = crop(r, ext(-60, -50, -5, 5))

# calculate mean precipitation
mean_rain = app(r, mean)

# write to disk
writeRaster(r,
            "chirps.tif",
            overwrite = TRUE)
```

## Datasets citation

When using data obtained through this package, please cite the original datasets.

### CHIRPS v3
> Funk, C., Peterson, P., Harrison, L. et al. (2026). The Climate Hazards Center Infrared Precipitation with Stations, Version 3. Scientific Data, 13, 718. <https://doi.org/10.1038/s41597-026-07096-4>

### CHIRTS-ERA5
> CHIRTS-ERA5 Data Repository https://doi.org/10.15780/G2F08J (2025). Data was accessed on [DATE].

### CHIRPS v2
> Funk C., Peterson P., Landsfeld M., … Michaelsen J. (2015). The climate hazards infrared precipitation with stations—a new environmental record for monitoring extremes. *Scientific Data*, 2, 150066. <https://doi.org/10.1038/sdata.2015.66>

### CHIRTS
> Verdin, A., Funk, C., Peterson, P., Landsfeld, M., Tuholske, C., and Grace, K. (2020). Development and validation of the CHIRTS-daily quasi-global high-resolution daily temperature data set. Scientific Data, 7, 303.
<https://doi.org/10.1038/s41597-020-00643-7>

## Additional resources

* CHIRPS v3: https://www.chc.ucsb.edu/data/chirps3
* CHIRTS-daily: https://www.chc.ucsb.edu/data/chirtsdaily
* CHIRTS-ERA5: https://www.chc.ucsb.edu/data/chirts-era5
* Climate Hazards Center: https://www.chc.ucsb.edu
