---
title: 'chirps: API Client for the CHIRPS Precipitation Data in R'
tags:
- CHIRPS
- climate data
- climatology
- earth science
- evapotranspiration
- precipitation data
- R
- reproducibility
- weather data
authors:
  - name: Kauê de Sousa
    orcid: 0000-0002-7571-7845
    affiliation: "1, 2"
  - name: Adam H. Sparks
    orcid: 0000-0002-0061-8359
    affiliation: 3
  - name: William Ashmall
    affiliation: 4
  - name: Jacob van Etten
    orcid: 0000-0001-7554-2558
    affiliation: 2
  - name: Svein Ø. Solberg
    orcid: 0000-0002-4491-4483
    affiliation: 1
affiliations:
  - name: Department of Agricultural Sciences, Inland Norway University of Applied Sciences, Hamar, Norway
    index: 1
  - name: The Alliance of Bioversity International and CIAT, Rome, Italy
    index: 2
  - name: Centre for Crop Health, University of Southern Queensland, Toowoomba, Australia
    index: 3
  - name: Universities Space Research Association, National Aeronautics and Space Administration (NASA), Huntsville, USA
    index: 4
citation_author: de Sousa et. al.
date: "03 January 2020"
year: 2020
bibliography: paper.bib
output: rticles::joss_article
journal: JOSS
---

# Summary

The *chirps* package provides functionalities for reproducible analysis in R [@RCoreTeam] using the CHIRPS data [@Funk2015]. Three main functions are provided, `get_chirps()`, `get_esi()` and `precip_indices()`. The `get_chirps()` function provides access to CHIRPS data via the ClimateSERV API Client [@ClimateSERV] with methods to handle objects of class 'data.frame', 'geojson' and 'sf' via the package *methods* [@RCoreTeam]. To accept the query, ClimateSERV requires a geojson object of type 'Polygon' (one single polygon per request). Using the package *sf* [@sf] internally, the input provided in `get_chirps()` is transformed into a list of polygons with a small buffer area (0.0001 arc-sec by default) around the point and transformed into a list of geojson strings. If multiple points are required, `get_chirps()` does this process with a `lapply()` internal process. *chirps* uses *crul* [@crul] to interface with ClimateSERV API. The query returns a json object parsed to *jsonlite* [@jsonlite] to obtain the data frame for the time series required. `get_chirps()` returns a *tibble* data frame [@tibble], which also inherits the class 'chirps', where each id represents the index for the rows in the in-putted 'object'. The function `get_esi()` behaves similarly to `get_chirps()` and returns the Evaporative Stress Index (ESI) data [@Anderson2011], but the output does not inherits the class 'chirps'. Users providing objects of class 'sf' and 'geojson' in `get_chiprs()` and `get_esi()` can also opt to return an object with the same class as the object provided using the arguments 'as.sf = TRUE' or 'as.geojson = TRUE'. With the function `precip_indices()` it is possible to track how the precipitation changes across the requested time series using precipitation variability indices [@Aguilar2005], computed using *stats* [@RCoreTeam]. Extended documentation is provided with examples on how to increase the buffer area and draw quadrants for the geojson polygon using *sf* [@sf].

This process can be integrated into workflows like @vanEtten2019 to track how crop varieties responds to seasonal climate variability, and @DeSousa2018 to assess how extreme precipitation events are changing in a regional time series analysis. 

# About CHIRPS and ESI data

CHIRPS is daily precipitation data set developed by the Climate Hazards Group [@Funk2015] for high resolution precipitation gridded data. Spanning 50$^{\circ}$ S to 50$^{\circ}$ N (and all longitudes) and ranging from 1981 to near-present, CHIRPS incorporates 0.05 arc-degree resolution satellite imagery, and in-situ station data to create gridded precipitation time series for trend analysis and seasonal drought monitoring [@Funk2015]. The Evaporative Stress Index (ESI) data describes temporal anomalies in evapotranspiration produced weekly at 0.25 arc-degree resolution for the entire globe [@Anderson2011]. The ESI data is based on satellite observations of land surface temperature, which are used to estimate water loss due to evapotranspiration (the sum of evaporation and plant transpiration from the Earth's land and ocean surface to the atmosphere). When using these data sets in publications please cite @Funk2015 for CHIRPS and @esi for ESI.


# A case study in the Tapajós National Forest

The *Tapajós* National Forest, Brazil is a protected area in the Brazilian Amazon. Located within the coordinates -55.4$^{\circ}$ and -54.8$^{\circ}$ E and -4.1$^{\circ}$ and -2.7$^{\circ}$ S, with 527 thousand ha of multiple ecosystems. Using `get_chirps()`, we take twenty random points across its area to get the precipitation from Jan-2010 to Dec-2018. Then, we compute the precipitation indices for this timeseries with intervals of 30 days using `precip_indices()`.

```r
library(chirps)

dat <- get_chirps(p, dates = c("2010-01-01","2018-01-31"))

pi <- precip_indices(dat, timeseries = TRUE, span = 30)

```





# Acknowledgments

This work is supported by the The Nordic Council of Ministers.


# References