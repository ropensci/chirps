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
  - name: Bioversity International, Rome, Italy
    index: 2
  - name: Centre for Crop Health, University of Southern Queensland, Toowoomba, Australia
    index: 3
  - name: Universities Space Research Association, National Aeronautics and Space Administration (NASA), Huntsville, USA
    index: 4
citation_author: de Sousa et. al.
date: "22 May 2020"
year: 2020
bibliography: paper.bib
output: rticles::joss_article
journal: JOSS
---

# Summary

The *chirps* package provides functionalities for reproducible analysis in R [@RCoreTeam] using the CHIRPS [@Funk2015] data. CHIRPS is daily precipitation data set developed by the Climate Hazards Group [@Funk2015] for high resolution precipitation gridded data. Spanning 50$^{\circ}$ S to 50$^{\circ}$ N (and all longitudes) and ranging from 1981 to near-present (normally with a 45 day lag), CHIRPS incorporates 0.05 arc-degree resolution satellite imagery, and in-situ station data to create gridded precipitation time series for trend analysis and seasonal drought monitoring [@Funk2015]. Additionally, the package provides the API client for the IMERG [@Huffman2014] and ESI [@esi] data.
The Integrated Multi-satelliE Retrievals for GPM (IMERG) data provides near-real time global observations of rainfall at 0.5 arc-degree resolution, which can be used to estimate total rainfall accumulation from storm systems and quantify the intensity of rainfall and flood impacts from tropical cyclones and other storm systems. IMERG is a daily precipitation dataset available from 2015 to near-present. The evaporative stress index (ESI) data describes temporal anomalies in evapotranspiration produced weekly at 0.25 arc-degree resolution for the entire globe [@Anderson2011]. The ESI data is based on satellite observations of land surface temperature, which are used to estimate water loss due to evapotranspiration (the sum of evaporation and plant transpiration from the Earth's land and ocean surface to the atmosphere). The ESI data is available from 2001 to near-present. When using these data sets in publications please cite @Funk2015 for CHIRPS, @Huffman2014 for IMERG and @esi for ESI.

# Implementation

Four main functions are provided, `get_chirps()`, `get_imerg()`, `get_esi()` and `precip_indices()`. The `get_chirps()` function provides access to CHIRPS data via the ClimateSERV API Client [@ClimateSERV] with methods to handle objects of class 'data.frame', 'geojson' and 'sf' via the package *methods* [@RCoreTeam]. To accept the query, ClimateSERV requires a geojson object of type 'Polygon' (one single polygon per request). Using the package *sf* [@sf] internally, the input provided in `get_chirps()` is transformed into a list of polygons with a small buffer area (0.0001 arc-sec by default) around the point and transformed into a list of geojson strings. *chirps* uses *crul* [@crul] to interface with ClimateSERV API. The query returns a JSON object parsed to *jsonlite* [@jsonlite] to obtain the data frame for the time series required. `get_chirps()` returns a data.frame, which also inherits the classes 'chirps' and 'chirps_df', where each id represents the index for the rows in the in-putted 'object'. The function `get_imerg()` returns the precipitation data from the IMERG data set. The function works with the same parameters described for `get_chirps()` and also inherits the classes 'chirps' and 'chirps_df'. The function `get_esi()` returns the evaporative stress index (ESI) data [@Anderson2011], and works similarly to `get_chirps()` returning a data.frame which inherit the class 'chirps_df'. Users providing objects of class 'sf' and 'geojson' in `get_chirps()`, `get_imerg()` and `get_esi()` can also choose to return an object with the same class as the object provided using the arguments 'as.sf = TRUE' or 'as.geojson = TRUE'. With the function `precip_indices()` users can assess how the precipitation changes across the requested time series using precipitation variability indices [@Aguilar2005], computed using *stats* [@RCoreTeam], the main input is an object of class 'chirps'. Extended documentation is provided with examples on how to increase the buffer area and draw quadrants for the geojson polygon using *sf* [@sf].

# Application: a case study in the Tapajós National Forest

The *Tapajós* National Forest is a protected area in the Brazilian Amazon. Located within the coordinates -55.4$^{\circ}$ and -54.8$^{\circ}$ E and -4.1$^{\circ}$ and -2.7$^{\circ}$ S with ~ 527,400 ha of multiple Amazonian ecosystems. We take twenty random points across its area to get the precipitation from Jan-2008 to Dec-2018 using `get_chirps()`. We use an object of class 'sf' which is passed to the method `get_chirps.sf()`. Then, we compute the precipitation indices for the time series with intervals of 30 days using `precip_indices()`.

```r
library("chirps")
library("sf")

data("tapajos", package = "chirps")
set.seed(1234)
tp <- st_sample(tapajos, 20)
tp <- st_as_sf(tp)

dt <- get_chirps(tp, dates = c("2008-01-01","2018-01-31"))

p_ind <- precip_indices(dt, timeseries = TRUE, intervals = 30)

```

We selected four indices for the visualization using *tidyverse* [@tidyverse]. Plots were ensembled together using *gridExtra* [@gridExtra]. Here we see how these indices are changing across the time series (Figure 1). In this quick assessment, we note an increasing extent of consecutive dry days (MLDS) across the time series, with also a decrease in the number of consecutive rainy days (MLWS), which stays above the historical average for MLDS and bellow the historical average for MLWS. The trends also show a decrease in the total rainfall in the 30-days intervals, staying below the average after 2014. Finally, we note a decrease in maximum consecutive 5-days precipitation, which also stays bellow the historical average. 

\begin{figure}
\includegraphics[width=0.9\linewidth]{Fig1} \caption{Trends in precipitation variability across the Tapajós National Forest, Brazil, for the period of 01-Jan-2010 to 31-Dec-2018 with four precipitation indices. MLDS, maximum length of consecutive dry days (days), MLWS, maximum length of consecutive wet days (days), Rtotal, total precipitation (mm), Rx5day, maximum consecutive 5-days precipitation (mm). Red lines indicates the historical mean of each index in the time series. Blue line indicates the smoothed trends in each index using the 'loess' method.}\label{fig:fig1}
\end{figure}

# Other applications and conclusion

Deriving precipitation indices that can be obtained from CHIRPS proved to be an excellent approach to evaluate the climate variability using precipitation data [@DeSousa2018] and the effects of climate change on a continental analysis [@Aguilar2005]. Additionally, these indices can be used to register specific effects of climate variability on crop varietal performance. In crop modelling, @Kehel2016 applied this to assess the interactions of wheat varieties with the environment, showing how severe drought, assessed with the maximum length of dry spell (MLDS), can affect the plant development and the yield. These indices can also be useful to improve variety recommendation for climate adaptation in marginal production environments [@vanEtten2019]. 

Overall, CHIRPS data can be used in many applications and currently has over 800 citations from studies using this tool. Many applications are the field of agriculture, hydrologic modelling and drought monitoring, but also some studies using this in disease control programs (e.g. @Thomson2017, @Horn2018). The *chirps* package aims to make it possible for researchers in these fields to implement this tool into a replicable and reproducible workflow in R. 

# Acknowledgements

This work was supported by The Nordic Joint Committee for Agricultural and Food Research (grant num. 202100-2817). The idea for this package was conceived during the course "Analysing Spatial Data" at the Norwegian School of Economics (NHH), we thank Professor Roger Bivand for his insights.

# References
