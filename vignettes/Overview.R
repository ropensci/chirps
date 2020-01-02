## ----setup, include=FALSE------------------------------------------------
TRAVIS <- !identical(tolower(Sys.getenv("TRAVIS")), "true")
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  purl = TRAVIS
)

## ----install, message=FALSE, eval=FALSE, echo=TRUE-----------------------
#  library("devtools")
#  
#  devtools::install_github("agrobioinfoservices/chirps")
#  

## ----get, message=TRUE, eval=TRUE, echo=TRUE-----------------------------
library("chirps")

set.seed(12)
lonlat <- data.frame(lon = runif(2, -55, -54),
                     lat = runif(2, -3, -2))

dates <- c("2015-01-01","2016-12-31")

dat <- get_chirps(lonlat, dates)

head(dat)


## ----indices, message=TRUE, eval=TRUE, echo=TRUE-------------------------
library("chirps")

pi <- precip_indices(dat, timeseries = TRUE, span = 7)

head(pi)


