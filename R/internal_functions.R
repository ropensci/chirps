# Concatenate a coordinate point into a geojson polygon
#
# Take single points from geographical coordinates 
# and convert it into a geojson 'Polygon' string using sf::st_buffer
# 'Polygon' is the only geojson format accepted by ClimateSERV
#
# @param lonlat a data.frame with geographical coordinates lonlat in that order
# @param dist numeric; buffer distance for all \code{lonlat}
# @param nQuadSegs integer; number of segments per quadrant 
# @return A list with geojson strings for each row in \code{lonlat}
# @examples
# # random geographic locations around bbox(10, 12, 45, 57)
# set.seed(123)
# lonlat <- data.frame(lon = runif(10, 10, 12),
#                      lat = runif(10, 45, 57))
# 
# .c_geojson(lonlat)
# 
.c_geojson <- function(lonlat, dist = 0.00001, nQuadSegs = 2L) {
  
  n <- nrow(lonlat)
  
  # lonlat into matrix
  lonlat <- as.matrix(lonlat)
  
  # split lonlat by rows
  lonlat <- split(lonlat, 1:nrow(lonlat))
  
  # transform into sf points
  lonlat <- lapply(lonlat, function(l) {
    sf::st_point(l)
  })
  
  # and then into a geometry list colunm
  lonlat <- sf::st_sfc(lonlat)
  
  # set a temporary file with geojson extension
  tf <- tempfile(fileext = ".geojson")
  
  # set the buffer around the points
  lonlatb <- sf::st_buffer(lonlat, 
                           dist = dist, 
                           nQuadSegs = nQuadSegs)
  
  # transform into a sf object
  lonlatb <- sf::st_as_sf(lonlatb)
  
  # write the geojson string
  sf::st_write(lonlatb, tf, quiet = TRUE)
  
  # capture these strings
  gj <- readLines(tf)
  
  # first 4 lines are for the features and last 2 lines to close features
  # keep only geojson geometries
  gj <- gj[5:(n+4)]
  
  gj <- split(gj, 1:n)
  
  return(gj)
  
}


# Validate lonlat within an accepted geographical range
# 
# @param lonlat a data.frame with geographical coordinates lonlat in that order
# @param xlim a numeric vector for the min and max accepted range in the longitude in that order
# @param ylim a numeric vector for the min and max accepted range in the latitude in that order 
# @return nothing
# @examples
# # random geographic locations around bbox(10, 12, 45, 57)
# set.seed(123)
# lonlat <- data.frame(lon = runif(10, 10, 12),
#                      lat = runif(10, 45, 49))
# 
# .validate_lonlat(lonlat)

.validate_lonlat <- function(lonlat, 
                             xlim = c(-180, 180), 
                             ylim = c(-50, 50)) {
  
  lon <- lonlat[,1]
  
  lat <- lonlat[,2]
  
  v1 <- min(lon) < xlim[1]
  v2 <- max(lon) > xlim[2]
  v3 <- min(lat) < ylim[1]
  v4 <- max(lat) > ylim[2]
  
  if(any(c(v1,v2,v3,v4))) {
    stop("lonlat are beyond the accepted lims, which are: ",
         paste(paste(xlim[1], ylim[1], sep = " , "),
               paste(xlim[2], ylim[2], sep = " , "), sep = " , "))
  }
}


# Validate dates within an accepted range
#
# @param x a character of start and end dates in that order in the format YYYY-MM-DD
# @return nothing
# @examples
# dates <- c("2016-01-31","2017-12-01")
# 
# .validate_dates(dates)
# 
# 
# dates <- c("2018-01-31","2017-12-01")
# 
# .validate_dates(dates)
.validate_dates <- function(x) {
  
  xmin <- as.Date(x[1], format = "%Y-%m-%d")
  
  xmax <- as.Date(x[2], format = "%Y-%m-%d")
  
  accepted <- as.integer(xmax - xmin) > 1
  
  if(!accepted) {
    stop("something wrong with dates provided, 
         end date seems to be older than begin date \n")
  }
  
}







