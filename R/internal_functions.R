# Concatenate lonlat into a geojson string of type Polygon for the API call
# 
# @param geometry a matrix for polygon geometry (lonlat)
# @return A geojson string of type 'Polygon' 
# @examples
# lonlat <- matrix(c(30,20,
#                    45,40,
#                    10,40,
#                    30,20), nrow = 4, ncol = 2, byrow = TRUE)
# 
# gj <- .c_geojson(lonlat)
# plot(sf::read_sf(gj))
# 
.c_geojson <- function(geometry) {
  
  geometry <- paste(geometry[,1], geometry[,2], sep = ",", collapse = "],[")
    
  gj <- paste0("{\"type\":\"Polygon\",\"coordinates\":", "[[[", geometry,  "]]]}")
  
  return(gj)
  
}

# Concatenate a coordinate point into a geojson polygon
#
# Take single points from geographical coordinates 
# and convert it into a geojson 'Polygon' string using sf::st_buffer
# 'Polygon' is the only geojson accepted by ClimateSERV
#
# @param lonlat a data.frame with geographical coordinates lonlat in that order
# @param dist numeric; buffer distance for all \code{lonlat}
# @return A list with geojson strings for each row in \code{lonlat}
# @examples
# # random geographic locations around bbox(10, 12, 45, 57)
# set.seed(123)
# lonlat <- data.frame(lon = runif(10, 10, 12),
#                      lat = runif(10, 45, 57))
# 
# .c_polygon(lonlat)
# 
.c_polygon <- function(lonlat, dist = 0.00001) {
  
  lonlat <- split(lonlat, 1:nrow(lonlat))
  
  geo <- lapply(lonlat, function(x) {
    x <- as.vector(t(x))
    
    x <- sf::st_point(x)
    
    x <- sf::st_buffer(x, dist)
    
    x <- x[[1]]
    
    .c_geojson(x)
  
  })
  
  return(geo)
    
}


# Validate lonlat within an accepted geographical range
# 
# @param lonlat a data.frame with geographical coordinates lonlat in that order
# @param xlim a numeric vector for the min and max accepted range in the longitude in that order
# @param ylim a numeric vector for the min and max accepted range in the latitude in that order 
# @param silent logical, return messages via \code{cat}
# @return nothing
# @examples
# # random geographic locations around bbox(10, 12, 45, 57)
# set.seed(123)
# lonlat <- data.frame(lon = runif(10, 10, 12),
#                      lat = runif(10, 45, 49))
# 
# .validate_lonlat(lonlat)

.validate_lonlat <- function(lonlat, xlim = c(-180, 180), ylim = c(-50, 50), silent = FALSE) {
  
  lon <- lonlat[,1]
  
  lat <- lonlat[,2]
  
  v1 <- min(lon) < xlim[1]
  v2 <- max(lon) > xlim[2]
  v3 <- min(lat) < ylim[1]
  v4 <- max(lat) > ylim[2]
  
  if(any(c(v1,v2,v3,v4))) {
    stop("lonlat are beyond the accepted lims, which are: " ,
         paste(paste(xlim[1], ylim[1], sep = " , "),
               paste(xlim[2], ylim[2], sep = " , "), sep = " , "))
  }
  if (!silent) {
    return(cat("lonlat OK! \n"))
  } 
}


# Validate dates within an accepted range
#
# @param x a character of start and end dates in that order in the format YYYY-MM-DD
# @param silent logical, return messages via \code{cat}
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
.validate_dates <- function(x, silent = FALSE) {
  
  xmin <- as.Date(x[1], format = "%Y-%m-%d")
  
  xmax <- as.Date(x[2], format = "%Y-%m-%d")
  
  accepted <- as.integer(xmax - xmin) > 1
  
  if(accepted & !silent) {
    cat("dates OK! \n")
  }
  
  if(!accepted) {
    stop("something wrong with dates provided, end date seems to be older than begin date \n")
  }
  
}





