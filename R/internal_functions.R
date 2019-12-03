# Concatenate a character in the format of geojson of type Polygon for the API call
# @param geometry a matrix for polygon geometry (lonlat)
# @return a character with the structure of geojson
# @examples
# lonlat <- matrix(c(30,20,
#                    45,40,
#                    10,40,
#                    30,20), nrow = 4, ncol = 2, byrow = TRUE)
# 
# gj <- .c_geojson(lonlat)
# plot(sf::read_sf(gj))
.c_geojson <- function(geometry) {
  
  geometry <- paste(geometry[,1], geometry[,2], sep = ",", collapse = "],[")
    
  gj <- paste0("{\"type\":\"Polygon\",\"coordinates\":", "[[[", geometry,  "]]]}")
  
  return(gj)
  
}


# random geographic locations around bbox(10, 12, 55, 57)
# set.seed(123)
# lonlat <- data.frame(lon = runif(10, 10, 12),
#                      lat = runif(10, 55, 57))
# 
# .c_polygon(lonlat)
.c_polygon <- function(lonlat) {
  
  lonlat <- split(lonlat, 1:nrow(lonlat))
  
  geo <- lapply(lonlat, function(x) {
    x <- as.vector(t(x))
    
    x <- sf::st_point(x)
    
    x <- sf::st_buffer(x, 0.00001)
    
    x <- x[[1]]
    
    .c_geojson(x)
  
  })
  
  return(geo)
    
}



