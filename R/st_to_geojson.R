#' Concatenate a sf object into a geojson polygon
#'
#' Take single points from geographical coordinates  and convert it into a
#' geojson 'Polygon' string using \code{\link[sf:geos_unary]{st_buffer}}.
#'
#' @param lonlat an object of class 'sf' and geometry type 'POINT' or 'POLYGON'
#' @param dist numeric, buffer distance for all \code{lonlat}
#' @param nQuadSegs integer, number of segments per quadrant
#' @param ... further arguments passed to \code{\link[sf]{sf}} methods
#' @return An object of class 'geosjon' for each row in \code{lonlat}
#' @family utility functions
#' @examples
#' \donttest{
#' # random geographic points within bbox(10, 12, 45, 47)
#' library("sf")
#' 
#' set.seed(123)
#' lonlat <- data.frame(lon = runif(5, 10, 12),
#'                      lat = runif(5, 45, 47))
#' 
#' lonlat <- st_as_sf(lonlat, coords = c("lon","lat"))
#' 
#' gjson <- sf_to_geojson(lonlat)
#' }
#' @export
sf_to_geojson <- function(lonlat,
                           dist = 0.00001,
                           nQuadSegs = 2L, ...) {
  # check geometry type
  type <- c("POINT", "POLYGON")
  
  # check for supported types 
  supp_type <- c(all(grepl(type[[1]], sf::st_geometry_type(lonlat))),
                 all(grepl(type[[2]], sf::st_geometry_type(lonlat))))
  
  if (!any(supp_type)) {
    stop("The sf geometry type is not supported. 
         Please provide a sf object of geometry type 'POINT' or 'POLYGON'\n")
  }
  
  type <- type[which(supp_type)]
  
  if (type == "POINT") {
    n <- dim(lonlat)[[1]]
    
    # set the buffer around the points
    lonlatb <- sf::st_buffer(lonlat,
                             dist = dist,
                             nQuadSegs = nQuadSegs, 
                             ...)
    
    # transform into a sf object
    lonlatb <- sf::st_as_sf(lonlatb)  
  }
  
  if (type == "POLYGON") {
    
    lonlatb <- lonlat
    
  }
  
  
  # write the geojson string
  tf <- tempfile(fileext = ".geojson")
  sf::st_write(lonlatb, tf, quiet = TRUE)
  
  # capture these strings
  gj <- readLines(tf)
  
  # keep only the geometry vectors
  index <- which(grepl("geometry", unlist(gj)))
  
  gj <- gj[index]
  
  # remove spaces and extra commas 
  gj <- lapply(gj, function(x) {
    gsub(" ", "", x)
  })
  
  gjson <- lapply(gj, function(x) {
    x <- gsub("}},", "}}", x)
  })
  
  result <- unlist(gjson)
  
  class(result) <- c("geojson", "json", class(result))
  
  return(result)
  
}
