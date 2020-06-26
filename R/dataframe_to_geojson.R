#' Concatenate a data.frame into a geojson polygon
#'
#' Take single points from geographical coordinates and convert it into a
#' geojson 'Polygon' string using \code{\link[sf:geos_unary]{st_buffer}}.
#'
#' @param lonlat a data.frame with geographical coordinates lonlat in 
#' that order
#' @inheritParams sf_to_geojson
#' @return An object of class 'geojson' for each row in \code{lonlat}
#' @family utility functions
#' @examples
#' \donttest{
#' # random geographic points within bbox(10, 12, 45, 47)
#' library("sf")
#' 
#' set.seed(123)
#' lonlat <- data.frame(lon = runif(2, 10, 12),
#'                      lat = runif(2, 45, 47))
#' 
#' gjson <- dataframe_to_geojson(lonlat)
#' }
#' @importFrom sf st_point st_sfc st_buffer st_write st_as_sf
#' @export
dataframe_to_geojson <- function(lonlat,
                                 dist = 0.00001,
                                 nQuadSegs = 2L, ...) {
  
  n <- dim(lonlat)[[1]]
  
  # lonlat into matrix
  lonlat <- as.matrix(lonlat)
  
  # split lonlat by rows
  lonlat <- split(lonlat, seq_len(n))
  
  # transform into sf points
  lonlat <- lapply(lonlat, function(l) {
    sf::st_point(l)
  })
  
  # and then into a geometry list column
  lonlat <- sf::st_sfc(lonlat)
  
  # set the buffer around the points
  lonlatb <- sf::st_buffer(lonlat,
                           dist = dist,
                           nQuadSegs = nQuadSegs, 
                           ...)
  
  # transform into a sf object
  lonlatb <- sf::st_as_sf(lonlatb)
  
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
