#' Methods to coerce geographical coordinates into a geojson polygon
#'
#' Take single points from geographical coordinates and coerce into a geojson of
#'  geometry 'Polygon'
#'
#' @param lonlat a \code{data.frame} or matrix with geographical coordinates
#'  'lonlat', in that order, or an object of class \code{\link[sf]{sf}} with
#'  geometry type 'POINT' or 'POLYGON'
#' @param dist numeric, buffer distance for all \code{lonlat}
#' @param nQuadSegs integer, number of segments per quadrant
#' @param ... further arguments passed to \code{\link[sf]{sf}} methods
#' @return An object of class 'geosjon' for each row in \code{lonlat}
#' @family utility functions
#'
#' @examplesIf interactive()
#' # Default S3 Method
#' # random geographic points within bbox(10, 12, 45, 47)
#' library("sf")
#'
#' set.seed(123)
#' lonlat <- data.frame(lon = runif(1, 10, 12),
#'                      lat = runif(1, 45, 47))
#'
#' gjson <- as.geojson(lonlat)
#'
#' #################
#'
#' # S3 Method for objects of class 'sf'
#' # random geographic points within bbox(10, 12, 45, 47)
#' library("sf")
#'
#' set.seed(123)
#' lonlat <- data.frame(lon = runif(5, 10, 12),
#'                      lat = runif(5, 45, 47))
#'
#' lonlat <- st_as_sf(lonlat, coords = c("lon","lat"))
#'
#' gjson <- as.geojson(lonlat)
#'
#' @importFrom sf st_point st_sfc st_buffer st_write st_as_sf
#' @export
as.geojson <- function(lonlat,
                       dist = 0.00001,
                       nQuadSegs = 2L,
                       ...) {
  UseMethod("as.geojson")
}

#' @rdname as.geojson
#' @export
as.geojson.default <- function(lonlat,
                               dist = 0.00001,
                               nQuadSegs = 2L,
                               ...) {
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
  lonlat <- sf::st_sfc(lonlat,
                       crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  
  # set the buffer around the points
  lonlatb <- sf::st_buffer(lonlat,
                           dist = dist,
                           nQuadSegs = nQuadSegs)
  
  # transform into a sf object
  lonlatb <- sf::st_as_sf(x = lonlatb)
                          
  
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
    x <- strsplit(x, "},")[[1]][2]
    x <- gsub(" ", "", x)
    x <- gsub("}},", "}}", x)
    x <- gsub('"geometry\":', "", x)
    x <- gsub(']}}', "]}", x)
    x
  })
  
  result <- unlist(gj)
  
  class(result) <- c("geojson", "json", class(result))
  
  return(result)
  
}

#' @rdname as.geojson
#' @method as.geojson sf
#' @export
as.geojson.sf <- function(lonlat,
                          dist = 0.00001,
                          nQuadSegs = 2L,
                          ...) {
  # check geometry type
  type <- c("POINT", "POLYGON")
  
  # check for supported types
  supp_type <-
    c(all(grepl(type[[1]], sf::st_geometry_type(lonlat))),
      all(grepl(type[[2]], sf::st_geometry_type(lonlat))))
  
  if (!any(supp_type)) {
    stop(
      "The sf geometry type is not supported.
         Please provide a sf object of geometry type 'POINT' or 'POLYGON'\n"
    )
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
    lonlatb <- sf::st_as_sf(lonlatb,
                            crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
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
    x <- strsplit(x, "},")[[1]][2]
    x <- gsub(" ", "", x)
    x <- gsub("}},", "}}", x)
    x <- gsub('"geometry\":', "", x)
    x <- gsub(']}}', "]}", x)
    x
  })
  
  result <- unlist(gj)
  
  class(result) <- c("geojson", "json", class(result))
  
  return(result)
  
}
