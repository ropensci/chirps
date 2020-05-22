#' Get CHIRPS precipitation data
#' 
#' Get daily precipitation data from the "Climate Hazards Group InfraRed
#'  Precipitation with Station Data" via ClimateSERV \acronym{API} client.
#'  ClimateSERV works with geojson of type 'Polygon'. The input \code{object}
#'  is then transformed into polygons with a small buffer area around the point.
#' 
#' @param object input, an object of class \code{\link[base]{data.frame}} (or
#'  any other object that can be coerced to data.frame), \code{geojson} or 
#'  \code{\link[sf]{sf}}
#' @param dates a character of start and end dates in that order in the format
#'  "YYYY-MM-DD"
#' @param operation optional, an integer that represents which type of
#' statistical operation to perform on the dataset
#' @param as.sf logical, returns an object of class \code{\link[sf]{sf}}
#' @param as.geojson logical, returns an object of class \code{geojson}
#' @param ... further arguments passed to \code{\link[sf]{sf}} methods
#'  See details 
#'  
#' @details
#'  \bold{operation}: supported operations are:
#'  \tabular{rll}{
#'  \bold{operation}      \tab    \tab \bold{value}\cr
#'  max                   \tab =  \tab 0\cr
#'  min                   \tab =  \tab 1\cr
#'  median                \tab =  \tab 2\cr
#'  sum                   \tab =  \tab 4\cr
#'  average               \tab =  \tab 5 (\emph{default value})\cr
#'  }
#' 
#' \bold{dist}: numeric, buffer distance for each \code{object} coordinate
#' 
#' \bold{nQuadSegs}: integer, number of segments per buffer quadrant
#' 
#' @return A data frame of \acronym{CHIRPS} data:
#' \describe{
#'   \item{id}{the index for the rows in \code{object}}
#'   \item{dates}{the dates from which \acronym{CHIRPS} was requested}
#'   \item{lon}{the longitude as provided in \code{object}}
#'   \item{lat}{the latitude as provided in \code{object}}
#'   \item{chirps}{the \acronym{CHIRPS} value in mm}
#' }
#' @references 
#' 
#' Funk C. et al. (2015). Scientific Data, 2, 150066.
#'  \cr\url{https://doi.org/10.1038/sdata.2015.66}
#' 
#' ClimateSERV \url{https://climateserv.servirglobal.net}
#' @note get_chirps may return some warning messages given by 
#' \code{\link[sf]{sf}}, please look sf documentation for 
#' possible issues.
#' @examples
#' \donttest{
#' lonlat <- data.frame(lon = c(-55.0281,-54.9857),
#'                      lat = c(-2.8094, -2.8756))
#' 
#' dates <- c("2017-12-15", "2017-12-31")
#' 
#' dt <- get_chirps(lonlat, dates)
#' 
#' dt
#' 
#' }
#' 
#' @importFrom sf st_centroid read_sf st_geometry_type
#' @export
get_chirps <- function(object, dates, operation = 5, ...) {
  
  UseMethod("get_chirps")
  
}

#' @rdname get_chirps
#' @export
get_chirps.default <- function(object, dates, operation = 5, 
                               ...) {
  
  
  object <- as.data.frame(object)
  
  # validate lonlat to check if they are within the CHIRPS range lat -50, 50
  .validate_lonlat(object, xlim = c(-180, 180), ylim = c(-50, 50))
  
  # validate dates
  dates_inter <- .reformat_dates(dates, availability = c("1981-01-01", "0"))

  # get geojson strings from data.frame
  gj <- dataframe_to_geojson(object, ...)
  
  class(gj) <- "character"
  
  gj <- split(gj, seq_along(gj))
  
  result <- .GET(gjson = gj, 
                 dates = dates_inter, 
                 operation = operation, 
                 datatype = 0)
  
  names(result)[names(result) == "value"] <- "chirps"
  
  object$id <- rownames(object)
  
  result <- merge(result, object, by = "id")
  
  names(result)[3:5] <- c("chirps", "lon", "lat")
  
  result <- result[, c("id", "lon", "lat", "date", "chirps")]
  
  result <- as.data.frame(result, stringsAsFactors = FALSE)
  
  class(result) <- c("chirps", "chirps_df", class(result))
  
  return(result)
  
}

#' @rdname get_chirps
#' @method get_chirps sf
#' @export
get_chirps.sf <- function(object, dates, operation = 5, 
                          as.sf = FALSE, 
                          ...) {
  
  # check geometry type
  type <- c("POINT", "POLYGON")
  
  # check for supported types 
  supp_type <- c(all(grepl(type[[1]], sf::st_geometry_type(object))),
                 all(grepl(type[[2]], sf::st_geometry_type(object))))

  if (!any(supp_type)) {
    stop("The sf geometry type is not supported. 
         Please provide a sf object of geometry type 'POINT' or 'POLYGON'\n")
  }
  
  type <- type[which(supp_type)]
  
  nr <- dim(object)[[1]]
  
  # find the sf_column
  index <- attr(object, "sf_column")
  
  # get the sf column
  lonlat <- object[[index]]
  
  if (type == "POINT") {
    
    # unlist the sf_column
    lonlat <- unlist(object[[index]])
    
  }
  
  if (type == "POLYGON") {
    
    # set centroid to validade lonlat
    lonlat <- sf::st_centroid(lonlat)
    
    # unlist the sf_column
    lonlat <- unlist(lonlat)
    
  }
  
  lonlat <- matrix(lonlat,
                   nrow = nr,
                   ncol = 2, 
                   byrow = TRUE, 
                   dimnames = list(seq_len(nr), c("lon","lat")))
  
  lonlat <- as.data.frame(lonlat)
  
  # validate lonlat to check if they are within the CHIRPS range
  .validate_lonlat(lonlat, xlim = c(-180, 180), ylim = c(-50, 50))
  
  # validate and reformat dates
  dates_inter <- .reformat_dates(dates, availability = c("1981-01-01", "0"))
  
  # get geojson strings from data.frame
  gj <- sf_to_geojson(object, ...)
  
  class(gj) <- "character"
  
  gj <- split(gj, seq_along(gj))
  
  result <- .GET(gjson = gj, 
                 dates = dates_inter, 
                 operation = operation, 
                 datatype = 0)
  
  if (isTRUE(as.sf)) {
    
    result$date <- as.integer(result$date)
    result$date <- paste0("day_",result$date)
    
    result <- split(result, result$date)
    
    result <- lapply(result, function(x) {
      x <- x[order(x$id), ]
      x <- x[, "value"] 
    })
    
    result <- do.call("cbind", result)
    
    result <- cbind(object, result)
    
  } 
  
  if (isFALSE(as.sf)) {
    
    lonlat$id <- rownames(lonlat)
    
    result <- merge(result, lonlat, by = "id", all.x = TRUE)
    
    names(result)[3:5] <- c("chirps", "lon", "lat")
    
    result <- result[, c("id", "lon", "lat", "date", "chirps")]
    
    result <- as.data.frame(result, stringsAsFactors = FALSE)
    
    class(result) <- c("chirps", "chirps_df", class(result))
    
  }
  
  return(result)
  
}

#' @rdname get_chirps
#' @method get_chirps geojson
#' @export
get_chirps.geojson <- function(object, dates, operation = 5, 
                               as.geojson = FALSE,
                               ...) {
  
  # check for the geometry tag
  if (isFALSE(grepl("geometry", object[[1]]))) {
    stop("geometry tag is missing in the geojson object\n")
  }
  
  type <- c("type\":\"Point", "type\":\"Polygon")
  
  # check for supported types 
  supp_type <- c(all(grepl(type[[1]], object)),
                 all(grepl(type[[2]], object)))
  
  if (isFALSE(any(supp_type))) {
    stop("The geojson geometry type is not supported. 
         Please provide a geojson of geometry type 'Point' or 'Polygon'\n")
  }
  
  # if type Point
  if (all(grepl(type[[1]], object))) {
    
    # get matrix with lonlat to validate later
    lonlat <- lapply(object, function (x) {
      
      # read as sf
      x <- sf::read_sf(x)
      
      # find the sf_column
      index <- attr(x, "sf_column")
       
      # unlist the sf_column
      x <- unlist(x[[index]])

    })
    
    # put all together
    lonlat <- do.call("rbind", lonlat)
    
    lonlat <- as.data.frame(lonlat)
    
    # lonlat into a geojson Polygon
    gjson <- dataframe_to_geojson(lonlat, ...)
    
  }
  
  # if Polygon
  if (all(grepl(type[[2]], object))) {
    
    # take the centroid from geojson Polygons
    # to validate lonlat coordinates
    lonlat <- lapply(object, function (x) {
      
      x <- sf::read_sf(x)
      
      x <- sf::st_centroid(x$geometry)
      
      x <- unlist(x)
    })
    
    # put all together
    lonlat <- do.call("rbind", lonlat)
    
    lonlat <- as.data.frame(lonlat)
    
    gjson <- split(object, seq_along(object))
    
  }
  
  # validate lonlat to check if they are within the CHIRPS range lat -50, 50
  .validate_lonlat(lonlat, xlim = c(-180, 180), ylim = c(-50, 50))
  
  # validate dates
  dates_inter <- .reformat_dates(dates, availability = c("1981-01-01", "0"))
  
  result <- .GET(gjson = gjson,
                 dates = dates_inter,
                 operation = operation,
                 datatype = 0)
  
  
  if (isTRUE(as.geojson)) {
    
    result <- split(result, result$id)
    
    object <- split(object, seq_along(object))
    
    # add geojson properties
    result <- mapply(function(X, Y) {
      
      .add_geojson_properties(geometry = X,
                              properties = Y,
                              name = "chirps")
      
    }, X = object, Y = result[])
    
    class(result) <- c("geojson", "json", class(result))
    
    
  }
  
  if (isFALSE(as.geojson)) {
    
    lonlat$id <- rownames(lonlat)
    
    result <- merge(result, lonlat, by = "id")
    
    names(result)[3:5] <- c("chirps", "lon", "lat")
    
    result <- result[, c("id", "lon", "lat", "date", "chirps")]
    
    result <- as.data.frame(result, stringsAsFactors = FALSE)
    
    class(result) <- c("chirps", "chirps_df", class(result))
    
  }
  
  return(result)
  
}
