#' Get CHIRPS precipitation data
#' 
#' Get daily precipitation data from the "Climate Hazards Group InfraRed
#'  Precipitation with Station Data" via ClimateSERV \acronym{API} client.
#'  ClimateSERV works with geojson of type 'Polygon'. The input \code{object}
#'  is then transformed into polygons with a small buffer area around the point.
#' 
#' @param object input, an object of class \code{\link[base]{data.frame}} or
#'  \code{\link[sf]{sf}}
#' @param dates a character of start and end dates in that order in the format
#'  "YYYY-MM-DD"
#' @param operation optional, an integer that represents which type of
#' statistical operation to perform on the dataset
#' @param as.sf logical, returns an object of class \code{[sf]{sf}}
#'  for S3 method of \pkg{chirps} class \code{sf}
#' @param ... further arguments passed to \code{\link[sf]{sf}} methods.
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
#' 
#' @examples
#' \donttest{
#'  
#' # Three points in the Tapajos National Forest, Brazil
#' library("chirps")
#' 
#' lonlat <- data.frame(lon = c(-55.0281,-54.9857, -55.0714),
#'                      lat = c(-2.8094, -2.8756, -3.5279))
#' 
#' dates <- c("2017-12-15","2017-12-31")
#' 
#' 
#' get_chirps(lonlat, dates)
#' 
#' 
#' # S3 method for objects of class 'sf'
#' library("sf")
#' 
#' lonlat <- data.frame(lon = c(-55.0281,-54.9857, -55.0714),
#'                      lat = c(-2.8094, -2.8756, -3.5279))
#' 
#' lonlat <- st_as_sf(lonlat, coords = c("lon","lat"))
#' 
#' dates <- c("2017-12-15","2017-12-31")
#' 
#' get_chirps(lonlat, dates)
#' 
#' # as.sf TRUE returns an object of class 'sf'
#' get_chirps(lonlat, dates, as.sf = TRUE)
#' } 
#' @import sf
#' @import methods
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @export
get_chirps <- function(object, dates, operation = 5, ...) {
  
  UseMethod("get_chirps")
  
}

#' @rdname get_chirps
#' @export
get_chirps.default <- function(object, dates, operation = 5, ...) {
  
  # validate lonlat to check if they are within the CHIRPS range lat -50, 50
  .validate_lonlat(object, xlim = c(-180, 180), ylim = c(-50, 50))
  
  # validate dates
  dates_inter <- .reformat_dates(dates, availability = c("1981-01-01", "0"))

  # get geojson strings from data.frame
  gj <- .dataframe_to_geojson(object, ...)
  
  result <- .GET(gjson = gj, 
                 dates = dates_inter, 
                 operation = operation, 
                 datatype = 0)
  
  names(result)[names(result) == "value"] <- "chirps"
  
  object$id <- rownames(object)
  
  result <- merge(result, object, by = "id")
  
  names(result)[3:5] <- c("chirps", "lon", "lat")
  
  result <- result[, c("id", "lon", "lat", "date", "chirps")]
  
  result <- tibble::as_tibble(result)
  
  class(result) <- c("chirps", class(result))
  
  return(result)
  
}

#' @rdname get_chirps
#' @method get_chirps sf
#' @export
get_chirps.sf <- function(object, dates, operation = 5, as.sf = FALSE, ...) {

  # convert sf into a data.frame
  n <- nrow(object)
  
  # find the sf_column
  sf_column <- attr(object, "sf_column")
  
  # unlist the sf_column
  lonlat <- unlist(object[[sf_column]])
  
  lonlat <- matrix(lonlat,
                   nrow = n,
                   ncol = 2, 
                   byrow = TRUE, 
                   dimnames = list(1:n, c("lon","lat")))
  
  lonlat <- as.data.frame(lonlat)
  
  # validate lonlat to check if they are within the CHIRPS range lat -50, 50
  .validate_lonlat(lonlat, xlim = c(-180, 180), ylim = c(-50, 50))
  
  # validate and reformat dates
  dates_inter <- .reformat_dates(dates, availability = c("1981-01-01", "0"))
  
  # get geojson strings from data.frame
  gj <- .sf_to_geojson(object, ...)
  
  result <- .GET(gjson = gj, 
                 dates = dates_inter, 
                 operation = operation, 
                 datatype = 0)
  
  if (as.sf) {
    
    result$date <- as.integer(result$date)
    result$date <- paste0("d_",result$date)
    
    result <- split(result, result$date)
    
    result <- lapply(result, function(x) {
      x <- x[order(x$id), ]
      x <- x[, "value"] 
    })
    
    result <- do.call("cbind", result)
    
    result <- cbind(object, result)
    
  } else {
    
    lonlat$id <- rownames(lonlat)
    
    result <- merge(result, lonlat, by = "id")
    
    names(result)[3:5] <- c("chirps", "lon", "lat")
    
    result <- result[, c("id", "lon", "lat", "date", "chirps")]
    
    result <- tibble::as_tibble(result)
    
    class(result) <- c("chirps", class(result))
    
  }
  
  return(result)
  
}