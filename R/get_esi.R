#' Get evaporative stress index (ESI) data
#' 
#' Get evaporative stress index (\acronym{ESI}) from SERVIR Global
#' via ClimateSERV \acronym{API} Client. \acronym{ESI} is available every four
#'  (or twelve) weeks from 2001 to present.
#' The dataset may contain cloudy data which is returned as NAs.
#' ClimateSERV works with geojson of type 'Polygon'. The input \code{object} is
#'  then transformed into polygons with a small buffer area around the point.
#' 
#' @param object input, an object of class \code{\link[base]{data.frame}} or
#'  \code{\link[sf]{sf}}
#' @param dates a character of start and end dates in that order in the format
#'  YYYY-MM-DD
#' @param operation optional, an integer that represents which type of
#' statistical operation to perform on the dataset
#' @param period an integer value for the period of ESI data, four weeks period
#'  = 1, twelve weeks = 2
#' @param as.sf logical, returns an object of class \code{sf} for S3 method of
#'  class \code{\link[sf]{sf}}
#' @param ... further arguments passed to \code{\link[sf]{sf}} methods. See
#'  details 
#' @details  
#' operation: supported operations are max = 0, min = 1, median = 2, sum = 4,
#'  average = 5
#' 
#' dist: numeric, buffer distance for each \code{object} coordinate
#' 
#' nQuadSegs: integer, number of segments per buffer quadrant
#' 
#' @return A data frame of \acronym{ESI} data:
#' \item{id}{the index for the rows in \code{object}}
#' \item{dates}{the dates from which ESI was requested}
#' \item{lon}{the longitude as provided in \code{object}}
#' \item{lat}{the latitude as provided in \code{object}}
#' \item{esi}{the ESI value}
#' @references 
#' 
#' ClimateSERV <https://climateserv.servirglobal.net>
#' 
#' Evaporative Stress Index <http://catalogue.servirglobal.net/Product?product_id=198>
#' 
#' @examples
#' \donttest{
#' library("chirps")
#' 
#' lonlat <- data.frame(lon = c(-55.0281,-54.9857, -55.0714),
#'                      lat = c(-2.8094, -2.8756, -3.5279))
#' 
#' dates <- c("2017-12-15","2018-06-20")
#' 
#' # by default the function set a very small buffer around the points
#' # which can return NAs due to cloudiness in ESI data
#' 
#' dat <- get_esi(lonlat, dates = dates)
#' 
#' dat
#' 
#' # the argument dist passed through sf increase the buffer area
#' 
#' dat <- get_esi(lonlat, dates = dates, dist = 0.1)
#' 
#' dat
#' } 
#' @export
get_esi <- function(object, dates, operation = 5, period = 1, ...) {
  
  UseMethod("get_esi")
  
}

#' @rdname get_esi
#' @export
get_esi.default <- function(object, dates, operation = 5, period = 1, ...) {
  
  # validate lonlat to check if they are within the CHIRPS range lat -50, 50
  .validate_lonlat(object, xlim = c(-180, 180), ylim = c(-50, 50))
  
  # validate dates
  dates_inter <- .reformat_dates(dates, availability = c("2001-01-01", "0"))
  
  # get geojson strings from data.frame
  gj <- .dataframe_to_geojson(object, ...)
  
  if (period == 1) {
    datatype <- 29
  }
  if (period == 2) {
    datatype <- 33
  }
  
  result <- .GET(gjson = gj, 
                 dates = dates_inter, 
                 operation = operation, 
                 datatype = datatype)
  
  names(result)[names(result) == "value"] <- "esi"
  
  object$id <- rownames(object)
  
  result <- merge(result, object, by = "id", all.y = TRUE)
  
  names(result)[3:5] <- c("esi", "lon", "lat")
  
  result <- result[, c("id", "lon", "lat", "date", "esi")]
  
  result <- tibble::as_tibble(result)
  
  return(result)
  
}

#' @rdname get_esi
#' @method get_esi sf
#' @export
get_esi.sf <- function(object, dates, operation = 5, period = 1, as.sf = FALSE,
                       ...) {
  
  # convert sf into a data.frame
  n <- nrow(object)
  
  lonlat <- unlist(object$geometry)
  
  lonlat <- matrix(lonlat,
                   nrow = n,
                   ncol = 2, 
                   byrow = TRUE, 
                   dimnames = list(1:n, c("lon","lat")))
  
  lonlat <- as.data.frame(lonlat)
  
  # validate lonlat to check if they are within the CHIRPS range lat -50, 50
  .validate_lonlat(lonlat, xlim = c(-180, 180), ylim = c(-50, 50))
  
  # validate dates
  dates_inter <- .reformat_dates(dates, availability = c("1981-01-01", "0"))
  
  # get geojson strings from data.frame
  gj <- .sf_to_geojson(object, ...)
  
  if (period == 1) {
    datatype <- 29
  }
  if (period == 2) {
    datatype <- 33
  }
  
  result <- .GET(gjson = gj, 
                 dates = dates_inter, 
                 operation = operation, 
                 datatype = datatype)
  
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
    
    names(result)[3:5] <- c("esi", "lon", "lat")
    
    result <- result[, c("id", "lon", "lat", "date", "esi")]
    
    result <- tibble::as_tibble(result)

    
  }
  
  return(result)
  
}