#' Get CHIRPS precipitation data
#' 
#' Get the Climate Hazards Group InfraRed Precipitation with Station Data via 
#' ClimateSERV API Client. ClimateSERV works with geojson strings of type 'Polygon'. The 
#' input 'lonlat' are then transformed into polygons with a small buffer area around 
#' the point.
#' 
#' @param object input, an object of class data.frame, geojson (Polygon), json (Polygon) or sf
#' @param dates a character of start and end dates in that order in the format YYYY-MM-DD
#' @param operation optional, an integer that represents which type of statistical operation 
#' to perform on the dataset
#' @param ... further arguments passed to \code{sf} methods. See details 
#' @details  
#' operation: supported operations are max = 0, min = 1, median = 2, sum = 4, average = 5
#' 
#' dist: numeric, buffer distance for all \code{lonlat} coordinates
#' 
#' nQuadSegs: integer, number of segments per quadrant
#' 
#' @return A data frame of CHIRPS data including:
#' \item{id}{the index for the rows in \code{lonlat}}
#' \item{dates}{the dates from which CHIRPS was requested}
#' \item{lon}{the longitude as provided in \code{lonlat}}
#' \item{lat}{the latitude as provided in \code{lonlat}}
#' \item{chirps}{the CHIRPS value in mm}
#' @references 
#' 
#' Funk C. et al. (2015). Scientific Data, 2, 150066. https://doi.org/10.1038/sdata.2015.66
#' 
#' ClimateSERV https://climateserv.servirglobal.net
#' 
#' @examples
#' \donttest{
#' # Three points in the Tapajos National Forest, Brazil
#' library("chirps")
#' 
#' lonlat <- data.frame(lon = c(-55.0281,-54.9857, -55.0714),
#'                      lat = c(-2.8094, -2.8756, -3.5279))
#' 
#' dates <- c("2017-12-15","2018-01-31")
#' 
#' 
#' df <- get_chirps(lonlat, dates)
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
get_chirps.default <-  function(object, dates, operation = 5, ...) {
  
  nr <- nrow(object)
  
  # validate lonlat to check if they are within the CHIRPS range lat -50, 50
  .validate_lonlat(object, xlim = c(-180, 180), ylim = c(-50, 50))
  
  dates <- .reformat_dates(dates)
  begindate <- dates[1]
  enddate <- dates[2]
  
  # get geojson strings from lonlat
  gjson <- .dataframe_to_geojson(object, ...)
  
  # submit data request
  ids <- lapply(gjson, function(x) {
    
    i <- .send_request(
      datatype = 0,
      begintime = begindate,
      endtime = enddate,
      intervaltype = 0,
      operationtype = operation,
      geometry = x
    )
    
    return(i)
    
  })
  
  # check server progress with data
  i <- 0
  client_progress <-
    "https://climateserv.servirglobal.net/chirps/getDataRequestProgress/?"
  progress <- paste0(client_progress, "id=", ids[[length(ids)]])

  # repeat checking the server until we're at 100% complete with results
  repeat {
    i <- suppressWarnings(readLines(progress))
    if (jsonlite::fromJSON(i) != -1 & jsonlite::fromJSON(i) == 100) break
  }
  
  # get data from request
  result <- lapply(ids, function(x) {
    
    d <- .get_data_from_request(id = x)
    
    return(d)
    
  })

  
  result <- do.call("rbind", result)

  # fix ids
  id <- strsplit(row.names(result), "[.]")
  id <- do.call("rbind", id)[,1]
  result$id <- id

  # transform dates to the original format as input
  dat <-  strsplit(result$date, "/")
  dat <- do.call("rbind", dat)
  dat <- paste(dat[,3], dat[,1], dat[,2], sep = "-")
  result$date <- as.Date(dat, format = "%Y-%m-%d")

  object$id <- rownames(object)

  result <- merge(result, object, by = "id")

  names(result)[3:5] <- c("chirps","lon","lat")

  result <- result[, c("id","lon","lat","date","chirps")]

  result <- tibble::as_tibble(result)

  class(result) <- c("chirps", class(result))
  
  return(result)
}

# @rdname get_chirps
# @method get_chirps geojson
# @export
get_chirps.geojson <- function(object, dates, operation = 5, ...) {
  
  
  
}

# get_chirps.json <-  function(object, dates, operation = 5, ...) {
#   
#   
# 
# }





