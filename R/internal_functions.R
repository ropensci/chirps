#' Concatenate a coordinate point data.frame into a geojson polygon
#'
#' Take single points from geographical coordinates 
#' and convert it into a geojson 'Polygon' string using
#' \code{\link{sf::st_buffer}} 'Polygon' is the only geojson format accepted by
#' ClimateSERV
#'
#' @param lonlat a data.frame with geographical coordinates lonlat in that order
#' @param dist numeric; buffer distance for all \code{lonlat}
#' @param nQuadSegs integer; number of segments per quadrant 
#' @return A list with geojson strings for each row in \code{lonlat}
#' @examples
#' # random geographic locations around bbox(10, 12, 45, 57)
#' set.seed(123)
#' lonlat <- data.frame(lon = runif(10, 10, 12),
#'                      lat = runif(10, 45, 57))
#' 
#' gjson <- .dataframe_to_geojson(lonlat)
#'@noRd
 
.dataframe_to_geojson <- function(lonlat, dist = 0.00001, nQuadSegs = 2L) {
  
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
  
  # set the buffer around the points
  lonlatb <- sf::st_buffer(lonlat, 
                           dist = dist, 
                           nQuadSegs = nQuadSegs)
  
  # transform into a sf object
  lonlatb <- sf::st_as_sf(lonlatb)
  
  # write the geojson string
  tf <- tempfile(fileext = ".geojson")
  sf::st_write(lonlatb, tf, quiet = TRUE)
  
  # capture these strings
  gj <- readLines(tf)
  
  # first 4 lines are for the features and last 2 lines to close features
  # keep only geojson geometries
  gj <- gj[5:(n + 4)]
  
  gj <- split(gj, 1:n)
  
  gjson <- lapply(gj, function(x) {
    gsub(" ", "", x)
  })
  
  return(gjson)
  
}


# Set output from ClimateServ data request
.set_output <- function(x, ids) {
  
  result <- do.call("rbind", x) 
  
  # fix ids
  id <- strsplit(row.names(result), "[.]")
  id <- do.call("rbind", id)[,1]
  result$id <- id
  
  # transform dates to the original format as input
  dat <-  strsplit(result$date, "/")
  dat <- do.call("rbind", dat)
  dat <- paste(dat[,3], dat[,1], dat[,2], sep = "-")
  result$date <- as.Date(dat, format = "%Y-%m-%d")
  
  lonlat$id <- rownames(lonlat)
  
  result <- merge(result, lonlat, by = "id")
  
  names(result)[3:5] <- c("chirps","lon","lat")
  
  result <- result[, c("id","lon","lat","date","chirps")]
  
  result <- tibble::as_tibble(result)
}

#' Send a request to ClimateSERV
#'
#' @param datatype integer, the unique datatype number for the dataset which this request operates on
#' @param begintime character, start date for processing interval, format ("MM/DD/YYYY")
#' @param endtime character, end date for processing interval, format ("MM/DD/YYYY")
#' @param intervaltype integer, value that represents which type of time interval to process
#' @param operationtype integer, value that represents which type of statistical operation to perform
#' @param geometry a geojson for the geometry that is defined by the user on the current client
#' @return A id to be used in the data request
#' @details
#' datatype codes are described at https://climateserv.readthedocs.io/en/latest/api.html
#' operation: supported operations are max = 0, min = 1, median = 2, sum = 4, average = 5
#' @noRd
.send_request <-
  function(datatype = 0,
           begintime = NULL,
           endtime = NULL,
           intervaltype = 0,
           operationtype = 5,
           geometry = NULL) {
    base_url <- "https://climateserv.servirglobal.net/chirps/"
    
    # organise the query
    query <- list(
      datatype = toString(datatype),
      begintime = begintime,
      endtime = endtime,
      intervaltype = toString(intervaltype),
      operationtype = toString(operationtype),
      dateType_Category = "default",
      isZip_CurrentDataType = "false",
      geometry = geometry
    )
    
    # screate a new client and send the query
    client_request <-
      crul::HttpClient$new(url = paste0(base_url, "submitDataRequest/?"))
    
    # check status
    status <- client_request$get()
    status$raise_for_status() # nocov end
    
    # send the query
    id <- client_request$get(query = query)
    id <- id$parse("UTF-8")
    
    # clean up ID
    id <- gsub('\\[\\"', "", id)
    id <- gsub('\\"]', '', id)

    Sys.sleep(20)
    
    # create a new client to fetch the goods
    client_data <- 
      crul::HttpClient$new(url = paste0(base_url,  "getDataFromRequest/?"))
    
    x <- list(id = id)
    
    d <- client_data$get(query = x)
    
    d <- jsonlite::fromJSON(d$parse("UTF-8"))
    
    d <- data.frame(cbind(date = d$data$date,
                          d$data$value))
    
    d$date <- as.character(d$date)
    return(d)
  }


#' Validate lonlat within an pre-defined bounding box
#' 
#' @param lonlat a data.frame with geographical coordinates lonlat in that order
#' @param xlim a numeric vector for the min and max accepted range in the
#'  longitude in that order
#' @param ylim a numeric vector for the min and max accepted range in the
#'  latitude in that order 
#' @return If lonlat are valid (within the bounding box) returns nothing
#' @examples
#' # random geographic locations around bbox(10, 12, 45, 57)
#' set.seed(123)
#' lonlat <- data.frame(lon = runif(10, 10, 12),
#'                      lat = runif(10, 45, 49))
#' 
#' .validate_lonlat(lonlat)
#' @noRd

.validate_lonlat <- function(lonlat, 
                             xlim = c(-180, 180), 
                             ylim = c(-50, 50)) {
  
  lon <- lonlat[,1]
  
  lat <- lonlat[,2]
  
  v1 <- min(lon) < xlim[1]
  v2 <- max(lon) > xlim[2]
  v3 <- min(lat) < ylim[1]
  v4 <- max(lat) > ylim[2]
  
  if (any(c(v1, v2, v3, v4))) {
    stop(
      "Subscript out of bounds. \n lonlat are beyond the accepted bbox, which are: ",
      paste(
        paste(xlim[1], ylim[1], sep = " , "),
        paste(xlim[2], ylim[2], sep = " , "),
        sep = " , "
      ),
      call. = FALSE
    )
  }
}


#' Validate dates within an accepted range
#'
#' @param x a character of start and end dates in that order in the format
#'  YYYY-MM-DD
#' @param availability a character for the dates the dataset is available
#' @return nothing
#' @examples
#' dates <- c("2016-01-31","2017-12-01")
#' 
#' .validate_dates(dates)
#' 
#' dates <- c("2018-01-31","2017-12-01")
#' 
#' .validate_dates(dates)
#' 
#' dates <- c("2018-01-31", as.character(Sys.Date()))
#' 
#' .validate_dates(dates)
#' 
#' dates <- c("1980-12-31", "2018-01-31")
#' 
#' .validate_dates(dates)
#' @noRd

.validate_dates <- function(x, availability = c("1981-01-01", "0")) {
  
  xmin <- as.Date(x[1], format = "%Y-%m-%d")
  
  xmax <- as.Date(x[2], format = "%Y-%m-%d")
  
  # the first day from which the dataset is available
  past <- as.Date(availability[1], origin = "1970-01-01")
  
  # the most recent date from which the dataset is available
  present <- availability[2]
  
  # generally it takes 45 days to update
  if (present == "0") {
    present <- Sys.Date() - 45
    present <- format(present,  "%Y-%m-%d")
  }
  
  # last given date should be higher than first
  cond1 <- as.integer(xmax - xmin) > 1
  
  # no older than past date
  cond2 <- xmin > past
  
  # no later then present date
  cond3 <- xmax < present
  
  if (!all(cond1, cond2, cond3)) {
    stop(
      "Subscript out of bounds\n
         Please check your dates. The dataset is available from ",
      as.character(past),
      " to about ",
      as.character(present),
      "\n
         Or your dates may be twisted. \n",
      call. = FALSE
    )
  }
  
}


#' Reformat dates as required by ClimateServ
#'
#' @param x a character of start and end dates in that order in the format YYYY-MM-DD
#' @param ... further arguments passed to .validate_dates
#' @return a character with reformated dates as MM/DD/YYYY
#' @examples
#' x <- c("2016-01-31","2017-12-01")
#' 
#' .reformat_dates(x)
#' @noRd

.reformat_dates <- function(x, ...) {
  
  # validate dates
  .validate_dates(x, ...)

  begindate <- x[1]
  begindate <- strsplit(begindate, "-")[[1]]
  begindate <- paste(begindate[2], begindate[3], begindate[1], sep = "/")
  
  enddate <- x[2]
  enddate <- strsplit(enddate, "-")[[1]]
  enddate <- paste(enddate[2], enddate[3], enddate[1], sep = "/")
  
  dates <- c(begindate, enddate)
  
  return(dates)
  
}


#' Check if contains class "chirps"
#' @param x an object to test
#' @return logical, TRUE for an object of class 'chirps'
#' @examples
#' .is_chirps(airquality)
#' @noRd

.is_chirps <- function(x) {
  
  c("chirps") %in% class(x)
  
}
