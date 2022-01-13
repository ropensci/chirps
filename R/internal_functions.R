#' Send a request to ClimateSERV
#'
#' @param datatype integer, the unique datatype number for the 
#'  dataset which this request operates on
#' @param begintime character, start date for processing interval, 
#'  in the format ("MM/DD/YYYY")
#' @param endtime character, end date for processing interval, format
#'  ("MM/DD/YYYY")
#' @param intervaltype integer, value that represents which type of time
#'  interval to process
#' @param operationtype integer, value that represents which type of 
#'  statistical operation to perform. Defaults to \code{5}.
#' @param geometry a geojson for the geometry that is defined by the user 
#'  on the current client
#' @return A id to be used in the data request
#' @details
#' datatype codes are described at 
#'  https://climateserv.servirglobal.net/help
#' 
#' operation: supported operations are max = 0, min = 1, 
#'  median = 2, sum = 4, average = 5
#' @examples
#' lonlat <- data.frame(lon = c(-60.34),
#'                      lat = c(-5.38))
#' 
#' gjson <- as.geojson(lonlat)
#' 
#' chirps:::.send_request(begintime = "12/10/2018",
#'                        endtime = "12/26/2018",
#'                        geometry = gjson)
#'                         
#' @importFrom httr RETRY accept_json content
#' @noRd
.send_request <- function(datatype = 0,
                          begintime = NULL,
                          endtime = NULL,
                          intervaltype = 0,
                          operationtype = 5,
                          geometry = NULL) {
  
    #base_url <- "https://climateserv.servirglobal.net/api/"
    
    # organise the query
    query <- list(
      datatype = toString(datatype),
      begintime = begintime,
      endtime = endtime,
      intervaltype = toString(intervaltype),
      operationtype = toString(operationtype),
      callback = "successCallback",
      dateType_Category = "default",
      isZip_CurrentDataType = "false",
      geometry = toString(geometry)
    )
    
    # client_request <-
    #   crul::HttpClient$new(url = paste0(base_url, "submitDataRequest/?"))
    
    # check status
    # status <- client_request$get()
    # status$raise_for_status()
    
    # send the query
    tryCatch({
      # id <- client_request$get(query = query),
      # nocov start
      id <- httr::RETRY(verb = "GET", 
                        url = "https://climateserv.servirglobal.net/api/submitDataRequest/?",
                        query = query,
                        httr::accept_json(), 
                        terminate_on = c(403, 404))
      
      id <- httr::content(id, as = "text", encoding = "UTF-8")
      
      }, 
      
      error = function(e) {
        e$message <-
          paste0(
            "Something went wrong with the query, no data were returned. ",
            "Most likely the server is down. Please see ",
            "<https://climateserv.servirglobal.net/> for potential ",
            "server issues.\n"
          )
        # Otherwise refers to open.connection
        e$call <- NULL
        stop(e)
      }
    ) # nocov end
    
    # id <- id$parse("UTF-8")
    
    # get content from the query
    id <- strsplit(id, '["]')[[1]][2]
    
    return(id)
    
}

#' Get request progress
#'
#' @param id character with the id obtained from \code{.send_request}
#' @return logical value, \code{TRUE} when the data is ready to be retrieved
#' @examples
#' 
#' lonlat <- data.frame(lon = runif(1, 10, 12),
#'                      lat = runif(1, 45, 47))
#' 
#' gjson <- as.geojson(lonlat)
#' 
#' gjson <- as.character(gjson)
#' 
#' gsjon <- split(gjson, seq_along(gjson))
#' 
#' id <- chirps:::.send_request(begintime = "12/10/2018",
#'                              endtime = "12/26/2018",
#'                              geometry = gjson)
#' 
#' chirps:::.get_request_progress(id)
#' @noRd
.get_request_progress <- function(id) {
  
  # base_url <- "https://climateserv.servirglobal.net/api/"
  # 
  # client_progress <-
  #   crul::HttpClient$new(url = paste0(base_url, 
  #                                     "getDataRequestProgress/?"))
  # 
  # 
  # # organise the query
  progress_query <- list(id = id)
  # 
  # # check status
  # status <- client_progress$get()
  # 
  # # send query
  # client_progress$get(query = progress_query, retry = 6)
  # 
  # p <- client_progress$get(query = progress_query)
  # 
  # p <- p$parse("UTF-8")
  # 
  # account for the chance that the server returns 
  # an error message and stop
  # the process with a (hopefully) useful error message
  
  p <- httr::RETRY(verb = "GET", 
                   url = "https://climateserv.servirglobal.net/api/getDataRequestProgress/?",
                   query = progress_query,
                   httr::accept_json(), 
                   terminate_on = c(403, 404))
  
  # nocov start
  p <- httr::content(p, as = "text", encoding = "UTF-8")
  
  if (p == -1) { #nocov start
    stop(call. = FALSE,
    "Something went wrong with the query, no data were returned. ",
    "Most likely the server is down. Please see ",
    "<https://climateserv.servirglobal.net/> for potential ",
    "server issues.\n"
    )
  } #nocov end
  
  p <- grepl(100, p)
  
  return(p)
}

#' Get data from a request to ClimateSERV
#'
#' @param id character with the id obtained from \code{.send_request}
#' @return A data frame with requested data
#' @examples
#' lonlat <- data.frame(lon = runif(1, 10, 12),
#'                      lat = runif(1, 45, 47))
#' 
#' gjson <- as.geojson(lonlat)
#' 
#' gjson <- as.character(gjson)
#' 
#' gsjon <- split(gjson, seq_along(gjson))
#' 
#' id <- chirps:::.send_request(begintime = "12/10/2018",
#'                              endtime = "12/26/2018",
#'                              geometry = gjson)
#' 
#' chirps:::.get_data_from_request(id)
#' 
#' @importFrom jsonlite fromJSON toJSON
#' @noRd
.get_data_from_request <- function(id) {
  
  # base_url <- "https://climateserv.servirglobal.net/api/"
  # 
  # client_data <-
  #   crul::HttpClient$new(url = paste0(base_url, "getDataFromRequest/?"))
  # 
  # organise the query
  query <- list(id = id)
  
  # # check status
  # status <- client_data$get()
  # status$raise_for_status()
  # 
  
  tryCatch({
    # client_data$get(query = query, retry = 6)
    # 
    # # send query
    # d <- client_data$get(query = query)
    # 
    # d <- d$parse("UTF-8")
    
    d <- httr::RETRY(verb = "GET", 
                      url = "https://climateserv.servirglobal.net/api/getDataFromRequest/?",
                      query = query,
                      httr::accept_json(), 
                      terminate_on = c(403, 404))
    
    d <- httr::content(d, as = "text", encoding = "UTF-8")
    
    
    d <- jsonlite::fromJSON(d)
    
  }, # nocov start
  error = function(e) {
    e$message <-
      paste0(
        "Something went wrong with the query, no data were returned. ",
        "Most likely the server is down. Please see ",
        "<https://climateserv.servirglobal.net/> for potential ",
        "server issues.\n"
      )
    # Otherwise refers to open.connection
    e$call <- NULL
    stop(e)
  }) # nocov end
  
  d <- data.frame(cbind(date = d$data$date,
                        d$data$value))
  
  d$date <- as.character(d$date)
  
  
  return(d)
  
}

#' Validate lonlat within an pre-defined bounding box
#'
#' @param lonlat a \code{\link[base]{data.frame}} with geographical
#'  coordinates lonlat in that order
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
  lon <- lonlat[, 1]
  
  lat <- lonlat[, 2]
  
  v1 <- min(lon) < xlim[1]
  v2 <- max(lon) > xlim[2]
  v3 <- min(lat) < ylim[1]
  v4 <- max(lat) > ylim[2]
  
  if (any(c(v1, v2, v3, v4))) {
    stop(
      "Subscript out of bounds. \n lonlat are beyond the accepted bbox,
      which are: ",
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
#'  "YYYY-MM-DD"
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
#' @param x a character of start and end dates in that order in the format
#'  "YYYY-MM-DD"
#' @param ... further arguments passed to \code{\link{.validate_dates}}
#' @return a character with reformated dates as "MM/DD/YYYY"
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
  begindate <-
    paste(begindate[2], begindate[3], begindate[1], sep = "/")
  
  enddate <- x[2]
  enddate <- strsplit(enddate, "-")[[1]]
  enddate <- paste(enddate[2], enddate[3], enddate[1], sep = "/")
  
  dates <- c(begindate, enddate)
  
  return(dates)
  
}


#' Check if contains class "chirps"
#' @param x an object to test
#' @return logical, \code{TRUE} for an object of class 'chirps'
#' @examples
#' .is_chirps(airquality)
#' @noRd
.is_chirps <- function(x) {
  c("chirps") %in% class(x)
  
}


#' Add feature properties to a geojson geometry 
#' 
#' @param geometry a geojson geometry
#' @param properties a data.frame with feature properties
#' @param name a characer for the feature properties name
#' @return a object of class geojson with FeatureCollection
#' @examples
#' set.seed(123)
#' lonlat <- data.frame(lon = 1,
#'                      lat = 1)
#' 
#' geometry <- as.geojson(lonlat)[[1]]
#' 
#' properties <- data.frame(x = LETTERS[1:3],
#'                          a = as.character(1:3),
#'                          z = colors()[1:3])
#' 
#' name <- "chirps"
#' 
#' gjson <- .add_geojson_properties(geometry, properties, name)
#' 
#' gjson
#' 
#' # check if it can be parsed to sf
#' sf::st_read(gjson)
#' 
#' # check if it can be parsed to jsonlite
#' jsonlite::fromJSON(gjson)
#' 
#' @noRd
.add_geojson_properties <- function(geometry, properties, name) {
  
  # extract the geometry
  g <- geometry
  
  g <- jsonlite::fromJSON(g)
  
  # coerce coordinates to character to prevent toJSON to 
  # suppress floating numbers
  g$geometry$coordinates <- as.character(g$geometry$coordinates)
  
  # coerce the geometries back to json
  g <- jsonlite::toJSON(g$geometry)
  
  # now convert the properties into json
  p <- properties
  
  nr <- dim(p)[[1]]
  
  nc <- dim(p)[[2]]
  
  # coerce values into characters
  p[seq_len(nc)] <- lapply(p[seq_len(nc)], as.character)
  
  p <- split(p, seq_len(nr))
  
  p <- jsonlite::toJSON(p)
  
  p <- gsub("[[]", "", p)
  
  p <- gsub("[]]", "", p)
  
  header <- paste0("{\"type\":\"FeatureCollection\",\"name\":\"",
                   name,
                   "\",\"features\":[{\"type\":\"Feature\",\"properties\":")
  
  header2 <- ",\"geometry\":"
  
  end <- "}]}"
  
  gjson <- paste0(header, p, header2, g, end)
  
  class(gjson) <- c("geojson", "json")
  
  return(gjson)
  
}
