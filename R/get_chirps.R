#' Get CHIRPS precipitation data
#' 
#' Fetch the Climate Hazards Group InfraRed Precipitation with Station Data via 
#' ClimateSERV API Client. ClimateSERV works with geojson strings of type 'Polygon'. The 
#' input 'lonlat' are then transformed into polygons with a small buffer area around 
#' the point.
#' 
#' @param lonlat a data.frame of geographic coordinates of longitude and latitude in that order
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
#' @importFrom httr accept_json content GET
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @export
get_chirps <- function(lonlat, dates, operation = 5, ...) {
  

  # the first path is the request the data
  # a successful request returns an id from which is possible to download the data
  path <- "https://climateserv.servirglobal.net/chirps/submitDataRequest/?"
  
  # the second path is to download the data from the previous request
  # a successful returns a json file which can be transformed into a data.frame 
  path2 <- "https://climateserv.servirglobal.net/chirps/getDataFromRequest/?"
  
  nr <- nrow(lonlat)
  
  # validate lonlat to check if they are within the CHIRPS range lat -50, 50
  .validate_lonlat(lonlat)
  
  # validate dates
  .validate_dates(dates)
  
  # get geojson strings from lonlat
  gjson <- .c_geojson(lonlat, ...)
  
  # reformat dates as required in ClimateSERV ("MM/DD/YYYY")
  begindate <- dates[1]
  begindate <- strsplit(begindate, "-")[[1]]
  begindate <- paste(begindate[2], begindate[3], begindate[1], sep = "/")
  
  enddate <- dates[2]
  enddate <- strsplit(enddate, "-")[[1]]
  enddate <- paste(enddate[2], enddate[3], enddate[1], sep = "/")
  
  # force a string in operation integer
  operation <- toString(operation)
  
  # submit data request
  ids <- lapply(gjson, function(x) {
    
    # organise the query
    query <- list(
      datatype = "0",
      begintime =  begindate,
      endtime = enddate,
      intervaltype = "0",
      operationtype = operation,
      callback = "successCallback",
      dateType_Category = "default",
      isZip_CurrentDataType = "false",
      geometry = x
    )
    
    # sent the query
    id <- httr::GET(url = path, 
                    query = query)
    
    # get content from the query
    id <- httr::content(id, as = "text", encoding = "UTF-8")
    
  })
  
  # check that all calls were successful
  success <- lapply(ids, function(x) {
    grepl("successCallback", x) 
    
  })
  
  success <- as.vector(unlist(success))
  
  if (!all(success)) {
    
    stop("Fail to fetch one or more points, which were: ", 
         paste(which(success == FALSE), collapse = ", "), "\n")
  
  }
  
  # pick the ids 
  ids <- lapply(ids, function(i) {
    i <- strsplit(i, '["]')[[1]][2] 
    return(i)
  })
  
  # get data from request
  result <- lapply(ids, function(x) {

    d <- httr::GET(url = path2,
                   query = list(id = x))

    d <- httr::content(d, as = "text", encoding = "UTF-8")

    d <- jsonlite::fromJSON(d)

    d <- data.frame(cbind(date = d$data$date,
                          d$data$value))

    d$date <- as.character(d$date)

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
  
  lonlat$id <- rownames(lonlat)
  
  result <- merge(result, lonlat, by = "id")
  
  names(result)[3:5] <- c("chirps","lon","lat")
  
  result <- result[, c("id","lon","lat","date","chirps")]
  
  result <- tibble::as_tibble(result)
  
  class(result) <- c("chirps", class(result))
  
  return(result)
}

