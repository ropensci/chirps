#' Get CHIRPS
#' 
#' Fetch the Climate Hazards Group InfraRed Precipitation with Station Data via 
#' ClimateSERV API Client
#' 
#' @param lonlat a data.frame of geographic coordinates of longitude and latitude in that order
#' @param dates a character of start and end dates in that order in the format YYYY-MM-DD
#' @param operation optional, an integer that represents which type of statistical operation to perform on the dataset
#' @details Types of operation supported by ClimateSERV are,
#' max = 0, min = 1, median = 2, range = 3, sum = 4, average = 5
#' @return A data frame of CHIRPS data including latlon and dates
#' @seealso \url{https://climateserv.servirglobal.net} and \url{http://chg.geog.ucsb.edu/data/chirps/} 
#' @examples 
#' lonlat <- data.frame(lon = runif(4, 5, 7),
#'                      lat = runif(4, 35, 37))
#' dates <- c("2018-10-01","2018-11-01")
#' 
#' df <- get_chirps(lonlat, dates)
#' @importFrom httr accept_json content GET
#' @importFrom jsonlite fromJSON
#' @importFrom sf st_point st_buffer
#' @importFrom tibble as_tibble tibble
#' @export
get_chirps <- function(lonlat, dates, operation = 5) {
  

  path <- "https://climateserv.servirglobal.net/chirps/submitDataRequest/?"
  
  path2 <- "https://climateserv.servirglobal.net/chirps/getDataFromRequest/?"
  
  nr <- nrow(lonlat)
  
  lonlat <- .c_polygon(lonlat)
  
  # format for ClimateSERV is ("MM/DD/YYYY")
  begindate <- dates[1]
  begindate <- strsplit(begindate, "-")[[1]]
  begindate <- paste(begindate[2], begindate[3], begindate[1], sep = "/")
  
  enddate <- dates[2]
  enddate <- strsplit(enddate, "-")[[1]]
  enddate <- paste(enddate[2], enddate[3], enddate[1], sep = "/")
  
  
  # submit data request
  ids <- 
    lapply(lonlat, function(x) {
      
      query <- list(
        datatype = "0",
        begintime =  "04/01/2018",
        endtime = "04/30/2018",
        intervaltype = "0",
        operationtype = "5",
        callback = "successCallback",
        dateType_Category = "default",
        isZip_CurrentDataType = "false",
        geometry = x
      )
      
      id <- httr::GET(url = path, 
                      query = query)
      
      id <- httr::content(id, as = "text", encoding = "UTF-8")
      
    })
  
  
  # check that all calls were successful
  success <- lapply(ids, function(x) {
    grepl("successCallback", x) 
    
  })
  
  success <- as.vector(unlist(success))
  
  if (all(success)) {
    
    cat("Fetching CHIRPS \n")
  
  }
  
  if (!all(success)) {
    
    stop("Fail to fetch one or more points, which were: ", 
         paste(which(success == FALSE), collapse = ", "), "\n")
  
  }
  
  # pick the ids 
  ids <- lapply(ids, function(x) {
    strsplit(x, '["]')[[1]][2] 
  })
  
  # get data from request
  data <- lapply(ids, function(x) {
    
    d <- httr::GET(url = path2,
                   query = list(id = x),
                   httr::accept_json())
    
    d <- httr::content(d, as = "text", encoding = "UTF-8")
    
    d <- jsonlite::fromJSON(d)
    
    d <- data.frame(date = d$data$date,
                    chirps = d$data$value$avg, 
                    stringsAsFactors = FALSE)
    
    return(d)
    
  })
  
  data <- do.call("rbind", data) 
  
  dat <-  strsplit(data$date, "/")
  dat <- do.call(rbind, dat)
  dat <- paste(dat[,3], dat[,1], dat[,2], sep = "-")
  data$date <- as.Date(dat, format = "%Y-%m-%d")
  
  data$id <- rep(1:nr, each = length(unique(dat)))
  
  data <- tibble::as_tibble(data)
  
  data <- data[c(4,1,2,3)]
  
  return(data)
}
