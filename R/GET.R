#' General function to get data from ClimateSERV API
#'
#' @param object a list with geojson strings
#' @param dates a character of start and end dates in that order in the format MM/DD/YYYY
#' @param operation an integer that represents which type of statistical operation to perform on the dataset
#' @param datatype an integer, the unique datatype number for the dataset which this request operates on
#' @return A data.frame with values
#' @details 
#' operation: supported operations are max = 0, min = 1, median = 2, sum = 4, average = 5
#' 
#' datatype: supported datatypes are Global CHIRPS = 0, Global ESI 4 Week = 29
#' 
#' @examples
#' # random geographic locations around bbox(10, 12, 45, 57)
#' set.seed(123)
#' lonlat <- data.frame(lon = runif(3, 10, 12),
#'                      lat = runif(3, 45, 57))
#' 
#' dates <- c("12/01/2017", "01/31/2018")
#' 
#' gjson <- chirps:::.dataframe_to_geojson(lonlat)
#' 
#' operation <- 5
#' 
#' datatype <- 0
#'@noRd
.GET <- function(gjson, dates, operation = NULL, datatype = NULL) {
  
  begindate <- dates[1]
  enddate <- dates[2]
  
  # submit data request and get ids
  ids <- lapply(gjson, function(x) {
    
    i <- .send_request(
      datatype = datatype,
      begintime = begindate,
      endtime = enddate,
      intervaltype = 0,
      operationtype = operation,
      geometry = x
    )
    
    return(i)
    
  })
  
  # check request progress and wait 
  # until the request is done by the server
  request_progress <- rep(FALSE, length(ids))
  
  while (!all(request_progress)) {
    
    request_progress <- lapply(ids, function(x) {
      
      p <- .get_request_progress(x)
      
    })
    
    request_progress <- unlist(request_progress)
    
    cat("Getting your request... patience you must have my young padawan\n")
    
  }
  
  # get data from request
  result <- lapply(ids, function(x) {
    
    d <- .get_data_from_request(id = x)
    
    return(d)
    
  })
  
  
  result <- do.call("rbind", result)
  
  # fix ids
  id <- strsplit(row.names(result), "[.]")
  id <- do.call("rbind", id)[, 1]
  result$id <- id
  
  # transform dates to the original format as input
  dat <-  strsplit(result$date, "/")
  dat <- do.call("rbind", dat)
  dat <- paste(dat[, 3], dat[, 1], dat[, 2], sep = "-")
  result$date <- as.Date(dat, format = "%Y-%m-%d")
  
  names(result) <- c("date", "value", "id")
  
  rownames(result) <- 1:nrow(result)
  
  result <- result[, c("id", "date", "value")]
  
  return(result)
}
