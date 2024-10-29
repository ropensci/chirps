#' General function to get data from ClimateSERV API
#'
#' @param gjson a list with geojson strings
#' @param dates a character of start and end dates in that order in
#' the format MM/DD/YYYY
#' @param operation an integer that represents which type of statistical
#'  operation to perform on the dataset
#' @param datatype an integer, the unique \code{datatype} number for the dataset
#' which this request operates on
#' @return A \code{data.frame} with values
#' @details
#' operation: supported operations are max = 0, min = 1, median = 2,
#'  sum = 4, average = 5
#'
#' datatype: supported datatypes are Global CHIRPS = 0,
#'  Global ESI 4 Week = 29
#'  datatype codes are described at
#'  <https://climateserv.readthedocs.io/en/latest/api.html>
#'
#' @examples
#' example("tapajos", package = "chirps")
#'
#' dates <- c("05/01/2017", "01/31/2018")
#'
#' operation <- 5
#'
#' datatype <- 29
#'
#' chirps:::.GET(gjson, dates, operation, datatype)
#'
#'@noRd
.GET <- function(gjson,
                 dates,
                 operation = NULL,
                 datatype = NULL) {
  message("\nFetching data from ClimateSERV \n")
  
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
  request_progress <- seq_along(ids) == FALSE
  
  nids <- max(seq_along(ids))
  
  message("\nGetting your request...\n")
  
  while (isFALSE(all(request_progress))) {
    request_progress <- lapply(ids, function(x) {
      p <- .get_request_progress(x)
      
    })
    
    request_progress <- unlist(request_progress)
    
  }
  
  # get data from request
  result <- lapply(ids, function(x) {
    d <- .get_data_from_request(id = x)
    
    return(d)
    
  })
  
  # define ids
  ids <- NULL
  for (i in seq_along(result)) {
    nr <- dim(result[[i]])[[1]]
    
    ids <- c(ids, rep(i, nr))
    
  }
  
  result <- do.call("rbind", result)
  
  nr <- dim(result)[[1]]
  
  if (nr == 0) {
    stop("Failed to get valid values,
         try to increase the buffer area with 'dist' \n",
         call. = FALSE)
  }
  
  # add ids
  result$id <- ids
  
  # transform dates to the original format as input
  dat <- strsplit(result$date, "/")
  dat <- do.call("rbind", dat)
  dat <- paste(dat[, 3], dat[, 1], dat[, 2], sep = "-")
  result$date <- as.Date(dat, format = "%Y-%m-%d")
  
  names(result) <- c("date", "value", "id")
  
  rownames(result) <- seq_len(nr)
  
  result <- result[, c("id", "date", "value")]
  
  result <- result[order(result$date),]
  
  result <- result[order(result$id),]
  
  result[result == -9999] <- NA
  
  class(result) <- union("chirps_df", class(result))
  
  return(result)
  
}
