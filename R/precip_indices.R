#' Precipitation indices
#'
#' Compute precipitation indices over a time series.
#'
#' @param object a data.frame with CHIRPS data as privided by \code{get_chirps}
#' @param timeseries logical, should the indices return static of in time series 
#' @param span integer no lower than 5, for the days intervals when \code{timeseries} = TRUE
#' @param index character or a vector with characters for the indices to be calculated 
#' @return A dataframe with selected indices. Options are:
#' \item{MLDS}{maximum length of consecutive dry days (r <  1 mm)}
#' \item{MLWS}{maximum length of consecutive wet days (r >= 1 mm)}
#' \item{R10mm}{number of heavy precipitation days (10 >= r < 20 mm)}
#' \item{R20mm}{number of very heavy precipitation days (r >= 20) }
#' \item{SDII}{simple daily intensity index (mean of wet days / total rainfall)}
#' \item{Rx1day}{maximum 1-day rainfall (mm)}
#' \item{Rx5day}{maximum 5-day rainfall (mm) }
#' \item{Rtotal}{total rainfall (mm) in wet days (R >= 1)}
#' @seealso \code{\link[tidyr]{pivot_wider}}
#' @references 
#' 
#' Aguilar E., et al. (2005). Journal of Geophysical Research, 110(D23), D23107. \link{https://doi.org/10.1029/2005JD006119}
#' 
#' Kehel Z., et al. (2016). In: Applied Mathematics and Omics to Assess Crop Genetic Resources for Climate Change Adaptive Traits (eds Bari A., Damania A. B., Mackay M., Dayanandan S.), pp. 151–174. CRC Press.
#' 
#' @examples
#' \donttest{
#' 
#' 
#' }       
#'          
#' @export
precip_indices <- function(object, timeseries = FALSE, 
                           span = NULL, index = NULL)
{
  
  if (!.is_chirps(object)) {
    stop("object must be a data.frame of class 'chirps'\n")
  }
  
  if (is.null(index)) {
    index <- c("MLDS","MLWS","R10mm","R20mm","SDII","Rx1day","Rx5day","Rtotal")
  }
  
  # keep unique ids in a new data.frame
  lonlat <- object[!duplicated(object$id), c("id","lon","lat")]
  
  # split the object
  object <- split(object, object$date)
  
  # keep only chirps data
  object <- lapply(object, function(x) { x[,"chirps"] })
  
  # put all together in a wide data.frame
  object <- do.call("cbind", object)
  
  nr <- nrow(object)
  nc <- ncol(object)
  
  if (timeseries) {
    
    bins <- floor(nc/span)
    
    bins <- rep(1:bins, each = span, length.out = NA)
    
  } 
  
  if (!timeseries) {
    
    bins <- 1:nc
  
  }
  
  
  ind <- tibble::as_tibble(matrix(nrow = nr, 
                                  ncol = length(index), 
                                  dimnames = list(1:nr, index)))
  
  # maximum length of consecutive dry days (< 1 mm)
  if ("MLDS" %in% index) {
    ind["MLDS"]   <- .dryspell(object)
  }
  # maximum length of consecutive wet days
  if ("MLWS" %in% index) {
    ind["MLWS"]   <- .wetspell(object)
  }
  # days with rainfall between 10-15 mm
  if ("R10mm" %in% index) {
    ind["R10mm"]  <- apply(object, 1, function(x) {
      sum(x >= 10 & x < 20, na.rm = TRUE)
    })
  }
  # days with rainfall > 20 mm
  if ("R20mm" %in% index) {
    ind["R20mm"]  <- apply(object, 1, function(x) {
      sum(x >= 20, na.rm = TRUE)
    })
  }
  # simple rainfall intensity index
  if ("SDII" %in% index) {
    ind["SDII"] <- apply(object, 1, function(x) {
      sum(x, na.rm = TRUE) / sum(x >= 1, na.rm = TRUE)
    })
  }
  # maximum 1-day rainfall
  if ("Rx1day" %in% index) {
    ind["Rx1day"] <- apply(object, 1, function(x) {
      max(x, na.rm = TRUE)
    })
  }
  # maximum consecutive 5-day precipitation
  if ("Rx5day" %in% index) {
    ind["Rx5day"] <- .rx5day(object)
  }
  # total rainfall (mm) in wet days (r >= 1)
  if ("Rtotal" %in% index) {
    ind["Rtotal"] <- apply(object, 1, function(x) {
      sum(x[as.vector(x >= 1)], na.rm = TRUE)
    })
  }
  
  ind[is.na(ind)] <- 0
  
  # reshape the data.frame
  result <- split(ind, 1:nr)
  
  result <- lapply(result, function(r) {
    r <- t(r)
    r <- data.frame(index = index, 
                    value = r, 
                    stringsAsFactors = FALSE)
        
  })
  
  result <- do.call("rbind", result)
  
  # fix ids
  id <- strsplit(row.names(result), "[.]")
  id <- do.call("rbind", id)[,1]
  result$id <- id
  
  # merge with ids and lon lat
  result <- merge(result, lonlat, by = "id")
  
  result <- result[,c("id","lon","lat","index","value")]
  
  result <- tibble::as_tibble(result)
  
  return(result)
}


# compute Rx5day rainfall index
.rx5day <- function(object)
{
  # this look for the maximum sum of rain in
  # consecutive 5 days
  l <- ncol(object)
  Y <- apply(object, 1, function(Y){
    r5day <- NULL
    for(i in 1:(l-4)){
      r5day <- cbind(r5day, sum(Y[i:(i+4)], na.rm = TRUE))}
    return(max(r5day, na.rm = TRUE))
  })
  return(Y)
}

# compute the maximum length of consecutive dry days
.dryspell <- function(object)
{
  # the function rle is applied
  # which looks for the sequencies of numbers
  # in this case, zeros (0)
  # take the maximum sequency
  f <- apply(object, 1, function(Y){
    # first all values < 1 are converted to zero (0)
    Y <- ifelse(Y < 1, 0, Y)
    # get the lengths of each sequency of zeros (0)
    keep <- rle(Y)$values
    keep <- keep == 0
    Y <- rle(Y)$lengths[keep]
    # if there is no value (empty()) then set as zero
    if (length(Y) == 0) {
      Y <- 0
    }
    # if there is values, take the maximum sequecy
    if (length(Y) != 0) {
      Y <- max(Y, na.rm = TRUE)
    }
    return(Y)
  }  )
  return(f)
}

# compute the maximum length of consecutive wet days
.wetspell <- function(object)
{
  # the function rle is applied
  # which looks for the sequencies of zeros
  # take the maximum sequency
  f <- apply(object, 1, function(Y){
    # first all values >= 1 are converted to zero (0)
    # no precipitation (r < 1) is converted to two (2)
    Y <- ifelse(Y >= 1, 0, 2)
    # get the lengths of each sequency of zeros (0)
    keep <- rle(Y)$values
    keep <- keep == 0
    Y <- rle(Y)$lengths[keep]
    # if there is no value (empty()) then set as zero
    if (length(Y) == 0) {
      Y <- 0
    }
    # if there is values, take the maximum sequecy
    if (length(Y) != 0) {
      Y <- max(Y, na.rm = TRUE)
    }
    return(Y)
  }  )
  return(f)
}
