#' Precipitation indices
#'
#' Compute precipitation indices over a time series.
#'
#' @param object a data.frame with CHIRPS data as privided by \code{get_chirps}
#' @param timeseries logical, FALSE for a single point time series observation or TRUE for a time series based on a \code{span}  
#' @param span integer no lower than 5, for the days intervals when \code{timeseries} = TRUE
#' @return A dataframe with selected indices:
#' \item{MLDS}{maximum length of consecutive dry day, rain <  1 mm (days)}
#' \item{MLWS}{maximum length of consecutive wet days, rain >= 1 mm (days)}
#' \item{R10mm}{number of heavy precipitation days 10 >= rain < 20 mm (days)}
#' \item{R20mm}{number of very heavy precipitation days rain >= 20 (days)}
#' \item{Rx1day}{maximum 1-day precipitation (mm)}
#' \item{Rx5day}{maximum 5-day precipitation (mm)}
#' \item{R95p}{total precipitation when rain > 95th percentile (mm)}
#' \item{R99p}{total precipitation when rain > 99th percentile (mm)}
#' \item{Rtotal}{total precipitation (mm) in wet days, rain >= 1 (mm)}
#' \item{SDII}{simple daily intensity index, total precipitation divided by the number of wet days (mm/days)}
#' @seealso \code{\link[tidyr]{pivot_wider}}
#' @references 
#' 
#' Aguilar E., et al. (2005). Journal of Geophysical Research, 110(D23), D23107. https://doi.org/10.1029/2005JD006119
#' 
#' Kehel Z., et al. (2016). In: Applied Mathematics and Omics to Assess Crop Genetic Resources for Climate Change Adaptive Traits (eds Bari A., Damania A. B., Mackay M., Dayanandan S.), pp. 151–174. CRC Press.
#' 
#' @examples
#' 
#' \donttest{
#' # Three points in the Tapajos National Forest, Brazil
#' lonlat <- data.frame(lon = c(-55.0281,-54.9857, -55.0714),
#'                      lat = c(-2.8094, -2.8756, -3.5279))
#' 
#' dates <- c("2017-12-15","2018-05-31")
#'  
#' df <- get_chirps(lonlat, dates)
#' 
#' # take the indices for the entire period 
#' precip_indices(df, timeseries = FALSE)
#' 
#' # take the indices for periods of 7 days
#' precip_indices(df, timeseries = TRUE, span = 7)
#' }       
#' @importFrom stats quantile        
#' @export
precip_indices <- function(object, timeseries = FALSE, span = NULL) {
  
  if (!.is_chirps(object)) {
    stop("object must be a data.frame with class 'chirps'\n")
  }
   
  indices <- c("MLDS","MLWS","R10mm","R20mm","Rx1day",
               "Rx5day","R95p","R99p","Rtotal","SDII")
  
  # keep unique ids in a new data.frame
  lonlat <- object[!duplicated(object$id), c("id","lon","lat")]
  
  nr <- length(unique(object$date))
  
  # it might happen that when bins are not well distributed across dates
  # the last values are dropped
  # for example, divide the periods of 7 days in a time series of 53 days
  # in that case, the last four observations are dropped to fit in a vector of
  # length == 49 (an the maximum integer from dividing days/span)
  if (timeseries) {
    
    bins <- floor(nr/span)
    
    bins <- rep(1:bins, each = span, length.out = NA)
    
  } 
  
  if (!timeseries) {
    
    bins <- min(table(object$id))
    
    bins <- rep(1, times = bins)
    
  }
  
  # force the dates to be in the right order
  object <- object[order(as.Date(object$date, format = "%Y/%m/%d")), ]
  
  # split by ids
  object <- split(object, object$id)
  
  # keep only chirps data and ids, and add bins
  object <- lapply(object, function(x) { 
    
    x <- x[,c("id", "chirps")]
    x <- x[1:length(bins), ]
    x$bin <- bins
    x
    
  })
  
  object <- do.call("rbind", object)
  
  object <- split(object, paste(object$id, bins, sep = "_"))
  
  object <- lapply(object, function(x) {
    
    chr <- x$chirps
    
    ind <- c(.dryspell(chr),
             .wetspell(chr),
             .r_ten_mm(chr),
             .r_twenty_mm(chr),
             .r_one_day(chr),
             .r_five_day(chr),
             .very_wet_days(chr),
             .extrem_wet_days(chr),
             .r_total(chr),
             .sdii(chr))
    
    ind <- data.frame(id = rep(x$id[1], length(indices)),
                      bin = rep(x$bin[1], length(indices)),
                      index = indices,
                      value = ind,
                      stringsAsFactors = FALSE)
    
  })
  
  result <- do.call("rbind", object)
  
  # merge with ids and lon lat
  result <- merge(result, lonlat, by = "id")
  
  result <- result[,c("id","bin","lon","lat","index","value")]
  
  result$id <- as.integer(result$id)
  
  result <- result[order(result$bin), ]
  
  result <- result[order(result$id), ]
  
  result <- tibble::as_tibble(result)
  
  return(result)
}



# Maximum length of consecutive dry days
# @param object numeric vector
# @return the MLDS index, which is the maximum length of consecutive dry days
# precipitation < 1 mm
# @examples
# set.seed(12)
# r <- runif(20, 0, 9)
# r[c(1,4,9:12,17)] <- 0
# .dryspell(r)
.dryspell <- function(object)
{
  # the function rle is applied
  # which looks for the sequencies of numbers
  # in this case, zeros (0)
  # take the maximum sequency
  # first all values < 1 are converted to zero (0)
  ds <- ifelse(object < 1, 0, object)
  
  # get the lengths of each sequency of zeros (0)
  keep <- rle(ds)$values
  
  keep <- keep == 0
  
  ds <- rle(ds)$lengths[keep]
  
  # if there is no value (empty()) then set as zero
  # which means there is no dry spell
  if (length(ds) == 0) {
    ds <- 0
  }
  
  # if there is values, take the maximum sequecy
  if (length(ds) != 0) {
    ds <- max(ds, na.rm = TRUE)
  }
  
  return(ds)
  
}

# Maximum length of consecutive wet days
# @param object numeric vector
# @return the MLWS index, which is the maximum length of consecutive wet days
# precipitation > 1 mm
# @examples
# set.seed(12)
# r <- runif(20, 0, 9)
# r[c(1,4,9:11)] <- 0.1
# .wetspell(r)
.wetspell <- function(object)
{
  # the function rle is applied
  # which looks for the sequencies of zeros
  # take the maximum sequency
  # first all values >= 1 are converted to zero (0)
  # no precipitation (r < 1) is converted to two (2)
  ws <- ifelse(object >= 1, 0, 2)
  
  # get the lengths of each sequency of zeros (0)
  keep <- rle(ws)$values
  
  keep <- keep == 0
  
  ws <- rle(ws)$lengths[keep]
  
  # if there is no value (empty()) then set as zero
  if (length(ws) == 0) {
    ws <- 0
  }
  # if there is values, take the maximum sequecy
  if (length(ws) != 0) {
    ws <- max(ws, na.rm = TRUE)
  }
  
  return(ws)
}

# Heavy precipitation days (10 >= r < 20 mm)
# @param object numeric vector
# @return the R10mm index, which is number of heavy precipitation days (10 >= r < 20 mm)
# @examples
# set.seed(12)
# r <- runif(20, 0, 12)
# r[c(1,4,9:11)] <- 0.1
# .r_ten_mm(r)
.r_ten_mm <- function(object) {
  
  rt <- sum(object >= 10 & object < 20, na.rm = TRUE)
  
  return(rt)

}

# Very heavy precipitation days (r >= 20)
# @param object numeric vector
# @return the R20mm index, which is number of very heavy precipitation days (r >= 20)
# @examples
# set.seed(12)
# r <- runif(20, 10, 23)
# r[c(1,4,9:11)] <- 0.1
# .r_twenty_mm(r)
.r_twenty_mm <- function(object) {
  
  rtw <- sum(object >= 20, na.rm = TRUE)
  
  return(rtw)
  
}

# Simple rainfall intensity index
# @param object numeric vector
# @return the SDII index, which is the simple daily intensity 
# index total precipitation divided by the number of wet days (r >= 1.0mm)
# @examples
# set.seed(12)
# r <- runif(20, 0, 9)
#
# r[c(1,4,9:11)] <- 0.1
#
# .sdii(r)
#
# .sdii(rep(0.1, 9))
.sdii <- function(object) {
  
  # total precipitation
  tp <- sum(object, na.rm = TRUE)
  
  # number of wet days
  wd <- length(object[object >= 1])
  
  #if both zero, then return 0
  if (wd == 0) {
    si <- 0L
  }else{
    si <- tp / wd
  }
  
  return(si)
  
}

# Compute Rx5day rainfall index
# @param object numeric vector
# @return the Rx5day index, which is the maximun sum 
# of rain in consecutive 5 days
# @examples
# set.seed(12)
# r <- runif(20, 0, 9)
# r[c(1,4,9:12,17)] <- 0
# .r_five_day(r)
.r_five_day <- function(object)
{
  # this look for the maximum sum of rain in
  # consecutive 5 days
  l <- length(object)
  
  r5day <- NULL
  
  for(i in 1:(l-4)){
    
    r5day <- cbind(r5day, sum(object[i:(i + 4)], na.rm = TRUE))
    
  }
  
  r5day <- max(r5day, na.rm = TRUE)
  
  return(r5day)
  
}

# Maximum 1-day rainfall
# @param object numeric vector
# @return the Rx1day index, which is the 1-day rainfall
# @examples
# set.seed(12)
# r <- runif(20, 0, 9)
# r[c(1,4,9:12,17)] <- 0
# .r_one_day(r)
.r_one_day <- function(object) {
  
  ro <- max(object, na.rm = TRUE)
  
  return(ro)
  
}

# Total rainfall (mm) in wet days (r >= 1)
# @param object numeric vector
# @return the Rtotal index, which is sum of rainfall (mm) in wet days (r >= 1)
# @examples
# set.seed(12)
# r <- runif(20, 0, 9)
# r[c(1,4,9:12,17)] <- 0
# .r_total(r)
.r_total <- function(object) {
  
  rt <- object[object >= 1]
  
  rt <- sum(object, na.rm = TRUE)
  
  return(rt)
  
}


# Very wet days
# @param object numeric vector
# @return the R95p index, annual total PRCP when rain > 95th percentile
# @examples
# set.seed(12)
# r <- runif(20, 0, 9)
# r[c(1,4,9:12,17)] <- 0
# .very_wet_days(r)
.very_wet_days <- function(object) {
  
  q <- stats::quantile(object, probs = seq(0, 1, 0.05), na.rm = TRUE)
  q <- q["95%"]
  
  vwd <- object[object > q]
  
  vwd <- sum(vwd, na.rm = TRUE)
  
  return(vwd)
  
}

# Very wet days
# @param object numeric vector
# @return the R95p index, annual total PRCP when rain > 95th percentile
# @examples
# set.seed(12)
# r <- runif(20, 0, 9)
# r[c(1,4,9:12,17)] <- 0
# .extrem_wet_days(r)
.extrem_wet_days <- function(object) {
  
  q <- stats::quantile(object, probs = seq(0, 1, 0.01), na.rm = TRUE)
  q <- q["99%"]
  
  vwd <- object[object > q]
  
  vwd <- sum(vwd, na.rm = TRUE)
  
  return(vwd)
  
}
