#' Compute precipitation indices over a time series
#'
#' @param object an object of class \code{chirps} as provided by
#'  \code{\link{get_chirps}}
#' @param timeseries logical, \code{FALSE} for a single point time series
#'  observation or \code{TRUE} for a time series based on \var{intervals}
#' @param intervals integer no lower than 5, for the days intervals when
#'  \var{timeseries} = \code{TRUE}
#' @return A data frame with precipitation indices:
#' \item{MLDS}{maximum length of consecutive dry day, rain < 1 mm (days)}
#' \item{MLWS}{maximum length of consecutive wet days, rain >= 1 mm (days)}
#' \item{R10mm}{number of heavy precipitation days 10 >= rain < 20 mm (days)}
#' \item{R20mm}{number of very heavy precipitation days rain >= 20 (days)}
#' \item{Rx1day}{maximum 1-day precipitation (mm)}
#' \item{Rx5day}{maximum 5-day precipitation (mm)}
#' \item{R95p}{total precipitation when rain > 95th percentile (mm)}
#' \item{R99p}{total precipitation when rain > 99th percentile (mm)}
#' \item{Rtotal}{total precipitation (mm) in wet days, rain >= 1 (mm)}
#' \item{SDII}{simple daily intensity index, total precipitation divided by the
#'  number of wet days (mm/days)}
#' @references 
#' 
#' Aguilar E., et al. (2005). Journal of Geophysical Research, 110(D23), D23107.
#' 
#' Kehel Z., et al. (2016). In: Applied Mathematics and Omics to Assess Crop
#'  Genetic Resources for Climate Change Adaptive Traits (eds Bari A., Damania
#'  A. B., Mackay M., Dayanandan S.), pp. 151–174. CRC Press.
#' 
#' @examplesIf interactive()
#' lonlat <- data.frame(lon = c(-55.0281,-54.9857),
#'                      lat = c(-2.8094, -2.8756))
#' 
#' dates <- c("2017-12-15", "2017-12-31")
#' 
#' dt <- get_chirps(lonlat, dates, server = "ClimateSERV")
#' 
#' # take the indices for the entire period
#' precip_indices(dt, timeseries = FALSE)
#' 
#' # take the indices for periods of 7 days
#' precip_indices(dt, timeseries = TRUE, intervals = 7)
#'
#' @importFrom stats quantile
#' @export
precip_indices <- function(object, timeseries = FALSE, intervals = NULL) {
  
  if (isFALSE(.is_chirps(object))) {
    stop("object must be a data.frame with class 'chirps'\n")
  }
  
  indices <- c("MLDS","MLWS","R10mm","R20mm","Rx1day",
               "Rx5day","R95p","R99p","Rtotal","SDII")
  
  # keep unique ids in a new data.frame
  lonlat <- object[!duplicated(object$id), c("id","lon","lat")]
  
  nr <- length(unique(object$date))
  
  names(object)[names(object) == "chirps"] <- "value"
  names(object)[names(object) == "imerg"] <- "value"
  
  # it might happen that when bins are not well distributed across dates
  # in that case the last values are dropped
  # for example, divide the periods of 7 days in a time series of 53 days
  # in that case, the last four observations are dropped to fit in a vector of
  # length == 49 (the maximum integer from dividing days/intervals)
  if (isTRUE(timeseries)) {
    
    bins <- floor(nr / intervals)
    
    bins <- rep(1:bins, each = intervals, length.out = NA)
    
  } 
  
  if (isFALSE(timeseries)) {
    
    bins <- min(table(object$id))
    
    bins <- rep(1, times = bins)
    
  }
  
  # force the dates to be in the right order
  object <- object[order(as.Date(object$date, format = "%Y/%m/%d")), ]
  
  # split by ids
  object <- split(object, object$id)
  
  # keep only rain data and ids, and add bins
  object <- lapply(object, function(x) { 
    
    x <- x[,c("id", "date", "value")]
    x <- x[seq_along(bins), ]
    x$bin <- bins
    x
    
  })
  
  object <- do.call("rbind", object)
  
  object <- split(object, paste(object$id, bins, sep = "_"))
  
  object <- lapply(object, function(x) {
    
    chr <- x$value
    
    ind <- c(.dryspell(chr),
             .wetspell(chr),
             .r_ten_mm(chr),
             .r_twenty_mm(chr),
             .r_one_day(chr),
             .r_five_day(chr),
             .very_wet_days(chr),
             .extreme_wet_days(chr),
             .r_total(chr),
             .sdii(chr))
    
    ind <- data.frame(id = rep(x$id[1], length(indices)),
                      date = rep(x$date[1], length(indices)),
                      index = indices,
                      value = ind,
                      stringsAsFactors = FALSE)
    
  })
  
  result <- do.call("rbind", object)
  
  # merge with ids and lon lat
  result <- merge(result, lonlat, by = "id")
  
  result <- result[,c("id","date","lon","lat","index","value")]
  
  result$id <- as.integer(result$id)
  
  result <- result[order(result$date), ]
  
  result <- result[order(result$id), ]
  
  result <- as.data.frame(result, stringsAsFactors = FALSE)
  
  class(result) <- union("chirps_df", class(result))
  
  return(result)
}

#' Maximum length of consecutive dry days
#' @param object numeric vector
#' @return the MLDS index, which is the maximum length of consecutive dry days
#' precipitation < 1 mm
#' @examples
#' set.seed(12)
#' r <- runif(20, 0, 9)
#' r[c(1,4,9:12,17)] <- 0
#' chirps:::.dryspell(r)
#' @noRd
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

#' Maximum length of consecutive wet days
#' @param object numeric vector
#' @return the MLWS index, which is the maximum length of consecutive wet days
#' precipitation > 1 mm
#' @examples
#' set.seed(12)
#' r <- runif(20, 0, 9)
#' r[c(1,4,9:11)] <- 0.1
#' chirps:::.wetspell(r)
#' @noRd
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

#' Heavy precipitation days (10 >= r < 20 mm)
#' @param object numeric vector
#' @return the R10mm index, which is number of heavy precipitation days (10 >= r
#'  < 20 mm)
#' @examples
#' set.seed(12)
#' r <- runif(20, 0, 12)
#' r[c(1,4,9:11)] <- 0.1
#' chirps:::.r_ten_mm(r)
#' @noRd
.r_ten_mm <- function(object) {
  
  rt <- sum(object >= 10 & object < 20, na.rm = TRUE)
  
  return(rt)

}

#' Very heavy precipitation days (r >= 20)
#' @param object numeric vector
#' @return the R20mm index, which is number of very heavy precipitation days (r
#'  >= 20)
#' @examples
#' set.seed(12)
#' r <- runif(20, 10, 23)
#' r[c(1,4,9:11)] <- 0.1
#' chirps:::.r_twenty_mm(r)
#' @noRd
.r_twenty_mm <- function(object) {
  
  rtw <- sum(object >= 20, na.rm = TRUE)
  
  return(rtw)
  
}

#' Simple rainfall intensity index
#' @param object numeric vector
#' @return the SDII index, which is the simple daily intensity 
#' index total precipitation divided by the number of wet days (r >= 1.0mm)
#' @examples
#' set.seed(12)
#' r <- runif(20, 0, 9)
#'
#' r[c(1,4,9:11)] <- 0.1
#'
#' .sdii(r)
#'
#' chirps:::.sdii(rep(0.1, 9))
#' @noRd
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

#' Compute Rx5day rainfall index
#' @param object numeric vector
#' @return the Rx5day index, which is the maximun sum 
#' of rain in consecutive 5 days
#' @examples
#' set.seed(12)
#' r <- runif(20, 0, 9)
#' r[c(1,4,9:12,17)] <- 0
#' chirps:::.r_five_day(r)
#' @noRd
.r_five_day <- function(object)
{
  # this look for the maximum sum of rain in
  # consecutive 5 days
  l <- length(object)
  
  r5day <- NULL
  
  for (i in 1:(l - 4)) {
    r5day <- cbind(r5day, sum(object[i:(i + 4)], na.rm = TRUE))
    
  }
  
  r5day <- max(r5day, na.rm = TRUE)
  
  return(r5day)
  
}

#' Maximum 1-day rainfall
#' @param object numeric vector
#' @return the Rx1day index, which is the 1-day rainfall
#' @examples
#' set.seed(12)
#' r <- runif(20, 0, 9)
#' r[c(1,4,9:12,17)] <- 0
#' chirps:::.r_one_day(r)
#' @noRd
.r_one_day <- function(object) {
  
  ro <- max(object, na.rm = TRUE)
  
  return(ro)
  
}

#' Total rainfall (mm) in wet days (r >= 1)
#' @param object numeric vector
#' @return the Rtotal index, which is sum of rainfall (mm) in wet days (r >= 1)
#' @examples
#' set.seed(12)
#' r <- runif(20, 0, 9)
#' r[c(1,4,9:12,17)] <- 0
#' chirps:::.r_total(r)
#' @noRd
.r_total <- function(object) {
  
  rt <- object[object >= 1]
  
  rt <- sum(object, na.rm = TRUE)
  
  return(rt)
  
}


#' Very wet days
#' @param object numeric vector
#' @return the R95p index, annual total PRCP when rain > 95th percentile
#' @examples
#' set.seed(12)
#' r <- runif(20, 0, 9)
#' r[c(1,4,9:12,17)] <- 0
#' chirps:::.very_wet_days(r)
#' @noRd
.very_wet_days <- function(object) {
  
  q <- stats::quantile(object, probs = seq(0, 1, 0.05), na.rm = TRUE)
  q <- q["95%"]
  
  vwd <- object[object > q]
  
  vwd <- sum(vwd, na.rm = TRUE)
  
  return(vwd)
  
}

#' Extreme wet days
#' @param object numeric vector
#' @return the R99p index, annual total PRCP when rain > 99th percentile
#' @examples
#' set.seed(12)
#' r <- runif(20, 0, 9)
#' r[c(1,4,9:12,17)] <- 0
#' chirps:::.extreme_wet_days(r)
#' @noRd
.extreme_wet_days <- function(object) {
  
  q <- stats::quantile(object, probs = seq(0, 1, 0.01), na.rm = TRUE)
  q <- q["99%"]
  
  ewd <- object[object > q]
  
  ewd <- sum(ewd, na.rm = TRUE)
  
  return(ewd)
  
}
