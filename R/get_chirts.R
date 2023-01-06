#' Get CHIRTS temperature data data
#' 
#' Get daily maximum and minimum temperature data from the "Climate Hazards
#'  Group". CHIRTS-daily is a global 2-m temperature product that combines the
#'  monthly CHIRTSmax data set with the ERA5 reanalysis to produce routinely
#'  updated data to support the monitoring of temperature extreme. Data is
#'  currently available from 1983 to 2016. Soon available to near-present.
#'
#' @inheritParams get_chirps 
#' @param object an object of class \code{\link[base]{data.frame}} (or any other 
#'  object that can be coerced to a \code{data.frame}), \code{\link[terra]{SpatExtent}},
#'  \code{\link[terra]{SpatVector}}, or \code{\link[terra]{SpatRaster}} 
#' @param var character, A valid variable from the options: \dQuote{Tmax},
#'  \dQuote{Tmin}, \dQuote{RHum} and \dQuote{HeatIndex}
#' @param ... additional arguments passed to \code{\link[terra]{terra}}
#' @return A SpatRaster object if \code{as.raster=TRUE}, else \code{matrix}, 
#' \code{list}, or \code{data.frame}
#' @details
#' 
#' Variable description from 
#' \url{https://data.chc.ucsb.edu/products/CHIRTSdaily/aaa.Readme.txt}
#' \describe{
#'   \item{Tmax}{Daily average maximum air temperature at 2 m above ground}
#'   \item{Tmin}{Daily average minimum air temperature at 2 m above ground}
#'   \item{RHum}{Daily average relative humidity}
#'   \item{HeatIndex}{Daily average heat index}
#'   }
#' @section Additional arguments: 
#' \bold{interval}: supported intervals are \dQuote{daily}, \dQuote{pentad},
#'  \dQuote{dekad}, \dQuote{monthly}, \dQuote{2-monthly}, \dQuote{3-monthly},
#'  and \dQuote{annual}. Currently hard coded to \dQuote{daily}.
#' 
#' @examplesIf interactive()
#' library("chirps")
#' library("terra")
#' 
#' # Case 1: input a data frame return a data frame in the long format
#' dates <- c("2010-12-15","2010-12-31")
#' lonlat <- data.frame(lon = c(-55.0281,-54.9857),
#'                      lat = c(-2.8094, -2.8756))
#' 
#' temp1 <- get_chirts(lonlat, dates, var = "Tmax")
#' 
#' # Case 2: input a data frame return a matrix
#' temp2 <- get_chirts(lonlat, dates, "Tmax", as.matrix = TRUE)
#' 
#' # Case 3: input a raster and return raster
#' f <- system.file("ex/lux.shp", package="terra")
#' v <- vect(f)
#' temp3 <- get_chirts(v, dates, var = "Tmax", as.raster = TRUE)
#' 
#' # Case 4: input a raster and return raster
#' temp4 <- get_chirts(v, dates, var = "Tmax", as.matrix = TRUE)
#'
#' @importFrom terra crop extract rast
#' @export
get_chirts <- function(object, dates, var, ...) {
  
  UseMethod("get_chirts")
  
}

#' @rdname get_chirts
#' @export
get_chirts.default <- function(object, dates, var, as.matrix = FALSE, ...){
  
  if (isTRUE(grepl("Spat", class(object)))) {
    
    r <- get_chirps.SpatVector(object, dates, ...)
    return(r)
    
  }

  dots <- list(...)
  
  as.raster <- dots[["as.raster"]]
  if (!isTRUE(as.raster)) as.raster <- FALSE
  
  if ("sf" %in% class(object)) {
    
    nr <- dim(object)[[1]]
    
    # find the sf_column
    index <- attr(object, "sf_column")
    
    # get the sf column
    lonlat <- object[[index]]  
    # unlist the sf_column
    lonlat <- unlist(object[[index]])
    
    object <- matrix(lonlat,
                   nrow = nr,
                   ncol = 2, 
                   byrow = TRUE, 
                   dimnames = list(seq_len(nr), c("lon","lat")))
  }
  
  object <- as.data.frame(object)
  
  .validate_lonlat(object, xlim = c(-180, 180), ylim = c(-60, 70))
  
  # get CHIRTS GeoTiff files
  rr <- .get_CHIRTS_tiles_CHC(dates, var, ...)

  if (isTRUE(as.raster)) {
    result <- terra::crop(rr, y = object)
    return(result) 
  }else{
    as.raster <- FALSE
  }
  
  if (isTRUE(as.matrix)) {
    result <- terra::extract(rr, y = object, ...)
    result$ID <- NULL
    return(result)
  }else{
    as.matrix <- FALSE
  }
  
  if (all(isFALSE(as.matrix), isFALSE(as.raster))) {
    result <- terra::extract(rr, y = object, ...)
    result$ID <- NULL
    result <- c(t(result))
    days <- seq.Date(as.Date(dates[1]), as.Date(dates[2]), by = "day")
    span <- length(days)
    
    result <- data.frame(id = as.integer(rep(rownames(object), each = span)),
                         lon = as.numeric(rep(object[,1], each = span)),
                         lat = as.numeric(rep(object[,2], each = span)),
                         date = rep(days, each = nrow(object)),
                         chirts = as.numeric(result))
    
    class(result) <- c("chirps_df", class(result))
    
    return(result)
    
  }
  
}


#' @rdname get_chirts
#' @method get_chirts SpatVector
#' @export
get_chirts.SpatVector <- function(object, dates, var, as.raster = TRUE, ...){
  
  dots <- list(...)
  as.matrix <- dots[["as.matrix"]]
  
  # get CHIRTS GeoTiff files
  rr <- .get_CHIRTS_tiles_CHC(dates, var, ...)
  
  if (isTRUE(as.matrix)) {
    result <- terra::extract(rr, y = object, ...)
    result$ID <- NULL
    return(result)
  }else{
    as.matrix <- FALSE
  }
  
  if (isTRUE(as.raster)) {
    result <- terra::crop(rr, y = object)
    return(result) 
  }else{
    as.raster <- FALSE
  }
  
  if (all(isFALSE(as.matrix), isFALSE(as.raster))) {
    days <- seq.Date(as.Date(dates[1]), as.Date(dates[2]), by = "day")
    span <- length(days)
    result <- terra::extract(rr, y = object, ...)
    ids <- result$ID
    result$ID <- NULL
    result <- c(t(result))
    
    result <- data.frame(id = as.integer(rep(ids, each = span)),
                         lon = NA,
                         lat = NA,
                         date = rep(days, each = length(ids)),
                         chirts = as.numeric(result))
    
    class(result) <- c("chirps_df", class(result))
    
    return(result)
    
  }
  
}


#' @rdname get_chirts
#' @method get_chirts SpatRaster
#' @export
get_chirts.SpatRaster <- function(object, dates, var, as.raster = TRUE, ...){
  
  UseMethod("get_chirts", object = "SpatVector")
  
}

#' @rdname get_chirts
#' @method get_chirts SpatExtent
#' @export
get_chirts.SpatExtent <- function(object, dates, var, as.raster = TRUE, ...){
  
  UseMethod("get_chirts", object = "SpatVector")
  
}


#' @noRd
.get_CHIRTS_tiles_CHC <- function(dates, var, 
                                  resolution = 0.05, 
                                  coverage = "global", 
                                  interval = "daily", 
                                  format = "tifs", ...){
  
  stopifnot(var %in% c("HeatIndex", "RHum", "Tmax", "Tmin"))
  
  # setup file names
  seqdate <- seq.Date(as.Date(dates[1]), as.Date(dates[2]), by = "day")
  years <- format(seqdate, format = "%Y")
  # create RH variable for filenames as it's not "RHUM" in the filename but
  # it is in the directory name
  if ("RHum" %in% var) {
    file_name_var <- c(var[var != "RHum"], "RH")
  } else
    file_name_var <- var
  
  # year range
  yrange <- seq(1983, 2016, 1)
  stopifnot(unique(years) %in% yrange)
  
  dates <- gsub("-","\\.", seqdate)
  fnames <- file.path(years, paste0(file_name_var, ".", dates, ".tif"))
  
  resolution <- gsub("0\\.", "p", resolution)
  
  u <- file.path("https://data.chc.ucsb.edu/products/CHIRTSdaily/v1.0", 
                 paste0(coverage, "_", format, "_", resolution), var, fnames)
  u1 <- file.path("/vsicurl", u)
  r <- terra::rast(u1)
  return(r)
}

