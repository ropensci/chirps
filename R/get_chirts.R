#' Get CHIRTS temperature data
#'
#' Get daily maximum and minimum temperature data from the "Climate Hazards
#'  Group". CHIRTS-daily is a global 2-m temperature product that combines the
#'  monthly CHIRTSmax data set with the ERA5 reanalysis to produce routinely
#'  updated data to support the monitoring of temperature extreme. Data is
#'  currently available from 1983 to 2016. Soon available to near-present.
#'
#' @inheritParams get_chirps
#' @param object an object of class \code{\link[base]{data.frame}} (or any other
#'  object that can be coerced to a \code{data.frame}),
#'  \code{\link[terra]{SpatVector}}, or \code{\link[terra]{SpatRaster}}
#' @param var character, A valid variable from the options: \dQuote{Tmax},
#'  \dQuote{Tmin}, \dQuote{RHum} and \dQuote{HeatIndex}
#' @param ... further arguments passed to \code{\link[terra]{terra}}
#' @return A SpatRaster object if \code{as.raster=TRUE}, else \code{matrix},
#' \code{list}, or \code{data.frame}
#' @details
#'
#' Variable description from
#' \url{https://data.chc.ucsb.edu/products/CHIRTS/daily/aaa.Readme.txt}
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
#' dates = c("2010-12-15","2010-12-31")
#' lonlat = data.frame(lon = c(-55.0281,-54.9857),
#'                      lat = c(-2.8094, -2.8756))
#'
#' temp1 = get_chirts(lonlat, dates, var = "Tmax")
#'
#' # Case 2: input a data frame return a matrix
#' temp2 = get_chirts(lonlat, dates, "Tmax", as.matrix = TRUE)
#'
#' # Case 3: input a raster and return raster
#' f = system.file("ex/lux.shp", package="terra")
#' v = vect(f)
#' temp3 = get_chirts(v, dates, var = "Tmax", as.raster = TRUE)
#'
#' # Case 4: input a raster and return raster
#' temp4 = get_chirts(v, dates, var = "Tmax", as.matrix = TRUE)
#'
#' @importFrom terra crop extract rast ext crds geomtype
#' @importFrom sf st_coordinates st_geometry
#' @export
get_chirts = function(object, dates, var, ...) {
  UseMethod("get_chirts")
  
}

#' @rdname get_chirts
#' @export
get_chirts.default = function(object, dates, var, as.matrix = FALSE, ...){
  
  dots = list(...)
  
  as.raster = isTRUE(dots[["as.raster"]])
  
  if (inherits(object, "sf")) {
    object = sf::st_coordinates(sf::st_geometry(object))[, 1:2, drop = FALSE]
  }
  
  object = as.data.frame(object)
  
  .validate_lonlat(object, xlim = c(-180, 180), ylim = c(-60, 70))
  
  .validate_dates(dates)
  
  days = seq.Date(as.Date(dates[1]), as.Date(dates[2]), by = "day")
  
  # get CHIRTS GeoTiff files
  rr = get_chirts_raw(dates, var, ...)
  
  if (isTRUE(as.raster)) {
    result = terra::crop(rr, y = object)
    return(result) 
  }
  
  result = .extract_values(rr, object, dots)
  
  if (isTRUE(as.matrix)) {
    return(as.matrix(result))
  }
  
  result = .to_long(result,
                    ids = seq_len(nrow(object)),
                    lon = object[, 1],
                    lat = object[, 2],
                    days = days,
                    name = "chirts")
  
  class(result) = c("chirps_df", class(result))
  
  return(result)
  
}

#' @rdname get_chirts
#' @method get_chirts SpatVector
#' @export
get_chirts.SpatVector =
  function(object, dates, var, as.raster = TRUE, ...) {
    dots = list(...)
    as.matrix = isTRUE(dots[["as.matrix"]])
    
    # get CHIRTS GeoTiff files
    rr = get_chirts_raw(dates, var, ...)
    
    if (isTRUE(as.matrix)) {
      result = .extract_values(rr, object, dots)
      return(as.matrix(result))
    }
    
    if (isTRUE(as.raster)) {
      result = terra::crop(rr, y = object)
      return(result)
    }
    
    days = seq.Date(as.Date(dates[1]), as.Date(dates[2]), by = "day")
    
    result = .extract_values(rr, object, dots)
    
    ids = attr(result, "ID")
    
    # coordinates are known for points, polygons return one row per cell
    lon = NA
    lat = NA
    if (terra::geomtype(object) == "points") {
      xy = terra::crds(object)
      lon = xy[ids, 1]
      lat = xy[ids, 2]
    }
    
    result = .to_long(result,
                      ids = ids,
                      lon = lon,
                      lat = lat,
                      days = days,
                      name = "chirts")
    
    class(result) = c("chirps_df", class(result))
    
    return(result)
    
  }

#' @rdname get_chirts
#' @method get_chirts SpatRaster
#' @export
get_chirts.SpatRaster =
  function(object, dates, var, as.raster = TRUE, ...) {
    get_chirts.SpatExtent(terra::ext(object),
                          dates = dates,
                          var = var,
                          as.raster = as.raster,
                          ...)
    
  }

#' @rdname get_chirts
#' @method get_chirts SpatExtent
#' @export
get_chirts.SpatExtent = function(object, dates, var, as.raster = TRUE, ...){
  
  dots = list(...)
  
  # get CHIRTS GeoTiff files
  rr = get_chirts_raw(dates, var, ...)
  
  result = terra::crop(rr, y = object)
  
  if (isFALSE(as.raster) || isTRUE(dots[["as.matrix"]])) {
    result = terra::values(result, mat = TRUE)
  }
  
  return(result)
  
}
