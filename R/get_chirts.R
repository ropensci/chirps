#' Get CHIRTS temperature data
#'
#' Get daily temperature from the Climate Hazards Center (CHC) for points,
#'  polygons or an area. Two products are available. CHIRTS-daily (default)
#'  covers 1983 to 2016 and has maximum and minimum temperature, relative
#'  humidity and heat index. CHIRTS-ERA5 extends maximum and minimum
#'  temperature from 1980 to near present.
#'
#' @inheritParams get_chirps
#' @param object an object of class \code{\link[base]{data.frame}} (or any other
#'  object that can be coerced to a \code{data.frame}) with longitude and
#'  latitude in the first two columns, \code{\link[sf]{sf}} points,
#'  \code{\link[terra]{SpatVector}}, \code{\link[terra]{SpatRaster}} or
#'  \code{\link[terra]{SpatExtent}}
#' @param var character, A valid variable from the options: \dQuote{Tmax},
#'  \dQuote{Tmin}, \dQuote{RHum} and \dQuote{HeatIndex}. CHIRTS-ERA5 has
#'  \dQuote{Tmax} and \dQuote{Tmin} only
#' @param ... further arguments passed to \code{\link{get_chirts_raw}} and
#'  \code{\link[terra]{extract}}. See details
#' @return A data frame in the long format, ordered by \code{id} and
#'  \code{date}, with the value in the column \code{chirts}. A SpatRaster
#'  if \code{as.raster = TRUE}, or a matrix with one column per day, named
#'  "YYYY-MM-DD", if \code{as.matrix = TRUE}
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
#'
#' \strong{Additional arguments}
#'
#' \bold{product}: \dQuote{CHIRTS-daily} (default, 1983 to 2016) or
#'  \dQuote{CHIRTS-ERA5} (1980 to near present, Tmax and Tmin)
#'
#' \bold{operation}: for polygons, the function used to summarise the cells
#'  of each polygon on each day, with the codes listed in
#'  \code{\link{get_chirps}} (average by default)
#'
#' \strong{Coverage}: both products cover 60°S to 70°N.
#'
#' \strong{Indices}: for temperature indices (growing degree days, crop
#'  sensitive indices, frost and heat days) use the \CRANpkg{climatrends}
#'  package, which takes the matrices returned with \code{as.matrix = TRUE}.
#'
#' @seealso \code{\link{get_chirts_raw}}
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
#' # Case 3: recent dates from CHIRTS-ERA5
#' temp3 = get_chirts(lonlat, c("2024-01-01", "2024-01-31"), var = "Tmin",
#'                    product = "CHIRTS-ERA5")
#'
#' # Case 4: input a SpatVector and return raster
#' f = system.file("ex/lux.shp", package="terra")
#' v = vect(f)
#' temp4 = get_chirts(v, dates, var = "Tmax", as.raster = TRUE)
#'
#' # Case 5: average per polygon and day
#' temp5 = get_chirts(v, dates, var = "Tmax", as.raster = FALSE)
#'
#' # Case 6: indices with climatrends
#' if (requireNamespace("climatrends", quietly = TRUE)) {
#'   tmin = get_chirts(lonlat, dates, var = "Tmin", as.matrix = TRUE)
#'   temp = array(c(temp2, tmin), dim = c(dim(temp2), 2),
#'                dimnames = list(NULL, colnames(temp2), NULL))
#'   climatrends::temperature(temp, day.one = dates[1], span = ncol(temp2))
#' }
#'
#' @importFrom terra crop extract rast ext crds geomtype vect
#' @importFrom sf st_sf st_coordinates st_geometry st_geometry_type st_centroid
#'  st_transform
#' @export
get_chirts = function(object, dates, var, ...) {
  UseMethod("get_chirts")
  
}

#' @rdname get_chirts
#' @export
get_chirts.default = function(object, dates, var, as.matrix = FALSE, ...){
  
  dots = list(...)
  
  as.raster = isTRUE(dots[["as.raster"]])
  
  # sf points and polygons go through the SpatVector method
  if (inherits(object, c("sf", "sfc"))) {
    v = terra::vect(sf::st_sf(geometry = sf::st_geometry(object)))
    dots[["as.raster"]] = as.raster
    dots[["as.matrix"]] = as.matrix
    return(do.call(get_chirts.SpatVector,
                   c(list(object = v, dates = dates, var = var), dots)))
  }
  
  object = as.data.frame(object)
  
  .validate_lonlat(object, xlim = c(-180, 180), ylim = c(-60, 70))
  
  # CHIRTS-ERA5 starts in 1980, the product functions check their own years
  dates = .validate_dates(dates, availability = c("1980-01-01", "0"))
  
  days = seq.Date(as.Date(dates[1]), as.Date(dates[2]), by = "day")
  
  # get CHIRTS GeoTiff files
  rr = get_chirts_raw(dates, var, ...)
  
  if (isTRUE(as.raster)) {
    result = .crop_to_points(rr, object)
    return(result) 
  }
  
  result = .extract_values(rr, object, dots)
  
  if (isTRUE(as.matrix)) {
    return(.date_colnames(result, days))
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
    
    if (isTRUE(as.raster) && !as.matrix) {
      return(terra::crop(rr, y = object))
    }
    
    days = .days(dates)
    
    object = .vect_to_sf(object)
    
    gtype = as.character(sf::st_geometry_type(object))
    is_polygon = all(gtype %in% c("POLYGON", "MULTIPOLYGON"))
    if (!is_polygon && !all(gtype == "POINT")) {
      stop("Please provide points or polygons.\n", call. = FALSE)
    }
    
    if (is_polygon) {
      result = .polygon_values(rr, object, dots[["operation"]], dots)
      xy = sf::st_coordinates(suppressWarnings(
        sf::st_centroid(.to_lonlat(sf::st_geometry(object)))))
    } else {
      xy = sf::st_coordinates(.to_lonlat(sf::st_geometry(object)))[, 1:2, drop = FALSE]
      result = .extract_values(rr, xy, dots)
    }
    
    .validate_lonlat(xy, xlim = c(-180, 180), ylim = c(-60, 70))
    
    if (as.matrix) {
      return(.date_colnames(result, days))
    }
    
    result = .to_long(result,
                      ids = seq_len(nrow(xy)),
                      lon = xy[, 1],
                      lat = xy[, 2],
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
    result = .date_colnames(terra::values(result, mat = TRUE), .days(dates))
  }
  
  return(result)
  
}
