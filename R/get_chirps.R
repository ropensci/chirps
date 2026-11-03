#' Get CHIRPS precipitation data
#'
#' Get daily rainfall from CHIRPS, produced by the Climate Hazards Center
#'  (CHC), for points, polygons or an area. The data are read from the CHC
#'  server, which is the main source and serves both CHIRPS v3 (default) and
#'  CHIRPS v2.
#'
#' @param object input, an object of class \code{\link[base]{data.frame}} (or
#'  any other object that can be coerced to \code{data.frame}) with longitude
#'  and latitude in the first two columns, \code{\link[terra]{SpatVector}},
#'  \code{\link[terra]{SpatRaster}}, \code{\link[terra]{SpatExtent}},
#'  \code{\link[sf]{sf}} or \code{geojson}
#' @param dates a character or Date vector "YYYY-MM-DD", with one date for a
#'  single day or two dates for the start and end of the period
#' @param server a character, "CHC" (default and recommended) or
#'  "ClimateSERV". See the section on servers
#' @param as.sf logical, returns an object of class \code{\link[sf]{sf}}
#' @param as.geojson logical, returns an object of class \code{geojson}
#' @param as.raster logical, returns an object of class
#' \code{\link[terra]{SpatRaster}}
#' @param as.matrix logical, returns a \code{matrix} with one row per
#'  location and one column per day, named "YYYY-MM-DD"
#' @param ... additional arguments passed to \code{\link{get_chirps_raw}} and
#' \code{\link[terra]{extract}}. See details
#'
#' @details
#' \strong{Additional arguments}
#'
#' \bold{version}: CHIRPS version, "3.0" (default) or "2.0". CHC retires
#'  CHIRPS v2 after the December 2026 release
#'
#' \bold{type}: CHIRPS v3 product, "rnl" (default) or "sat". See
#'  \code{\link{get_chirps_raw}}
#'
#' \bold{resolution}: numeric, resolution of CHIRPS v2, 0.05 (default) or
#'  0.25 degrees
#'
#' \bold{operation}: for polygons, the function used to summarise the cells
#'  of each polygon on each day
#'  \tabular{rll}{
#'  \bold{operation}      \tab    \tab \bold{value}\cr
#'  max                   \tab =  \tab 0\cr
#'  min                   \tab =  \tab 1\cr
#'  median                \tab =  \tab 2\cr
#'  sum                   \tab =  \tab 4\cr
#'  average               \tab =  \tab 5 (\emph{default value})\cr
#'  }
#'
#' \strong{Coverage}: CHIRPS v3 covers 60°S to 60°N and CHIRPS v2 covers
#'  50°S to 50°N, from 1981 to near present. Final data for a month arrive
#'  some weeks after it ends.
#'
#' \strong{Polygons}: with \code{sf}, \code{SpatVector} or \code{geojson}
#'  polygons, each day is summarised over the cells that touch the polygon,
#'  with the function set by \bold{operation}. The \code{lon} and \code{lat}
#'  columns give the centroid of each polygon.
#'
#' \strong{Indices}: for rainfall indices (dry spells, heavy rain days,
#'  intensity) use \code{climatrends::rainfall()}. The matrix returned with
#'  \code{as.matrix = TRUE} is its input, see the examples.
#'
#' @section Servers:
#' \bold{CHC} (default) reads the CHIRPS files from
#'  \url{https://data.chc.ucsb.edu} through \CRANpkg{terra}. It serves CHIRPS
#'  v3 and v2, gives the value of the cell that holds each point, and handles
#'  many points and long periods in one call. It is the recommended source.
#'
#' \bold{ClimateSERV} is kept for backward compatibility. It serves CHIRPS v2
#'  only, averaged over a small buffer around each point, so its values can
#'  differ from the CHC values. It sends several requests per location and is
#'  slow for more than a few points. ClimateSERV support for CHIRPS may be
#'  removed in a future release. Its extra arguments are \bold{dist}, the
#'  buffer distance around each point, \bold{nQuadSegs}, the number of
#'  segments per buffer quadrant, and \bold{operation} as above.
#'
#' \code{SpatVector}, \code{SpatRaster} and \code{SpatExtent} input always
#'  uses CHC.
#'
#' @return A data frame of \acronym{CHIRPS} data in the long format, ordered
#'  by \code{id} and \code{date}, or a matrix, raster, \code{sf} or
#'  \code{geojson} object as requested:
#' \describe{
#'   \item{id}{the index for the rows in \code{object}}
#'   \item{date}{the date of each value}
#'   \item{lon}{the longitude as provided in \code{object}}
#'   \item{lat}{the latitude as provided in \code{object}}
#'   \item{chirps}{the \acronym{CHIRPS} value in mm}
#' }
#' @references
#'
#' Funk, C., Peterson, P., Harrison, L. et al. (2026). Scientific Data, 13,
#'  718. \cr\doi{10.1038/s41597-026-07096-4}
#'
#' Funk C. et al. (2015). Scientific Data, 2, 150066.
#'  \cr\doi{10.1038/sdata.2015.66}
#'
#' @seealso \code{\link{get_chirps_raw}} for the rasters,
#'  \code{\link{get_chirts}} for temperature, and the \CRANpkg{climatrends}
#'  package for rainfall and temperature indices
#'
#' @examplesIf interactive()
#' library("chirps")
#' library("terra")
#'
#' dates = c("2017-12-15", "2017-12-31")
#' lonlat = data.frame(lon = c(-55.0281, -54.9857), lat = c(-2.8094, -2.8756))
#'
#' # Case 1: return a data.frame in the long format
#' r1 = get_chirps(lonlat, dates)
#'
#' # Case 2: a single day, from the CHIRPS v3 product based on IMERG
#' r2 = get_chirps(lonlat, "2017-12-15", type = "sat")
#'
#' # Case 3: return a matrix, one column per day
#' r3 = get_chirps(lonlat, dates, as.matrix = TRUE)
#'
#' # Case 4: input SpatVector and return raster
#' f = system.file("ex/lux.shp", package = "terra")
#' v = vect(f)
#' r4 = get_chirps(v, dates, as.raster = TRUE)
#'
#' # Case 5: input SpatExtent and return a raster within the extent
#' area = ext(c(-66, -64, -6, -4))
#' r5 = get_chirps(area, dates)
#'
#' # Case 6: total rainfall per polygon and day
#' r6 = get_chirps(v, dates, as.raster = FALSE, operation = 4)
#'
#' # Case 7: rainfall indices with climatrends
#' if (requireNamespace("climatrends", quietly = TRUE)) {
#'   climatrends::rainfall(r3, day.one = dates[1], span = ncol(r3))
#' }
#'
#' @importFrom sf st_centroid read_sf st_geometry_type st_coordinates
#'  st_geometry st_crs st_is_longlat st_transform
#' @importFrom terra crop extract rast ext crds geomtype
#' @export
get_chirps = function(object, dates, server = "CHC", ...) {
  if (isFALSE(any(server %in% c("CHC", "ClimateSERV")))) {
    stop("Unknown server, please choose 'CHC' or 'ClimateSERV'\n",
         call. = FALSE)
  }
  
  if (identical(server, "ClimateSERV")) {
    .notice_climateserv()
  }
  
  UseMethod("get_chirps")
  
}

#' @rdname get_chirps
#' @export
get_chirps.default = function(object, dates, server = "CHC",
                              as.matrix = FALSE, ...) {
  
  object = as.data.frame(object)
  
  dots = list(...)
  
  as.raster = isTRUE(dots[["as.raster"]])
  
  # validate lonlat to check if they are within the CHIRPS range
  .validate_lonlat(object, xlim = c(-180, 180),
                   ylim = .chirps_ylim(dots[["version"]], server))
  
  nr = nrow(object)
  
  dates = .validate_dates(dates)
  
  days = seq.Date(as.Date(dates[1]), as.Date(dates[2]), by = "day")
  
  if (server == "ClimateSERV") {
    # get geojson strings from data.frame
    gj = as.geojson(object, ...)
    
    class(gj) = "character"
    
    gj = split(gj, seq_along(gj))
    
    result = .chirps_climateserv(gj, object, dates, dots[["operation"]])
    
    if (isTRUE(as.matrix)) {
      result = .to_wide(result, "chirps", seq_len(nr), days, "")
      return(.date_colnames(result, days))
    }
    
    class(result) = c("chirps", "chirps_df", class(result))
    
    return(result)
    
  }
  
  # server CHC, get CHIRPS CoG files
  rr = get_chirps_raw(dates = dates, ...)
  
  if (isTRUE(as.raster)) {
    result = .crop_to_points(rr, object)
    return(result)
  }
  
  result = .extract_values(rr, object, dots)
  
  if (isTRUE(as.matrix)) {
    return(.date_colnames(result, days))
  }
  
  result = .to_long(result,
                    ids = seq_len(nr),
                    lon = object[, 1],
                    lat = object[, 2],
                    days = days,
                    name = "chirps")
  
  class(result) = c("chirps", "chirps_df", class(result))
  
  return(result)
  
}

#' @rdname get_chirps
#' @method get_chirps SpatVector
#' @export
get_chirps.SpatVector = function(object,
                                 dates,
                                 server = "CHC",
                                 as.raster = TRUE,
                                 ...) {
  dots = list(...)
  as.matrix = isTRUE(dots[["as.matrix"]])
  
  if (isTRUE(as.raster) && !as.matrix) {
    rr = get_chirps_raw(dates = dates, ...)
    return(terra::crop(rr, y = object))
  }
  
  # points and polygons go through the sf method, so polygons are
  # summarised over their cells and ids follow the rows of object
  object = .vect_to_sf(object)
  
  result = get_chirps.sf(object, dates, server = "CHC", ...)
  
  if (as.matrix) {
    days = .days(dates)
    result = .to_wide(result, "chirps", seq_len(nrow(object)), days, "")
    result = .date_colnames(result, days)
  }
  
  result
  
}

#' @rdname get_chirps
#' @method get_chirps SpatRaster
#' @export
get_chirps.SpatRaster = function(object,
                                 dates,
                                 server = "CHC",
                                 as.raster = TRUE,
                                 ...) {
  get_chirps.SpatExtent(terra::ext(object),
                        dates = dates,
                        server = server,
                        as.raster = as.raster,
                        ...)
}

#' @rdname get_chirps
#' @method get_chirps SpatExtent
#' @export
get_chirps.SpatExtent = function(object,
                                 dates,
                                 server = "CHC",
                                 as.raster = TRUE,
                                 ...) {
  dots = list(...)
  
  # get CHIRPS CoG files
  rr = get_chirps_raw(dates = dates, ...)
  
  result = terra::crop(rr, y = object)
  
  if (isFALSE(as.raster) || isTRUE(dots[["as.matrix"]])) {
    result = .date_colnames(terra::values(result, mat = TRUE), .days(dates))
  }
  
  return(result)
  
}

#' @rdname get_chirps
#' @method get_chirps sf
#' @export
get_chirps.sf = function(object, dates, server = "CHC",
                         as.sf = FALSE,
                         ...) {
  # check geometry type
  type = c("POINT", "POLYGON")
  
  dots = list(...)
  
  # check for supported types
  gtype = as.character(sf::st_geometry_type(object))
  
  supp_type =
    c(all(gtype == "POINT"),
      all(gtype %in% c("POLYGON", "MULTIPOLYGON")))
  
  if (!any(supp_type)) {
    stop(
      "The sf geometry type is not supported.
         Please provide a sf object of geometry type 'POINT' or 'POLYGON'\n"
    )
  }
  
  type = type[which(supp_type)]
  
  # work in longitude and latitude
  crs = sf::st_crs(object)
  if (!is.na(crs) && !isTRUE(sf::st_is_longlat(object))) {
    object = sf::st_transform(object, 4326)
  }
  
  geom = sf::st_geometry(object)
  
  # lon and lat of polygons are their centroids, one per feature
  if (type == "POLYGON") {
    geom = suppressWarnings(sf::st_centroid(geom))
  }
  
  lonlat = sf::st_coordinates(geom)[, 1:2, drop = FALSE]
  
  lonlat = data.frame(lon = lonlat[, 1], lat = lonlat[, 2])
  
  # validate lonlat to check if they are within the CHIRPS range
  .validate_lonlat(lonlat, xlim = c(-180, 180),
                   ylim = .chirps_ylim(dots[["version"]], server))
  
  nr = nrow(lonlat)
  
  dates = .validate_dates(dates)
  
  days = seq.Date(as.Date(dates[1]), as.Date(dates[2]), by = "day")
  
  if (server == "ClimateSERV") {
    # get geojson strings from data.frame
    gj = as.geojson(object, ...)
    
    class(gj) = "character"
    
    gj = split(gj, seq_along(gj))
    
    result = .chirps_climateserv(gj, lonlat, dates, dots[["operation"]])
    
  }
  
  if (server == "CHC") {
    # get CHIRPS CoG files
    rr = get_chirps_raw(dates = dates, ...)
    
    # polygons are summarised over their cells, as ClimateSERV does
    if (type == "POLYGON") {
      result = .polygon_values(rr, object, dots[["operation"]], dots)
    } else {
      result = .extract_values(rr, lonlat, dots)
    }
    
    result = .to_long(result,
                      ids = seq_len(nr),
                      lon = lonlat$lon,
                      lat = lonlat$lat,
                      days = days,
                      name = "chirps")
    
  }
  
  if (isTRUE(as.sf)) {
    wide = .to_wide(result, "chirps", seq_len(nr), days, "day_")
    colnames(wide) = paste0("day_", as.integer(days))
    rownames(wide) = NULL
    result = cbind(object, wide)
    return(result)
  }
  
  class(result) = c("chirps", "chirps_df", class(result))
  
  return(result)
  
}

#' @rdname get_chirps
#' @method get_chirps sfc
#' @export
get_chirps.sfc = function(object, dates, server = "CHC", as.sf = FALSE, ...) {
  get_chirps.sf(sf::st_sf(geometry = object), dates = dates, server = server,
                as.sf = as.sf, ...)
}

#' @rdname get_chirps
#' @method get_chirps geojson
#' @export
get_chirps.geojson = function(object,
                              dates,
                              server = "CHC",
                              as.geojson = FALSE,
                              ...) {
  dots = list(...)
  
  type = c("type\":\"Point", "type\":\"Polygon")
  
  # check for supported types
  supp_type = c(all(grepl(type[[1]], object)),
                all(grepl(type[[2]], object)))
  
  if (isFALSE(any(supp_type))) {
    stop(
      "The geojson geometry type is not supported.
         Please provide a geojson of geometry type 'Point' or 'Polygon'\n"
    )
  }
  
  is_polygon = all(grepl(type[[2]], object))
  
  # take the coordinates of Points and the centroid of Polygons
  lonlat = lapply(object, function(x) {
    x = sf::st_geometry(sf::read_sf(x))
    if (!all(grepl("POINT", sf::st_geometry_type(x)))) {
      x = suppressWarnings(sf::st_centroid(x))
    }
    sf::st_coordinates(x)[1, 1:2]
  })
  
  # put all together
  lonlat = do.call("rbind", lonlat)
  
  lonlat = data.frame(lon = lonlat[, 1], lat = lonlat[, 2])
  
  # validate lonlat to check if they are within the CHIRPS range
  .validate_lonlat(lonlat, xlim = c(-180, 180),
                   ylim = .chirps_ylim(dots[["version"]], server))
  
  nr = nrow(lonlat)
  
  dates = .validate_dates(dates)
  
  days = seq.Date(as.Date(dates[1]), as.Date(dates[2]), by = "day")
  
  if (server == "ClimateSERV") {
    # get geojson strings
    gj = split(object, seq_along(object))
    
    gj = lapply(gj, as.character)
    
    result = .chirps_climateserv(gj, lonlat, dates, dots[["operation"]])
    
  }
  
  if (server == "CHC") {
    # get CHIRPS CoG files
    rr = get_chirps_raw(dates = dates, ...)
    
    # polygons are summarised over their cells, as ClimateSERV does
    if (is_polygon) {
      polygons = do.call(rbind, lapply(object, function(x) {
        sf::st_sf(geometry = sf::st_geometry(sf::read_sf(x)))
      }))
      result = .polygon_values(rr, polygons, dots[["operation"]], dots)
    } else {
      result = .extract_values(rr, lonlat, dots)
    }
    
    result = .to_long(result,
                      ids = seq_len(nr),
                      lon = lonlat$lon,
                      lat = lonlat$lat,
                      days = days,
                      name = "chirps")
    
  }
  
  class(result) = c("chirps", "chirps_df", class(result))
  
  if (isTRUE(as.geojson)) {
    result = split(result, result$id)
    
    object = split(object, seq_along(object))
    
    # add geojson properties
    result = mapply(function(X, Y) {
      .add_geojson_properties(geometry = X,
                              properties = Y,
                              name = "chirps")
      
    }, X = object, Y = result[])
    
    class(result) = c("geojson", "json", class(result))
    
  }
  
  return(result)
  
}
