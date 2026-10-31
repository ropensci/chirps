#' Get CHIRPS precipitation data
#'
#' Get daily precipitation data from the "Climate Hazards Group". Two server
#'  sources are available. The first, "CHC" (default) is recommended for
#'  multiple data-points, while "ClimateSERV" is recommended when few
#'  data-points are required (~ 50).
#'
#' @param object input, an object of class \code{\link[base]{data.frame}} (or
#'  any other object that can be coerced to \code{data.frame}),
#'  \code{\link[terra]{SpatVector}}, \code{\link[terra]{SpatRaster}},
#'  \code{\link[sf]{sf}} or \code{geojson}
#' @param dates a character of start and end dates in that order in the format
#'  "YYYY-MM-DD"
#' @param server a character that represents the server source "CHC" or
#'  "ClimateSERV"
#' @param as.sf logical, returns an object of class \code{\link[sf]{sf}}
#' @param as.geojson logical, returns an object of class \code{geojson}
#' @param as.raster logical, returns an object of class
#' \code{\link[terra]{SpatRaster}}
#' @param as.matrix logical, returns an object of class \code{matrix}
#' @param ... additional arguments passed to \code{\link[terra]{terra}}
#' or \code{\link[sf]{sf}} methods
#' See details
#'
#' @details
#' Data description at
#' \url{https://data.chc.ucsb.edu/products/CHIRPS-2.0/README-CHIRPS.txt}
#'
#' \strong{Additional arguments when using server = "CHC"}
#'
#' \bold{resolution}: numeric, resolution of CHIRPS tiles either 0.05 (default)
#'  or 0.25 degrees
#'
#' \strong{Additional arguments when using server = "ClimateSERV"}
#'
#' \bold{dist}: numeric, buffer distance for each \code{object} coordinate
#'
#' \bold{nQuadSegs}: integer, number of segments per buffer quadrant
#'
#' \bold{operation}: supported operations for ClimateSERV are:
#'  \tabular{rll}{
#'  \bold{operation}      \tab    \tab \bold{value}\cr
#'  max                   \tab =  \tab 0\cr
#'  min                   \tab =  \tab 1\cr
#'  median                \tab =  \tab 2\cr
#'  sum                   \tab =  \tab 4\cr
#'  average               \tab =  \tab 5 (\emph{default value})\cr
#'  }
#'
#' @return A matrix, raster or a data frame of \acronym{CHIRPS} data:
#' \describe{
#'   \item{id}{the index for the rows in \code{object}}
#'   \item{dates}{the dates from which \acronym{CHIRPS} was requested}
#'   \item{lon}{the longitude as provided in \code{object}}
#'   \item{lat}{the latitude as provided in \code{object}}
#'   \item{chirps}{the \acronym{CHIRPS} value in mm}
#' }
#' @references
#'
#' Funk C. et al. (2015). Scientific Data, 2, 150066.
#'  \cr\doi{10.1038/sdata.2015.66}
#'
#' @note \code{get_chirps()} may return some warning messages given by
#' \code{\link[sf]{sf}}, please look \CRANpkg{sf} documentation for possible
#' issues.
#'
#' @examplesIf interactive()
#' library("chirps")
#' library("terra")
#'
#' # Case 1: return as a data.frame
#' dates = c("2017-12-15","2017-12-31")
#' lonlat = data.frame(lon = c(-55.0281,-54.9857), lat = c(-2.8094, -2.8756))
#'
#' r1 = get_chirps(lonlat, dates, server = "CHC")
#'
#' # Case 2: return a matrix
#' r2 = get_chirps(lonlat, dates, server = "CHC", as.matrix = TRUE)
#'
#' # Case 3: input SpatVector and return raster
#' f = system.file("ex/lux.shp", package = "terra")
#' v = vect(f)
#' r3 = get_chirps(v, dates, server = "CHC", as.raster = TRUE)
#'
#' # Case 4: input SpatExtent and return a raster within the extent
#' area = ext(c(-66, -64, -6, -4))
#'
#' dates = c("2017-12-15", "2017-12-31")
#'
#' r4 = get_chirps(area, dates, server = "CHC")
#'
#' # Case 5: using the server "ClimateSERV"
#' r5 = get_chirps(lonlat, dates, server = "ClimateSERV")
#'
#' # Case 6: from "ClimateSERV" and return as a matrix
#' r6 = get_chirps(lonlat, dates, server = "ClimateSERV", as.matrix = TRUE)
#'
#'
#' @importFrom sf st_centroid read_sf st_geometry_type st_coordinates
#'  st_geometry
#' @importFrom terra crop extract rast ext crds geomtype
#' @export
get_chirps = function(object, dates, server = "CHC", ...) {
  if (isFALSE(any(server %in% c("CHC", "ClimateSERV")))) {
    stop("Unknown server, please choose 'CHC' or 'ClimateSERV'\n",
         call. = FALSE)
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
  
  # validate lonlat to check if they are within the CHIRPS range lat -50, 50
  .validate_lonlat(object, xlim = c(-180, 180), ylim = c(-50, 50))
  
  nr = nrow(object)
  
  .validate_dates(dates)
  
  days = seq.Date(as.Date(dates[1]), as.Date(dates[2]), by = "day")
  
  if (server == "ClimateSERV") {
    # get geojson strings from data.frame
    gj = as.geojson(object, ...)
    
    class(gj) = "character"
    
    gj = split(gj, seq_along(gj))
    
    result = .chirps_climateserv(gj, object, dates, dots[["operation"]])
    
    if (isTRUE(as.matrix)) {
      result = .to_wide(result, "chirps", seq_len(nr), days, "chirps-v2.0.")
      return(result)
    }
    
    class(result) = c("chirps", "chirps_df", class(result))
    
    return(result)
    
  }
  
  # server CHC, get CHIRPS CoG files
  rr = get_chirps_raw(dates = dates, ...)
  
  if (isTRUE(as.raster)) {
    result = terra::crop(rr, y = object)
    return(result)
  }
  
  result = .extract_values(rr, object, dots)
  
  if (isTRUE(as.matrix)) {
    return(as.matrix(result))
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
  
  # get CHIRPS CoG files
  rr = get_chirps_raw(dates = dates, ...)
  
  if (isTRUE(as.matrix)) {
    result = .extract_values(rr, object, dots)
    return(as.matrix(result))
  }
  
  if (isTRUE(as.raster)) {
    result = terra::crop(rr, y = object)
    return(result)
  }
  
  .validate_dates(dates)
  
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
                    name = "chirps")
  
  class(result) = c("chirps", "chirps_df", class(result))
  
  return(result)
  
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
    result = as.matrix(result)
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
  supp_type =
    c(all(grepl(type[[1]], sf::st_geometry_type(object))),
      all(grepl(type[[2]], sf::st_geometry_type(object))))
  
  if (!any(supp_type)) {
    stop(
      "The sf geometry type is not supported.
         Please provide a sf object of geometry type 'POINT' or 'POLYGON'\n"
    )
  }
  
  type = type[which(supp_type)]
  
  geom = sf::st_geometry(object)
  
  # polygons are represented by their centroids, one per feature
  if (type == "POLYGON") {
    geom = suppressWarnings(sf::st_centroid(geom))
  }
  
  lonlat = sf::st_coordinates(geom)[, 1:2, drop = FALSE]
  
  lonlat = data.frame(lon = lonlat[, 1], lat = lonlat[, 2])
  
  # validate lonlat to check if they are within the CHIRPS range
  .validate_lonlat(lonlat, xlim = c(-180, 180), ylim = c(-50, 50))
  
  nr = nrow(lonlat)
  
  .validate_dates(dates)
  
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
    
    result = .extract_values(rr, lonlat, dots)
    
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
  
  # validate lonlat to check if they are within the CHIRPS range lat -50, 50
  .validate_lonlat(lonlat, xlim = c(-180, 180), ylim = c(-50, 50))
  
  nr = nrow(lonlat)
  
  .validate_dates(dates)
  
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
    
    result = .extract_values(rr, lonlat, dots)
    
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
