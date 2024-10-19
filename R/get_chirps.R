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
#' dates <- c("2017-12-15","2017-12-31")
#' lonlat <- data.frame(lon = c(-55.0281,-54.9857), lat = c(-2.8094, -2.8756))
#'
#' r1 <- get_chirps(lonlat, dates, server = "CHC")
#'
#' # Case 2: return a matrix
#' r2 <- get_chirps(lonlat, dates, server = "CHC", as.matrix = TRUE)
#'
#' # Case 3: input SpatVector and return raster
#' f <- system.file("ex/lux.shp", package = "terra")
#' v <- vect(f)
#' r3 <- get_chirps(v, dates, server = "CHC", as.raster = TRUE)
#'
#' # Case 4: input SpatExtent and return a raster within the extent
#' area <- ext(c(-66, -64, -6, -4))
#'
#' dates <- c("2017-12-15", "2017-12-31")
#'
#' r4 <- get_chirps(area, dates, server = "CHC")
#'
#' # Case 5: using the server "ClimateSERV"
#' r5 <- get_chirps(lonlat, dates, server = "ClimateSERV")
#'
#' # Case 6: from "ClimateSERV" and return as a matrix
#' r6 <- get_chirps(lonlat, dates, server = "ClimateSERV", as.matrix = TRUE)
#'
#'
#' @importFrom sf st_centroid read_sf st_geometry_type
#' @importFrom terra crop extract rast
#' @export
get_chirps <- function(object, dates, server, ...) {
  if (isFALSE(any(server %in% c("CHC", "ClimateSERV")))) {
    stop("Unknown server, please choose 'CHC' or 'ClimateSERV'\n",
         call. = FALSE)
  }
  
  UseMethod("get_chirps")
  
}

#' @rdname get_chirps
#' @export
get_chirps.default <- function(object, dates, server,
                               as.matrix = FALSE, ...) {
  
  
  if (isTRUE(grepl("Spat", class(object)))) {
    
   r <- get_chirps.SpatVector(object, dates, ...)
   return(r)
    
  }
  
  object <- as.data.frame(object)
  
  dots <- list(...)
  
  as.raster <- dots[["as.raster"]]
  
  # validate lonlat to check if they are within the CHIRPS range lat -50, 50
  .validate_lonlat(object, xlim = c(-180, 180), ylim = c(-50, 50))
  
  if (server == "ClimateSERV") {
    # validate dates
    dates_inter <-
      .reformat_dates(dates, availability = c("1981-01-01", "0"))
    
    # get geojson strings from data.frame
    gj <- as.geojson(object, ...)
    
    class(gj) <- "character"
    
    gj <- split(gj, seq_along(gj))
    
    operation <- dots[["operation"]]
    if (is.null(operation)) {
      operation <- 5
    }
    
    result <- .GET(
      gjson = gj,
      dates = dates_inter,
      operation = operation,
      datatype = 0
    )
    
    names(result)[names(result) == "value"] <- "chirps"
    
    object$id <- rownames(object)
    
    result <- merge(result, object, by = "id")
    
    names(result)[3:5] <- c("chirps", "lon", "lat")
    
    result <- result[, c("id", "lon", "lat", "date", "chirps")]
    
    result <- as.data.frame(result, stringsAsFactors = FALSE)
    
    class(result) <- c("chirps", "chirps_df", class(result))
    
    if (isTRUE(as.matrix)) {
      result <- split(result, result$id)
      rr <- lapply(result, function(x) {
        as.vector(x$chirps)
      })
      rr <- do.call(rbind, rr)
      
      newnames <-
        paste0("chirps-v2.0.", gsub("-", "\\.", result[[1]]$date))
      
      result <- as.matrix(rr)
      
      dimnames(result)[[2]] <- newnames
      
    }
    
    return(result)
    
  }
  
  if (server == "CHC") {
    days <- seq.Date(as.Date(dates[1]), as.Date(dates[2]), by = "day")
    span <- length(days)
    
    # get CHIRPS CoG files
    rr <- .get_CHIRPS_tiles_CHC(dates, ...)
    
    if (isTRUE(as.raster)) {
      result <- terra::crop(rr, y = object)
      return(result)
    } else{
      as.raster <- FALSE
    }
    
    if (isTRUE(as.matrix)) {
      result <- terra::extract(rr, y = object, ...)
      result$ID <- NULL
      return(result)
    } else{
      as.matrix <- FALSE
    }
    
    if (all(isFALSE(as.matrix), isFALSE(as.raster))) {
      result <- terra::extract(rr, y = object, ...)
      result$ID <- NULL
      result <- c(t(result))
      
      result <-
        data.frame(
          id = as.integer(rep(rownames(object), each = span)),
          lon = as.numeric(rep(object[, 1], each = span)),
          lat = as.numeric(rep(object[, 2], each = span)),
          date = rep(days, times = nrow(object)),
          chirps = as.numeric(result)
        )
      
      class(result) <- c("chirps", "chirps_df", class(result))
      
      return(result)
      
    }
    
  }
  
}

#' @rdname get_chirps
#' @method get_chirps SpatVector
#' @export
get_chirps.SpatVector <- function(object,
                                  dates,
                                  server = "CHC",
                                  as.raster = TRUE,
                                  ...) {
  dots <- list(...)
  as.matrix <- dots[["as.matrix"]]
  
  # get CHIRTS GeoTiff files
  rr <- .get_CHIRPS_tiles_CHC(dates, ...)
  
  if (isTRUE(as.raster)) {
    result <- terra::crop(rr, y = object)
    return(result)
  }
  
  if (isTRUE(as.matrix)) {
    result <- terra::extract(rr, y = object, ...)
    result$ID <- NULL
    return(result)
  }
  
  if (all(isFALSE(as.matrix), isFALSE(as.raster))) {
    days <- seq.Date(as.Date(dates[1]), as.Date(dates[2]), by = "day")
    span <- length(days)
    result <- terra::extract(rr, y = object, ...)
    ids <- result$ID
    result$ID <- NULL
    result <- c(t(result))
    
    result <- data.frame(
      id = as.integer(rep(ids, each = span)),
      lon = NA,
      lat = NA,
      date = rep(days, each = length(ids)),
      chirps = as.numeric(result)
    )
    
    class(result) <- c("chirps", "chirps_df", class(result))
    
    return(result)
    
  }
}

#' @rdname get_chirps
#' @method get_chirps SpatRaster
#' @export
get_chirps.SpatRaster <- function(object,
                                  dates,
                                  server = "CHC",
                                  as.matrix = TRUE,
                                  as.raster = FALSE,
                                  ...) {
  UseMethod("get_chirps", object = "SpatVector")
}


#' @rdname get_chirps
#' @method get_chirps SpatExtent
#' @export
get_chirps.SpatExtent <- function(object, dates, server = "CHC",
                                  as.matrix = TRUE, as.raster = FALSE, ...) {
  
  UseMethod("get_chirps", object = "SpatVector")
}


#' @rdname get_chirps
#' @method get_chirps sf
#' @export
get_chirps.sf <- function(object, dates, server,
                          as.sf = FALSE,
                          ...) {
  # check geometry type
  type <- c("POINT", "POLYGON")
  
  dots <- list(...)
  
  # check for supported types
  supp_type <-
    c(all(grepl(type[[1]], sf::st_geometry_type(object))),
      all(grepl(type[[2]], sf::st_geometry_type(object))))
  
  if (!any(supp_type)) {
    stop(
      "The sf geometry type is not supported.
         Please provide a sf object of geometry type 'POINT' or 'POLYGON'\n"
    )
  }
  
  type <- type[which(supp_type)]
  
  nr <- dim(object)[[1]]
  
  # find the sf_column
  index <- attr(object, "sf_column")
  
  if (type == "POINT") {
    # get the sf column
    lonlat <- object[[index]]
    # unlist the sf_column
    lonlat <- unlist(object[[index]])
    
  }
  
  if (type == "POLYGON") {
    # set centroid to validade lonlat
    lonlat <- sf::st_centroid(object)
    
    # unlist the sf_column
    lonlat <- unlist(lonlat)
    
    nr <- 1
    
  }
  
  lonlat <- matrix(
    lonlat,
    nrow = nr,
    ncol = 2,
    byrow = TRUE,
    dimnames = list(seq_len(nr), c("lon", "lat"))
  )
  
  lonlat <- as.data.frame(lonlat)
  
  # validate lonlat to check if they are within the CHIRPS range
  .validate_lonlat(lonlat, xlim = c(-180, 180), ylim = c(-50, 50))
  
  if (server == "ClimateSERV") {
    # validate and reformat dates
    dates_inter <-
      .reformat_dates(dates, availability = c("1981-01-01", "0"))
    
    operation <- dots[["operation"]]
    if (is.null(operation)) {
      operation <- 5
    }
    
    # get geojson strings from data.frame
    gj <- as.geojson(object, ...)
    
    class(gj) <- "character"
    
    gj <- split(gj, seq_along(gj))
    
    result <- .GET(
      gjson = gj,
      dates = dates_inter,
      operation = operation,
      datatype = 0
    )
    
    lonlat$id <- rownames(lonlat)
    
    result <- merge(result, lonlat, by = "id", all.x = TRUE)
    
    names(result)[3:5] <- c("chirps", "lon", "lat")
    
    result <- result[, c("id", "lon", "lat", "date", "chirps")]
    
    result <- as.data.frame(result, stringsAsFactors = FALSE)
    
    class(result) <- c("chirps", "chirps_df", class(result))
    
  }
  
  if (server == "CHC") {
    days <- seq.Date(as.Date(dates[1]), as.Date(dates[2]), by = "day")
    span <- length(days)
    
    # get CHIRPS CoG files
    rr <- .get_CHIRPS_tiles_CHC(dates, ...)
    
    result <- terra::extract(rr, y = lonlat, ...)
    result$ID <- NULL
    result <- c(t(result))
    
    result <-
      data.frame(
        id = as.integer(rep(rownames(lonlat), each = span)),
        lon = as.numeric(rep(lonlat[, 1], each = span)),
        lat = as.numeric(rep(lonlat[, 2], each = span)),
        date = rep(days, each = nrow(lonlat)),
        chirps = as.numeric(result)
      )
    
    class(result) <- c("chirps", "chirps_df", class(result))
    
  }
  
  if (isTRUE(as.sf)) {
    result$date <- as.integer(result$date)
    result$date <- paste0("day_", result$date)
    
    result <- split(result, result$date)
    
    result <- lapply(result, function(x) {
      x <- x[order(x$id),]
      x <- x[, "chirps"]
    })
    
    result <- do.call("cbind", result)
    
    result <- cbind(object, result)
    
  }
  
  return(result)
  
}

#' @rdname get_chirps
#' @method get_chirps geojson
#' @export
get_chirps.geojson <- function(object,
                               dates,
                               server,
                               as.geojson = FALSE,
                               ...) {
  dots <- list(...)
  
  type <- c("type\":\"Point", "type\":\"Polygon")
  
  # check for supported types
  supp_type <- c(all(grepl(type[[1]], object)),
                 all(grepl(type[[2]], object)))
  
  if (isFALSE(any(supp_type))) {
    stop(
      "The geojson geometry type is not supported.
         Please provide a geojson of geometry type 'Point' or 'Polygon'\n"
    )
  }
  
  # if type Point
  if (all(grepl(type[[1]], object))) {
    # get matrix with lonlat to validate later
    lonlat <- lapply(object, function(x) {
      # read as sf
      x <- sf::read_sf(x)
      
      # find the sf_column
      index <- attr(x, "sf_column")
      
      # unlist the sf_column
      x <- unlist(x[[index]])
      
    })
    
    # put all together
    lonlat <- do.call("rbind", lonlat)
    
    lonlat <- as.data.frame(lonlat)
    
  }
  
  # if Polygon
  if (all(grepl(type[[2]], object))) {
    # take the centroid from geojson Polygons
    # to validate lonlat coordinates
    lonlat <- lapply(object, function(x) {
      x <- sf::read_sf(x)
      
      x <- sf::st_centroid(x$geometry)
      
      x <- unlist(x)
    })
    
    # put all together
    lonlat <- do.call("rbind", lonlat)
    
    lonlat <- as.data.frame(lonlat)
    
  }
  
  # validate lonlat to check if they are within the CHIRPS range lat -50, 50
  .validate_lonlat(lonlat, xlim = c(-180, 180), ylim = c(-50, 50))
  
  if (server == "ClimateSERV") {
    # validate and reformat dates
    dates_inter <-
      .reformat_dates(dates, availability = c("1981-01-01", "0"))
    
    operation <- dots[["operation"]]
    if (is.null(operation)) {
      operation <- 5
    }
    
    # get geojson strings from data.frame
    gj <- split(object, seq_along(object))
    
    class(gj) <- "character"
    
    result <- .GET(
      gjson = gj,
      dates = dates_inter,
      operation = operation,
      datatype = 0
    )
    
    lonlat$id <- rownames(lonlat)
    
    result <- merge(result, lonlat, by = "id", all.x = TRUE)
    
    names(result)[3:5] <- c("chirps", "lon", "lat")
    
    result <- result[, c("id", "lon", "lat", "date", "chirps")]
    
    result <- as.data.frame(result, stringsAsFactors = FALSE)
    
    class(result) <- c("chirps", "chirps_df", class(result))
    
  }
  
  if (server == "CHC") {
    days <- seq.Date(as.Date(dates[1]), as.Date(dates[2]), by = "day")
    span <- length(days)
    
    # get CHIRPS CoG files
    rr <- .get_CHIRPS_tiles_CHC(dates, ...)
    
    result <- terra::extract(rr, y = lonlat, ...)
    result$ID <- NULL
    result <- c(t(result))
    
    result <-
      data.frame(
        id = as.integer(rep(rownames(lonlat), each = span)),
        lon = as.numeric(rep(lonlat[, 1], each = span)),
        lat = as.numeric(rep(lonlat[, 2], each = span)),
        date = rep(days, each = nrow(lonlat)),
        chirps = as.numeric(result)
      )
    
    class(result) <- c("chirps", "chirps_df", class(result))
    
  }
  
  
  if (isTRUE(as.geojson)) {
    result <- split(result, result$id)
    
    object <- split(object, seq_along(object))
    
    # add geojson properties
    result <- mapply(function(X, Y) {
      .add_geojson_properties(geometry = X,
                              properties = Y,
                              name = "chirps")
      
    }, X = object, Y = result[])
    
    class(result) <- c("geojson", "json", class(result))
    
    
  }
  
  return(result)
  
}


#' @rdname get_chirps
#' @method get_chirps SpatExtent
#' @export
get_chirps.SpatExtent <- function(object,
                                  dates,
                                  server = "CHC",
                                  as.raster = TRUE,
                                  ...) {
  # get CHIRTS GeoTiff files
  rr <- .get_CHIRPS_tiles_CHC(dates, ...)
  
  result <- terra::crop(rr, y = object)
  
  if (isFALSE(as.raster)) {
    result <- as.matrix(result)
    
  }
  
  return(result)
  
}

#' @noRd
.get_CHIRPS_tiles_CHC <- function(dates,
                                  resolution = 0.05,
                                  coverage = "global",
                                  interval = "daily",
                                  format = "cogs",
                                  ...) {
  message("\nFetching data as GeoTIFF files from CHC server \n")
  # setup file names
  .validate_dates(dates)
  seqdate <-
    seq.Date(as.Date(dates[1]), as.Date(dates[2]), by = "day")
  years <- format(seqdate, format = "%Y")
  dates <- gsub("-", "\\.", seqdate)
  fnames <- file.path(years, paste0("chirps-v2.0.", dates, ".cog"))
  
  # check for resolution
  if (!missing("resolution")) {
    if (!resolution %in% c(.05, .25)) {
      stop("Resolution must be .05 deg or .25 deg")
    }
  } else {
    resolution <- .05
    message("\nGetting CHIRPS in a .05 deg resolution \n")
  }
  
  resolution <- gsub("p0.", "p", paste0("p", resolution))
  u <- file.path(
    "https://data.chc.ucsb.edu/products/CHIRPS-2.0",
    paste0(coverage, "_", interval),
    format,
    resolution,
    fnames
  )
  u1 <- file.path("/vsicurl", u)
  r <- terra::rast(u1)
  return(r)
}
