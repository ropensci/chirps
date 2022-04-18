#' Get Integrated Multisatellite Retrievals for GPM (IMERG) data
#'
#'  The IMERG dataset provides near-real time global observations of rainfall at
#'  10km resolution, which can be used to estimate total rainfall accumulation
#'  from storm systems and quantify the intensity of rainfall and flood impacts
#'  from tropical cyclones and other storm systems. \acronym{IMERG} is a daily
#'  precipitation dataset available from 2015 to present within the latitudes 70
#'  and -70 degrees.
#'
#' @inheritParams get_chirps
#' @param operation optional, an integer that represents which type of
#' statistical operation to perform on the dataset
#' @details
#'  \bold{operation}: supported operations are:
#'  \tabular{rll}{
#'  \bold{operation}      \tab    \tab \bold{value}\cr
#'  max                   \tab =  \tab 0\cr
#'  min                   \tab =  \tab 1\cr
#'  median                \tab =  \tab 2\cr
#'  sum                   \tab =  \tab 4\cr
#'  average               \tab =  \tab 5 (\emph{default value})\cr
#'  }
#'
#' \bold{dist}: numeric, buffer distance for each \code{object} coordinate
#'
#' \bold{nQuadSegs}: integer, number of segments per buffer quadrant
#'
#' @return A data frame of \acronym{imerg} data:
#' \item{id}{the index for the rows in \code{object}}
#' \item{dates}{the dates from which imerg was requested}
#' \item{lon}{the longitude as provided in \code{object}}
#' \item{lat}{the latitude as provided in \code{object}}
#' \item{imerg}{the IMERG value}
#'
#' @examplesIf interactive()
#' lonlat <- data.frame(lon = c(-55.0281,-54.9857),
#'                      lat = c(-2.8094, -2.8756))
#'
#' dates <- c("2017-12-15", "2017-12-31")
#'
#' dt <- get_imerg(lonlat, dates)
#'
#' dt
#'
#' @importFrom sf st_centroid read_sf st_geometry_type
#' @export
get_imerg <- function(object, dates, operation = 5, ...) {
  UseMethod("get_imerg")
  
}

#' @rdname get_imerg
#' @export
get_imerg.default <- function(object, dates, operation = 5, ...) {
  object <- as.data.frame(object)
  
  # validate lonlat to check if they are within the IMERG range lat -60, 60
  .validate_lonlat(object, xlim = c(-180, 180), ylim = c(-70, 70))
  
  # validate dates
  dates_inter <-
    .reformat_dates(dates, availability = c("2015-01-01", "0"))
  
  # get geojson strings from data.frame
  gj <- as.geojson(object, ...)
  
  class(gj) <- "character"
  
  gj <- split(gj, seq_along(gj))
  
  result <- .GET(
    gjson = gj,
    dates = dates_inter,
    operation = operation,
    datatype = 26
  )
  
  names(result)[names(result) == "value"] <- "imerg"
  
  object$id <- rownames(object)
  
  result <- merge(result, object, by = "id", all.y = TRUE)
  
  names(result)[3:5] <- c("imerg", "lon", "lat")
  
  result <- result[, c("id", "lon", "lat", "date", "imerg")]
  
  result <- as.data.frame(result, stringsAsFactors = FALSE)
  
  class(result) <- union(c("chirps_df", "chirps"), class(result))
  
  return(result)
  
}

#' @rdname get_imerg
#' @method get_imerg sf
#' @export
get_imerg.sf <-
  function(object,
           dates,
           operation = 5,
           as.sf = FALSE,
           ...) {
    # check geometry type
    type <- c("POINT", "POLYGON")
    
    # check for supported types
    supp_type <-
      c(all(grepl(type[[1]], sf::st_geometry_type(object))),
        all(grepl(type[[2]], sf::st_geometry_type(object))))
    
    if (isFALSE(any(supp_type))) {
      stop(
        "The sf geometry type is not supported.
         Please provide a sf object of geometry type 'POINT' or 'POLYGON'\n"
      )
    }
    
    type <- type[which(supp_type)]
    
    nr <- dim(object)[[1]]
    
    # find the sf_column
    index <- attr(object, "sf_column")
    
    # get the sf column
    lonlat <- object[[index]]
    
    if (isTRUE(type == "POINT")) {
      # unlist the sf_column
      lonlat <- unlist(object[[index]])
      
    }
    
    if (isTRUE(type == "POLYGON")) {
      # set centroid to validade lonlat
      lonlat <- sf::st_centroid(lonlat)
      
      # unlist the sf_column
      lonlat <- unlist(lonlat)
      
    }
    
    lonlat <- matrix(
      lonlat,
      nrow = nr,
      ncol = 2,
      byrow = TRUE,
      dimnames = list(seq_len(nr), c("lon", "lat"))
    )
    
    lonlat <- as.data.frame(lonlat)
    
    # validate lonlat to check if they are within the imerg range
    .validate_lonlat(lonlat, xlim = c(-180, 180), ylim = c(-70, 70))
    
    # validate and reformat dates
    dates_inter <-
      .reformat_dates(dates, availability = c("2015-01-01", "0"))
    
    # get geojson strings from data.frame
    gj <- as.geojson(object, ...)
    
    class(gj) <- "character"
    
    gj <- split(gj, seq_along(gj))
    
    result <- .GET(
      gjson = gj,
      dates = dates_inter,
      operation = operation,
      datatype = 26
    )
    
    if (isTRUE(as.sf)) {
      result$date <- as.integer(result$date)
      result$date <- paste0("day_", result$date)
      
      result <- split(result, result$date)
      
      result <- lapply(result, function(x) {
        x <- x[order(x$id),]
        x <- x[, "value"]
      })
      
      result <- do.call("cbind", result)
      
      result <- cbind(object, result)
      
    }
    
    if (isFALSE(as.sf)) {
      lonlat$id <- rownames(lonlat)
      
      result <- merge(result, lonlat, by = "id")
      
      names(result)[3:5] <- c("imerg", "lon", "lat")
      
      result <- result[, c("id", "lon", "lat", "date", "imerg")]
      
      result <- as.data.frame(result, stringsAsFactors = FALSE)
      
      class(result) <- union(c("chirps_df", "chirps"), class(result))
      
      
    }
    
    return(result)
    
  }

#' @rdname get_imerg
#' @method get_imerg geojson
#' @export
get_imerg.geojson <- function(object,
                              dates,
                              operation = 5,
                              as.geojson = FALSE,
                              ...) {
  
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
    
    # lonlat into a geojson Polygon
    gjson <- as.geojson(lonlat, ...)
    
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
    
    gjson <- split(object, seq_along(object))
    
  }
  
  # validate lonlat to check if they are within the IMERG range lat -60, 60
  .validate_lonlat(lonlat, xlim = c(-180, 180), ylim = c(-70, 70))
  
  # validate dates
  dates_inter <-
    .reformat_dates(dates, availability = c("2015-01-01", "0"))
  
  result <- .GET(
    gjson = gjson,
    dates = dates_inter,
    operation = operation,
    datatype = 26
  )
  
  
  if (isTRUE(as.geojson)) {
    result <- split(result, result$id)
    
    object <- split(object, seq_along(object))
    
    # add geojson properties
    result <- mapply(function(X, Y) {
      .add_geojson_properties(geometry = X,
                              properties = Y,
                              name = "imerg")
      
    }, X = object, Y = result[])
    
    class(result) <- c("geojson", "json", class(result))
    
    
  }
  
  if (isFALSE(as.geojson)) {
    lonlat$id <- rownames(lonlat)
    
    result <- merge(result, lonlat, by = "id")
    
    names(result)[3:5] <- c("imerg", "lon", "lat")
    
    result <- result[, c("id", "lon", "lat", "date", "imerg")]
    
    result <- as.data.frame(result, stringsAsFactors = FALSE)
    
    class(result) <- union(c("chirps_df", "chirps"), class(result))
    
  }
  
  return(result)
  
}
