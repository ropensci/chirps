#' Get evaporative stress index (ESI) data
#' 
#' Get evaporative stress index (\acronym{ESI}) from \acronym{SERVIR} Global
#' via ClimateSERV \acronym{API} Client. \acronym{ESI} is available every four
#'  (or twelve) weeks from 2001 to present.
#' The dataset may contain cloudy data which is returned as \code{NA}s.
#' ClimateSERV works with geojson of type 'Polygon'. The input \code{object} is
#'  then transformed into polygons with a small buffer area around the point.
#' 
#' @inheritParams get_chirps
#' @param period an integer value for the period of ESI data, four weeks period = 1, twelve weeks = 2
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
#' @return A data frame of \acronym{ESI} data:
#' \item{id}{the index for the rows in \code{object}}
#' \item{dates}{the dates from which ESI was requested}
#' \item{lon}{the longitude as provided in \code{object}}
#' \item{lat}{the latitude as provided in \code{object}}
#' \item{esi}{the ESI value}
#' 
#' @references
#' ClimateSERV \url{https://climateserv.servirglobal.net}
#' 
#' Evaporative Stress Index
#' \url{http://catalogue.servirglobal.net/Product?product_id=198}
#' 
#' @examples
#' \donttest{
#' library("chirps")
#' 
#' lonlat <- data.frame(lon = c(-55.0281,-54.9857, -55.0714),
#'                      lat = c(-2.8094, -2.8756, -3.5279))
#' 
#' dates <- c("2017-12-15","2018-06-20")
#' 
#' # by default the function set a very small buffer around the points
#' # which can return NAs due to cloudiness in ESI data
#' 
#' dat <- get_esi(lonlat, dates = dates)
#' 
#' dat
#' 
#' # the argument dist passed through sf increase the buffer area
#' 
#' dat <- get_esi(lonlat, dates = dates, dist = 0.1)
#' 
#' dat
#' 
#' ############################################
#' 
#' # S3 method for objects of class 'sf'
#' library("sf")
#' 
#' # geometry 'POINT'
#' lonlat <- data.frame(lon = c(-55.0281,-54.9857, -55.0714),
#'                      lat = c(-2.8094, -2.8756, -3.5279))
#' 
#' lonlat <- st_as_sf(lonlat, coords = c("lon","lat"))
#' 
#' dates <- c("2017-11-15", "2017-12-31")
#' 
#' get_esi(lonlat, dates)
#' 
#' # as.sf = TRUE returns an object of class 'sf'
#' get_esi(lonlat, dates, as.sf = TRUE)
#' 
#' # geometry 'POLYGON'
#' p1 <- matrix(c(10.67, 49.90, 
#'                10.57, 49.80, 
#'                10.47, 49.90, 
#'                10.57, 50.00, 
#'                10.67, 49.90), 
#'              nrow = 5, ncol = 2, byrow = TRUE)
#' 
#' p1 <- st_polygon(list(p1))
#' 
#' p2 <- matrix(c(11.67, 45.59, 
#'                11.57, 45.49, 
#'                11.47, 45.59, 
#'                11.57, 45.69, 
#'                11.67, 45.59),
#'              nrow = 5, ncol = 2, byrow = TRUE)
#' 
#' p2 <- st_polygon(list(p2))
#' 
#' pol <- data.frame(x = c(1, 2))
#' pol$geometry = st_sfc(p1, p2)
#' 
#' pol <- st_as_sf(pol)
#' 
#' get_esi(pol, dates = c("2018-01-01", "2018-02-20"))
#' 
#' 
#' ############################################
#' 
#' # S3 method for objects of class 'geojson'
#' library("sf")
#' 
#' tapajos <- chirps:::tapajos
#' 
#' object <- chirps:::.sf_to_geojson(tapajos[1:2,])
#' 
#' object <- unlist(object)
#' 
#' class(object) <- c("geojson", "json", class(object))
#' 
#' dates <- c("2018-01-01","2018-01-20")
#' 
#' get_esi(object, dates)
#' 
#' # as.geojson = TRUE returns an object of class 'geojson'
#' get_esi(object, dates, as.geojson = TRUE)
#' } 
#' @export
get_esi <- function(object, dates, operation = 5, period = 1, 
                    ...) {
  
  UseMethod("get_esi")
  
}

#' @rdname get_esi
#' @export
get_esi.default <- function(object, dates, operation = 5, period = 1, 
                            ...) {
  
  # validate lonlat to check if they are within the CHIRPS range lat -50, 50
  .validate_lonlat(object, xlim = c(-180, 180), ylim = c(-50, 50))
  
  # validate dates
  dates_inter <- .reformat_dates(dates, availability = c("2001-01-01", "0"))
  
  # get geojson strings from data.frame
  gj <- .dataframe_to_geojson(object, ...)
  
  if (period == 1) {
    datatype <- 29
  }
  if (period == 2) {
    datatype <- 33
  }
  
  result <- .GET(gjson = gj, 
                 dates = dates_inter, 
                 operation = operation, 
                 datatype = datatype)
  
  names(result)[names(result) == "value"] <- "esi"
  
  object$id <- rownames(object)
  
  result <- merge(result, object, by = "id", all.y = TRUE)
  
  names(result)[3:5] <- c("esi", "lon", "lat")
  
  result <- result[, c("id", "lon", "lat", "date", "esi")]
  
  result <- tibble::as_tibble(result)
  
  return(result)
  
}

#' @rdname get_esi
#' @method get_esi sf
#' @export
get_esi.sf <- function(object, dates, operation = 5, period = 1, 
                       as.sf = FALSE,
                       ...) {
  
  # check geometry type
  type <- c("POINT", "POLYGON")
  
  # check for supported types 
  supp_type <- c(all(grepl(type[[1]], sf::st_geometry_type(object))),
                 all(grepl(type[[2]], sf::st_geometry_type(object))))
  
  if (!any(supp_type)) {
    stop("The sf geometry type is not supported. 
         Please provide a sf object of geometry type 'POINT' or 'POLYGON'\n")
  }
  
  type <- type[which(supp_type)]
  
  n <- nrow(object)
  
  # find the sf_column
  index <- attr(object, "sf_column")
  
  if (type == "POINT") {
    
    # unlist the sf_column
    lonlat <- unlist(object[[index]])
    
    lonlat <- matrix(lonlat,
                     nrow = n,
                     ncol = 2, 
                     byrow = TRUE, 
                     dimnames = list(1:n, c("lon","lat")))
    
    lonlat <- as.data.frame(lonlat)
    
  }
  
  if (type == "POLYGON") {
    
    lonlat <- object[[index]]
    
    nl <- length(lonlat)
    
    # set centroid to validade lonlat
    lonlat <- sf::st_centroid(lonlat)
    
    # unlist the sf_column
    lonlat <- unlist(lonlat)
    
    lonlat <- matrix(lonlat,
                     nrow = nl,
                     ncol = 2, 
                     byrow = TRUE, 
                     dimnames = list(1:nl, c("lon","lat")))
    
    lonlat <- as.data.frame(lonlat)
    
  }
  
  # validate lonlat to check if they are within the CHIRPS range
  .validate_lonlat(lonlat, xlim = c(-180, 180), ylim = c(-50, 50))
  
  # validate and reformat dates
  dates_inter <- .reformat_dates(dates, availability = c("2001-01-01", "0"))
  
  # get geojson strings from data.frame
  gj <- .sf_to_geojson(object, ...)
  
  
  if (period == 1) {
    datatype <- 29
  }
  if (period == 2) {
    datatype <- 33
  }
  
  result <- .GET(gjson = gj, 
                 dates = dates_inter, 
                 operation = operation, 
                 datatype = datatype)
  
  if (as.sf) {
    
    result$date <- as.integer(result$date)
    result$date <- paste0("d_",result$date)
    
    result <- split(result, result$date)
    
    result <- lapply(result, function(x) {
      x <- x[order(x$id), ]
      x <- x[, "value"] 
    })
    
    result <- do.call("cbind", result)
    
    result <- cbind(object, result)
    
  } else {
    
    lonlat$id <- rownames(lonlat)
    
    result <- merge(result, lonlat, by = "id")
    
    names(result)[3:5] <- c("esi", "lon", "lat")
    
    result <- result[, c("id", "lon", "lat", "date", "esi")]
    
    result <- tibble::as_tibble(result)

    
  }
  
  return(result)
  
}

#' @rdname get_esi
#' @method get_esi geojson
#' @export
get_esi.geojson <- function(object, dates, operation = 5, period = 1, 
                            as.geojson = FALSE, 
                            ...) {
  
  # check for the geometry tag
  if (!grepl("geometry", object[[1]])) {
    stop("geometry tag is missing in the geojson object\n")
  }
  
  type <- c("type\":\"Point", "type\":\"Polygon")
  
  # check for supported types 
  supp_type <- c(all(grepl(type[[1]], object)),
                 all(grepl(type[[2]], object)))
  
  if (!any(supp_type)) {
    stop("The geojson geometry type is not supported. 
         Please provide a geojson of geometry type 'Point' or 'Polygon'\n")
  }
  
  # if type Point
  if (all(grepl(type[[1]], object))) {
    
    # get matrix with lonlat to validate later
    lonlat <- lapply(object, function (x) {
      
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
    gjson <- .dataframe_to_geojson(lonlat, ...)
    
  }
  
  # if Polygon
  if (all(grepl(type[[2]], object))) {
    
    # take the centroid from geojson Polygons
    # to validate lonlat coordinates
    lonlat <- lapply(object, function (x) {
      
      x <- sf::read_sf(x)
      
      x <- sf::st_centroid(x$geometry)
      
      x <- unlist(x)
    })
    
    # put all together
    lonlat <- do.call("rbind", lonlat)
    
    lonlat <- as.data.frame(lonlat)
    
    gjson <- split(object, 1:length(object))
    
  }
  
  # validate lonlat to check if they are within the CHIRPS range lat -50, 50
  .validate_lonlat(lonlat, xlim = c(-180, 180), ylim = c(-50, 50))
  
  # validate dates
  dates_inter <- .reformat_dates(dates, availability = c("2001-01-01", "0"))
  
  if (period == 1) {
    datatype <- 29
  }
  if (period == 2) {
    datatype <- 33
  }
  
  result <- .GET(gjson = gjson,
                 dates = dates_inter,
                 operation = operation,
                 datatype = datatype)
  
  
  if(as.geojson){
    
    result <- split(result, result$id)
    
    object <- split(object, 1:length(object))
    
    # add geojson properties
    result <- mapply(function(X, Y) {
      
      .add_geojson_properties(geometry = X,
                              properties = Y,
                              name = "esi")
      
    }, X = object, Y = result[])
    
    class(result) <- c("geojson", "json", class(result))
    
    
  }
  
  if (!as.geojson) {
    
    lonlat$id <- rownames(lonlat)
    
    result <- merge(result, lonlat, by = "id")
    
    names(result)[3:5] <- c("esi", "lon", "lat")
    
    result <- result[, c("id", "lon", "lat", "date", "esi")]
    
    result <- tibble::as_tibble(result)

  }
  
  return(result)
  
}