#' Send a request to ClimateSERV
#'
#' @param datatype integer, the unique datatype number for the 
#'  dataset which this request operates on
#' @param begintime character, start date for processing interval, 
#'  in the format ("MM/DD/YYYY")
#' @param endtime character, end date for processing interval, format
#'  ("MM/DD/YYYY")
#' @param intervaltype integer, value that represents which type of time
#'  interval to process
#' @param operationtype integer, value that represents which type of 
#'  statistical operation to perform. Defaults to \code{5}.
#' @param geometry a geojson for the geometry that is defined by the user 
#'  on the current client
#' @return A id to be used in the data request
#' @details
#' datatype codes are described at 
#'  https://climateserv.servirglobal.net/help
#' 
#' operation: supported operations are max = 0, min = 1, 
#'  median = 2, sum = 4, average = 5
#' @examples
#' lonlat = data.frame(lon = c(-60.34),
#'                      lat = c(-5.38))
#' 
#' gjson = as.geojson(lonlat)
#' 
#' chirps:::.send_request(begintime = "12/10/2018",
#'                        endtime = "12/26/2018",
#'                        geometry = gjson)
#'                         
#' @importFrom httr RETRY accept_json content
#' @noRd
.send_request = function(datatype = 0,
                          begintime = NULL,
                          endtime = NULL,
                          intervaltype = 0,
                          operationtype = 5,
                          geometry = NULL) {
  
    #base_url = "https://climateserv.servirglobal.net/api/"
    
    # organise the query
    query = list(
      datatype = toString(datatype),
      begintime = begintime,
      endtime = endtime,
      intervaltype = toString(intervaltype),
      operationtype = toString(operationtype),
      callback = "successCallback",
      dateType_Category = "default",
      isZip_CurrentDataType = "false",
      geometry = toString(geometry)
    )
    
    # client_request =
    #   crul::HttpClient$new(url = paste0(base_url, "submitDataRequest/?"))
    
    # check status
    # status = client_request$get()
    # status$raise_for_status()
    
    # send the query
    tryCatch({
      # id = client_request$get(query = query),
      # nocov start
      id = httr::RETRY(verb = "GET", 
                        url = "https://climateserv.servirglobal.net/api/submitDataRequest/?",
                        query = query,
                        httr::accept_json(),
                        httr::user_agent(.chirps_ua()),
                        terminate_on = c(403, 404))
      
      id = httr::content(id, as = "text", encoding = "UTF-8")
      
      }, 
      
      error = function(e) {
        e$message =
          paste0(
            "Something went wrong with the query, no data were returned. ",
            "Most likely the server is down. Please see ",
            "<https://climateserv.servirglobal.net/> for potential ",
            "server issues.\n"
          )
        # Otherwise refers to open.connection
        e$call = NULL
        stop(e)
      }
    ) # nocov end
    
    # id = id$parse("UTF-8")
    
    # get content from the query
    id = strsplit(id, '["]')[[1]][2]
    
    return(id)
    
}

#' Get request progress
#'
#' @param id character with the id obtained from \code{.send_request}
#' @return logical value, \code{TRUE} when the data is ready to be retrieved
#' @examples
#' 
#' lonlat = data.frame(lon = runif(1, 10, 12),
#'                      lat = runif(1, 45, 47))
#' 
#' gjson = as.geojson(lonlat)
#' 
#' gjson = as.character(gjson)
#' 
#' gsjon = split(gjson, seq_along(gjson))
#' 
#' id = chirps:::.send_request(begintime = "12/10/2018",
#'                              endtime = "12/26/2018",
#'                              geometry = gjson)
#' 
#' chirps:::.get_request_progress(id)
#' @noRd
.get_request_progress = function(id) {
  
  # base_url = "https://climateserv.servirglobal.net/api/"
  # 
  # client_progress =
  #   crul::HttpClient$new(url = paste0(base_url, 
  #                                     "getDataRequestProgress/?"))
  # 
  # 
  # # organise the query
  progress_query = list(id = id)
  # 
  # # check status
  # status = client_progress$get()
  # 
  # # send query
  # client_progress$get(query = progress_query, retry = 6)
  # 
  # p = client_progress$get(query = progress_query)
  # 
  # p = p$parse("UTF-8")
  # 
  # account for the chance that the server returns 
  # an error message and stop
  # the process with a (hopefully) useful error message
  
  p = httr::RETRY(verb = "GET", 
                   url = "https://climateserv.servirglobal.net/api/getDataRequestProgress/?",
                   query = progress_query,
                   httr::accept_json(),
                   httr::user_agent(.chirps_ua()),
                   terminate_on = c(403, 404))
  
  p = httr::content(p, as = "text", encoding = "UTF-8")
  
  if (p == -1) { #nocov start
    stop(call. = FALSE,
    "Something went wrong with the query, no data were returned. ",
    "Most likely the server is down. Please see ",
    "<https://climateserv.servirglobal.net/> for potential ",
    "server issues.\n"
    )
  } #nocov end
  
  p = grepl(100, p)
  
  return(p)
}

#' Get data from a request to ClimateSERV
#'
#' @param id character with the id obtained from \code{.send_request}
#' @return A data frame with requested data
#' @examples
#' lonlat = data.frame(lon = runif(1, 10, 12),
#'                      lat = runif(1, 45, 47))
#' 
#' gjson = as.geojson(lonlat)
#' 
#' gjson = as.character(gjson)
#' 
#' gsjon = split(gjson, seq_along(gjson))
#' 
#' id = chirps:::.send_request(begintime = "12/10/2018",
#'                              endtime = "12/26/2018",
#'                              geometry = gjson)
#' 
#' chirps:::.get_data_from_request(id)
#' 
#' @importFrom jsonlite fromJSON toJSON
#' @noRd
.get_data_from_request = function(id) {
  
  # base_url = "https://climateserv.servirglobal.net/api/"
  # 
  # client_data =
  #   crul::HttpClient$new(url = paste0(base_url, "getDataFromRequest/?"))
  # 
  # organise the query
  query = list(id = id)
  
  # # check status
  # status = client_data$get()
  # status$raise_for_status()
  # 
  
  tryCatch({
    # client_data$get(query = query, retry = 6)
    # 
    # # send query
    # d = client_data$get(query = query)
    # 
    # d = d$parse("UTF-8")
    
    d = httr::RETRY(verb = "GET", 
                      url = "https://climateserv.servirglobal.net/api/getDataFromRequest/?",
                      query = query,
                      httr::accept_json(),
                      httr::user_agent(.chirps_ua()),
                      terminate_on = c(403, 404))
    
    d = httr::content(d, as = "text", encoding = "UTF-8")
    
    
    d = jsonlite::fromJSON(d)
    
  }, # nocov start
  error = function(e) {
    e$message =
      paste0(
        "Something went wrong with the query, no data were returned. ",
        "Most likely the server is down. Please see ",
        "<https://climateserv.servirglobal.net/> for potential ",
        "server issues.\n"
      )
    # Otherwise refers to open.connection
    e$call = NULL
    stop(e)
  }) # nocov end
  
  d = data.frame(cbind(date = d$data$date,
                        d$data$value))
  
  d$date = as.character(d$date)
  
  
  return(d)
  
}

#' Validate lonlat within an pre-defined bounding box
#'
#' @param lonlat a \code{\link[base]{data.frame}} with geographical
#'  coordinates lonlat in that order
#' @param xlim a numeric vector for the min and max accepted range in the
#'  longitude in that order
#' @param ylim a numeric vector for the min and max accepted range in the
#'  latitude in that order
#' @return If lonlat are valid (within the bounding box) returns nothing
#' @examples
#' # random geographic locations around bbox(10, 12, 45, 57)
#' set.seed(123)
#' lonlat = data.frame(lon = runif(10, 10, 12),
#'                      lat = runif(10, 45, 49))
#'
#' .validate_lonlat(lonlat)
#' @noRd
.validate_lonlat = function(lonlat,
                             xlim = c(-180, 180),
                             ylim = c(-50, 50)) {
  lon = lonlat[, 1]
  
  lat = lonlat[, 2]
  
  v1 = min(lon) < xlim[1]
  v2 = max(lon) > xlim[2]
  v3 = min(lat) < ylim[1]
  v4 = max(lat) > ylim[2]
  
  if (any(c(v1, v2, v3, v4))) {
    stop("Some coordinates are outside the area this product covers, ",
         "longitude ", xlim[1], " to ", xlim[2], " and latitude ",
         ylim[1], " to ", ylim[2], ". Check that the first column is ",
         "longitude and the second is latitude.\n", call. = FALSE)
  }
  
  invisible(lonlat)
}

#' Latitude range covered by CHIRPS
#'
#' CHIRPS v3 covers 60S to 60N, CHIRPS v2 (also served by ClimateSERV)
#' covers 50S to 50N.
#'
#' @param version the CHIRPS version, NULL for the default (3.0)
#' @param server the server, "CHC" or "ClimateSERV"
#' @return a numeric vector of length two
#' @noRd
.chirps_ylim = function(version = NULL, server = "CHC") {
  v2 = identical(server, "ClimateSERV") ||
    (!is.null(version) && as.character(version) %in% c("2", "2.0"))
  if (v2) {
    return(c(-50, 50))
  }
  c(-60, 60)
}

#' Name the columns of a wide matrix by date
#'
#' Columns named "YYYY-MM-DD" are the input that climatrends expects, e.g.
#' climatrends::rainfall() and climatrends::temperature().
#'
#' @param m a matrix with one column per day
#' @param days Date vector, one per column
#' @return the matrix with column names "YYYY-MM-DD"
#' @noRd
.date_colnames = function(m, days) {
  m = as.matrix(m)
  attr(m, "ID") = NULL
  if (ncol(m) == length(days)) {
    colnames(m) = format(days)
  }
  m
}

#' Show a message once per session
#'
#' @param key character, the name of the notice
#' @param ... the message, passed to message()
#' @return nothing, called for its side effect
#' @noRd
.notice_once = function(key, ...) {
  if (isTRUE(.chirps_env[[key]])) {
    return(invisible(NULL))
  }
  assign(key, TRUE, envir = .chirps_env)
  message(...)
  invisible(NULL)
}


#' Turn the dates argument into a start and end date
#'
#' @param x the \code{dates} argument as given by the user, a character or
#'  Date vector with one date (a single day) or two dates (start and end)
#' @return a character vector of length two, "YYYY-MM-DD"
#' @examples
#' .as_dates("2010-12-15")
#' .as_dates(as.Date(c("2010-12-15", "2010-12-31")))
#' @noRd
.as_dates = function(x) {
  if (inherits(x, "Date")) {
    x = format(x, "%Y-%m-%d")
  }
  
  if (!is.character(x)) {
    stop("`dates` must be a character or Date vector with the start and ",
         "end dates, \"YYYY-MM-DD\", but an object of class '",
         paste(class(x), collapse = "', '"), "' was given. ",
         "Check that the dates are passed to `dates` and not to another ",
         "argument.\n", call. = FALSE)
  }
  
  if (length(x) == 1) {
    x = c(x, x)
  }
  
  if (length(x) != 2) {
    stop("`dates` must have one date (a single day) or two dates ",
         "(start and end), not ", length(x), ".\n", call. = FALSE)
  }
  
  d = as.Date(x, format = "%Y-%m-%d")
  
  if (any(is.na(d))) {
    stop("`dates` must be in the format \"YYYY-MM-DD\", but got \"",
         paste(x, collapse = "\", \""), "\".\n", call. = FALSE)
  }
  
  format(d, "%Y-%m-%d")
}

#' Validate dates within an accepted range
#'
#' @param x a character of start and end dates in that order in the format
#'  "YYYY-MM-DD", or a single date
#' @param availability a character for the dates the dataset is available
#' @return nothing
#' @examples
#' dates = c("2016-01-31","2017-12-01")
#'
#' .validate_dates(dates)
#'
#' dates = c("2018-01-31","2017-12-01")
#'
#' .validate_dates(dates)
#' @noRd
.validate_dates = function(x, availability = c("1981-01-01", "0")) {
  x = .as_dates(x)
  
  xmin = as.Date(x[1], format = "%Y-%m-%d")
  
  xmax = as.Date(x[2], format = "%Y-%m-%d")
  
  # the first day from which the dataset is available
  past = as.Date(availability[1], origin = "1970-01-01")
  
  # the most recent date from which the dataset is available
  present = availability[2]
  
  # generally it takes 45 days to update
  if (present == "0") {
    present = Sys.Date() - 45
  }
  
  present = as.Date(present)
  
  if (xmax < xmin) {
    stop("Please check your dates. The start date (", x[1],
         ") is after the end date (", x[2], ").\n", call. = FALSE)
  }
  
  if (xmin < past || xmax > present) {
    stop("Please check your dates. The dataset is available from ",
         format(past), " to about ", format(present), ", but ",
         x[1], " to ", x[2], " was requested.\n", call. = FALSE)
  }
  
  invisible(x)
}


#' Reformat dates as required by ClimateServ
#'
#' @param x a character of start and end dates in that order in the format
#'  "YYYY-MM-DD"
#' @param ... further arguments passed to \code{\link{.validate_dates}}
#' @return a character with reformated dates as "MM/DD/YYYY"
#' @examples
#' x = c("2016-01-31","2017-12-01")
#'
#' .reformat_dates(x)
#' @noRd
.reformat_dates = function(x, ...) {
  # validate dates
  x = .validate_dates(x, ...)
  
  begindate = x[1]
  begindate = strsplit(begindate, "-")[[1]]
  begindate =
    paste(begindate[2], begindate[3], begindate[1], sep = "/")
  
  enddate = x[2]
  enddate = strsplit(enddate, "-")[[1]]
  enddate = paste(enddate[2], enddate[3], enddate[1], sep = "/")
  
  dates = c(begindate, enddate)
  
  return(dates)
  
}


#' Check if contains class "chirps"
#' @param x an object to test
#' @return logical, \code{TRUE} for an object of class 'chirps'
#' @examples
#' .is_chirps(airquality)
#' @noRd
.is_chirps = function(x) {
  c("chirps") %in% class(x)
  
}


#' Add feature properties to a geojson geometry 
#' 
#' @param geometry a geojson geometry
#' @param properties a data.frame with feature properties
#' @param name a characer for the feature properties name
#' @return a object of class geojson with FeatureCollection
#' @examples
#' set.seed(123)
#' lonlat = data.frame(lon = 1,
#'                      lat = 1)
#' 
#' geometry = as.geojson(lonlat)[[1]]
#' 
#' properties = data.frame(x = LETTERS[1:3],
#'                          a = as.character(1:3),
#'                          z = colors()[1:3])
#' 
#' name = "chirps"
#' 
#' gjson = .add_geojson_properties(geometry, properties, name)
#' 
#' gjson
#' 
#' # check if it can be parsed to sf
#' sf::st_read(gjson)
#' 
#' # check if it can be parsed to jsonlite
#' jsonlite::fromJSON(gjson)
#' 
#' @noRd
.add_geojson_properties = function(geometry, properties, name) {
  
  # extract the geometry
  g = geometry
  
  g = jsonlite::fromJSON(g)
  
  # coerce coordinates to character to prevent toJSON to 
  # suppress floating numbers
  g$geometry$coordinates = as.character(g$geometry$coordinates)
  
  # coerce the geometries back to json
  g = jsonlite::toJSON(g$geometry)
  
  # now convert the properties into json
  p = properties
  
  nr = dim(p)[[1]]
  
  nc = dim(p)[[2]]
  
  # coerce values into characters
  p[seq_len(nc)] = lapply(p[seq_len(nc)], as.character)
  
  p = split(p, seq_len(nr))
  
  p = jsonlite::toJSON(p)
  
  p = gsub("[[]", "", p)
  
  p = gsub("[]]", "", p)
  
  header = paste0("{\"type\":\"FeatureCollection\",\"name\":\"",
                   name,
                   "\",\"features\":[{\"type\":\"Feature\",\"properties\":")
  
  header2 = ",\"geometry\":"
  
  end = "}]}"
  
  gjson = paste0(header, p, header2, g, end)
  
  class(gjson) = c("geojson", "json")
  
  return(gjson)
  
}

#' Add %notin% function
#'
#' Negates `%in%` for easier (mis)matching.
#'
#' @param x A character string to match.
#' @param table A table containing values to match `x` against.
#'
#' @return A logical vector, indicating if a mismatch was located for any
#'  element of `x`: thus the values are `TRUE` or `FALSE` and never `NA`.
#' @keywords internal
#' @noRd
`%notin%` = function(x, table) {
  match(x, table, nomatch = 0L) == 0L
}

#' Reshape extracted raster values into the long chirps format
#'
#' @param values a data.frame or matrix with one row per location and one
#'  column per day, as returned by \code{terra::extract()} without the ID
#'  column
#' @param ids integer ids, one per row of \code{values}
#' @param lon,lat numeric coordinates, one per row of \code{values} or a
#'  single value (e.g. NA) recycled to all rows
#' @param days a Date vector, one per column of \code{values}
#' @param name the name of the value column
#' @return a data.frame with columns id, lon, lat, date and \code{name},
#'  ordered by id and then by date
#' @examples
#' v = data.frame(d1 = c(1, 10), d2 = c(2, 20))
#' .to_long(v, 1:2, c(-55, -54), c(-2, -3), as.Date("2020-01-01") + 0:1,
#'          "chirps")
#' @noRd
.to_long = function(values, ids, lon, lat, days, name) {
  values = as.matrix(values)
  n = nrow(values)
  span = length(days)
  
  if (ncol(values) != span) {
    stop("The number of layers returned does not match the number of days",
         call. = FALSE)
  }
  
  result = data.frame(
    id = rep(as.integer(ids), each = span),
    lon = rep(rep_len(as.numeric(lon), n), each = span),
    lat = rep(rep_len(as.numeric(lat), n), each = span),
    date = rep(days, times = n),
    value = as.numeric(t(values))
  )
  
  names(result)[5] = name
  
  result
}

#' Extract raster values at points, keeping only arguments terra understands
#'
#' @param r a SpatRaster
#' @param y a SpatVector, or a data.frame or matrix with lon and lat in
#'  the first two columns
#' @param dots a list of further arguments, those used by chirps itself
#'  are dropped
#' @return a data.frame with one row per location (or per cell for polygons)
#'  and one column per layer, with the ID column removed and kept as
#'  attribute "ID"
#' @noRd
.extract_values = function(r, y, dots = list()) {
  own = c("version", "resolution", "type", "verbose", "use_vsicurl",
          "as.raster", "as.matrix", "as.sf", "as.geojson", "operation",
          "dist", "nQuadSegs", "server", "coverage", "interval", "format",
          "product")
  
  dots = dots[setdiff(names(dots), own)]
  
  if (!inherits(y, "SpatVector")) {
    y = as.matrix(as.data.frame(y)[, 1:2])
    storage.mode(y) = "numeric"
    # terra accepts only standard coordinate names for a matrix
    colnames(y) = c("x", "y")
  }
  
  result = do.call(terra::extract, c(list(x = r, y = y), dots))
  result = as.data.frame(result)
  
  ids = result[["ID"]]
  if (is.null(ids)) {
    ids = seq_len(nrow(result))
  }
  result[["ID"]] = NULL
  
  attr(result, "ID") = ids
  
  result
}

#' Turn long chirps data into a wide matrix, one row per id and one column
#' per day
#'
#' @param x a data.frame with columns id and date
#' @param value the name of the value column
#' @param ids integer, all ids expected in the rows
#' @param days Date vector, all days expected in the columns
#' @param prefix character, prefix for the column names
#' @return a matrix with NA where a day is missing for an id
#' @noRd
.to_wide = function(x, value, ids, days, prefix) {
  m = matrix(NA_real_, nrow = length(ids), ncol = length(days))
  i = match(x$id, ids)
  j = match(as.Date(x$date), days)
  keep = !is.na(i) & !is.na(j)
  m[cbind(i[keep], j[keep])] = as.numeric(x[[value]][keep])
  dimnames(m) = list(ids, paste0(prefix, gsub("-", ".", format(days))))
  m
}

#' Request CHIRPS from ClimateSERV and return the long chirps format
#'
#' @param gj a list of geojson strings, one per location
#' @param lonlat a data.frame with lon and lat, one row per location in the
#'  same order as \code{gj}
#' @param dates a character of start and end dates "YYYY-MM-DD"
#' @param operation integer, the ClimateSERV operation
#' @return a data.frame with id, lon, lat, date and chirps, ordered by id
#'  and date, where id is the position of the location in \code{gj}
#' @noRd
.chirps_climateserv = function(gj, lonlat, dates, operation = 5) {
  dates_inter =
    .reformat_dates(dates, availability = c("1981-01-01", "0"))
  
  if (is.null(operation)) {
    operation = 5
  }
  
  result = .GET(
    gjson = gj,
    dates = dates_inter,
    operation = operation,
    datatype = 0
  )
  
  id = as.integer(result$id)
  
  result = data.frame(
    id = id,
    lon = as.numeric(lonlat[id, 1]),
    lat = as.numeric(lonlat[id, 2]),
    date = as.Date(result$date),
    chirps = as.numeric(result$value)
  )
  
  result = result[order(result$id, result$date), ]
  
  rownames(result) = NULL
  
  result
}

#' Map a ClimateSERV operation code to a summary function
#'
#' @param operation integer, 0 max, 1 min, 2 median, 4 sum, 5 average
#' @return a function
#' @importFrom stats median
#' @noRd
.operation_fun = function(operation = 5) {
  if (is.null(operation)) {
    operation = 5
  }
  
  f = switch(as.character(operation),
             "0" = max,
             "1" = min,
             "2" = stats::median,
             "4" = sum,
             "5" = mean,
             NULL)
  
  if (is.null(f)) {
    stop("`operation` must be one of 0 (max), 1 (min), 2 (median), ",
         "4 (sum) or 5 (average).\n", call. = FALSE)
  }
  
  f
}

#' Summarise raster cells within each polygon
#'
#' @param r a SpatRaster, one layer per day
#' @param polygons an sf object with POLYGON or MULTIPOLYGON geometries
#' @param operation integer, see \code{.operation_fun}
#' @param dots a list of further arguments for \code{terra::extract}
#' @return a data.frame with one row per polygon and one column per layer.
#'  Cells that touch a polygon count, so polygons smaller than a cell still
#'  get a value
#' @importFrom terra vect
#' @importFrom sf st_transform st_sf st_geometry
#' @noRd
.polygon_values = function(r, polygons, operation = 5, dots = list()) {
  v = terra::vect(.to_lonlat(sf::st_geometry(polygons)))
  
  dots[["touches"]] = TRUE
  dots[["fun"]] = .operation_fun(operation)
  dots[["na.rm"]] = TRUE
  
  result = .extract_values(r, v, dots)
  
  result[] = lapply(result, function(x) ifelse(is.nan(x), NA_real_, x))
  
  result
}

#' Crop a raster to the extent of a set of points
#'
#' @param r a SpatRaster
#' @param lonlat a data.frame with lon and lat in the first two columns
#' @return a SpatRaster that covers all points, with one cell of margin
#' @importFrom terra ext res crop
#' @noRd
.crop_to_points = function(r, lonlat) {
  xy = as.matrix(as.data.frame(lonlat)[, 1:2])
  storage.mode(xy) = "numeric"
  e = terra::ext(min(xy[, 1]), max(xy[, 1]), min(xy[, 2]), max(xy[, 2]))
  e = e + max(terra::res(r))
  terra::crop(r, e, snap = "out")
}

#' All days between the start and end of the dates argument
#'
#' @param dates the dates argument, one or two dates
#' @return a Date vector
#' @noRd
.days = function(dates) {
  dates = .as_dates(dates)
  seq.Date(as.Date(dates[1]), as.Date(dates[2]), by = "day")
}

#' Convert a SpatVector to sf
#'
#' @param x a SpatVector of points or polygons
#' @return an sf object with the same geometries and CRS
#' @importFrom terra geom crs
#' @importFrom sf st_as_sfc st_crs NA_crs_
#' @noRd
.vect_to_sf = function(x) {
  wkt = terra::crs(x)
  crs = if (nzchar(wkt)) sf::st_crs(wkt) else sf::NA_crs_
  g = sf::st_as_sfc(terra::geom(x, wkt = TRUE), crs = crs)
  sf::st_sf(geometry = g)
}

#' Transform geometries to longitude and latitude when their CRS is known
#'
#' Geometries without a CRS are taken as longitude and latitude.
#'
#' @param g an sf or sfc object
#' @return g in EPSG:4326, or unchanged when it has no CRS or is already in
#'  longitude and latitude
#' @noRd
.to_lonlat = function(g) {
  crs = sf::st_crs(g)
  if (!is.na(crs) && !isTRUE(sf::st_is_longlat(g))) {
    g = sf::st_transform(g, 4326)
  }
  g
}
