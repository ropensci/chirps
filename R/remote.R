#' User agent sent with every request to CHC and ClimateSERV
#'
#' CHC asked for a descriptive user agent with a contact address, so they can
#' tell package traffic apart from bots.
#'
#' @return a character string
#' @importFrom utils packageVersion
#' @importFrom httr RETRY status_code timeout user_agent
#' @importFrom terra getGDALconfig setGDALconfig
#' @noRd
.chirps_ua = function() {
  v = tryCatch(as.character(utils::packageVersion("chirps")),
               error = function(e) "dev")
  paste0("chirps R package/", v,
         " (https://github.com/AgrDataSci/chirps; desousa.kaue@gmail.com)")
}

#' Set GDAL options for reading the CHC files
#'
#' terra reads the remote files through GDAL, which reads them lazily, so the
#' options have to stay set for the session. Options the user has set
#' themselves are kept. Besides the user agent, GDAL is told not to list the
#' remote folder and not to look for side files (.aux.xml, .ovr) on each
#' open, which saves requests and avoids requests for files that do not
#' exist, and to retry a failed read.
#'
#' @return nothing, called for its side effect
#' @noRd
.set_gdal_options = function() {
  opts = c(
    GDAL_HTTP_USERAGENT = .chirps_ua(),
    GDAL_DISABLE_READDIR_ON_OPEN = "EMPTY_DIR",
    CPL_VSIL_CURL_ALLOWED_EXTENSIONS = ".tif,.tiff,.cog",
    GDAL_HTTP_MAX_RETRY = "3",
    GDAL_HTTP_RETRY_DELAY = "2"
  )
  
  for (o in names(opts)) {
    current = tryCatch(terra::getGDALconfig(o), error = function(e) "")
    if (length(current) == 0 || all(is.na(current) | current == "")) {
      tryCatch(terra::setGDALconfig(o, opts[[o]]), error = function(e) NULL)
    }
  }
  
  invisible(NULL)
}

#' Check that the first and last files of a request exist before reading them
#'
#' CHC blocks IP addresses that request many files that do not exist. Checking
#' two files instead of letting terra request every date stops a request with
#' a wrong path or date after at most two misses.
#'
#' @param urls character, the URLs of the files, one per date
#' @return nothing, stops with an informative error when a file is missing
#' @noRd
.check_remote = function(urls) {
  urls = sub("^/vsicurl/", "", urls)
  
  probe = unique(urls[c(1, length(urls))])
  
  for (u in probe) {
    # retry when the server is busy (429, 5xx), with growing pauses, but
    # stop at once on a missing file or a refusal
    status = tryCatch({
      r = httr::RETRY("HEAD", u,
                      httr::user_agent(.chirps_ua()),
                      httr::timeout(60),
                      times = 4,
                      pause_base = 2,
                      pause_cap = 30,
                      terminate_on = c(403, 404),
                      quiet = TRUE)
      httr::status_code(r)
    }, error = function(e) NA_integer_)
    
    # a network failure is left to terra, which gives its own error
    if (is.na(status) || status < 400) {
      next
    }
    
    if (status == 404) {
      stop("The file ", u, " was not found on the CHC server. ",
           "Check that the dates are within the period the product covers ",
           "and that the version and type exist.\n", call. = FALSE)
    }
    
    if (status == 403) {
      stop("The CHC server refused the request for ", u, " (HTTP 403). ",
           "The server blocks addresses that request many missing files. ",
           "Wait before trying again, or contact the Climate Hazards Center ",
           "<https://www.chc.ucsb.edu/>.\n", call. = FALSE)
    }
    
    if (status == 429) {
      stop("The CHC server is receiving too many requests from this address ",
           "(HTTP 429). Wait a few minutes before trying again.\n",
           call. = FALSE)
    }
    
    stop("The CHC server returned HTTP ", status, " for ", u, ".\n",
         call. = FALSE)
  }
  
  invisible(NULL)
}

#' Open remote CHC files as a SpatRaster
#'
#' @param urls character, the URLs of the files, one per date
#' @param dates Date vector, one per URL
#' @return a SpatRaster with one layer per date
#' @noRd
.read_remote = function(urls, dates) {
  .check_remote(urls)
  .set_gdal_options()
  
  if (!all(grepl("^/vsicurl/", urls))) {
    urls = file.path("/vsicurl", urls)
  }
  
  r = tryCatch(terra::rast(urls), error = function(e) {
    if (grepl("429", conditionMessage(e))) {
      stop("The CHC server is receiving too many requests from this address ",
           "(HTTP 429). Wait a few minutes before trying again.\n",
           call. = FALSE)
    }
    stop(e)
  })
  terra::time(r) = dates
  
  r
}
