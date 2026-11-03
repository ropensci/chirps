#' Get raw CHIRPS raster data
#'
#' Download Global CHIRPS precipitation data in its original raster format.
#'
#' This function returns CHIRPS raster layers as a \code{\link[terra]{SpatRaster}}
#' without extracting values, reshaping the data, or converting it to a data
#' frame. It is intended for users who need direct access to the original
#' raster product for custom spatial workflows.
#'
#' @param dates Character vector of length two with start and end dates in
#'   \code{"YYYY-MM-DD"} format.
#' @param version Character or numeric. CHIRPS version. Supported values are
#'   \code{"3.0"} (default) and \code{"2.0"}. CHC retires CHIRPS v2 after
#'   the December 2026 release.
#' @param resolution Numeric. Spatial resolution for CHIRPS v2. Supported
#'   values are \code{0.05} and \code{0.25}. Ignored for CHIRPS v3.
#' @param type Character. CHIRPS v3 product type. Supported values are
#'   \code{"rnl"} and \code{"sat"}. Ignored for CHIRPS v2.
#' @param verbose Logical. If \code{TRUE}, prints the URLs used.
#' @param ... Additional arguments reserved for future use.
#'
#' @return A \code{\link[terra]{SpatRaster}} with one layer per day.
#'
#' @details
#' CHIRPS v3 (Climate Hazards Center Infrared Precipitation with Stations)
#' is a high-resolution (0.05°), quasi-global rainfall dataset spanning
#' 60°N to 60°S from 1981 to near-present.
#'
#' CHIRPS v3 combines satellite-based thermal infrared rainfall estimates
#' with in-situ station observations to produce gridded precipitation
#' estimates over land.
#'
#' Two daily products are available:
#' \describe{
#'   \item{rnl}{Daily values derived using ECMWF ERA5 precipitation to
#'   partition pentadal CHIRPS totals into daily amounts.}
#'   \item{sat}{Daily values derived using NASA IMERG precipitation to
#'   partition pentadal CHIRPS totals into daily amounts.}
#' }
#'
#' Additional information is available at:
#' \url{https://www.chc.ucsb.edu/data/chirps3}
#'
#' \strong{Messages}: each request states what it downloads, here daily
#'  rainfall from CHIRPS, with the period, so it is not mistaken for
#'  temperature from \code{\link{get_chirts}}. Set
#'  \code{options(chirps.quiet = TRUE)} to hide these messages.
#'
#' Daily product documentation:
#' \url{https://data.chc.ucsb.edu/products/CHIRPS/v3.0/daily/readme.txt}
#' 
#' @references
#'
#' Climate Hazards Center Infrared Precipitation with Stations version 3
#' (CHIRPS3) Data Repository.
#' \doi{10.15780/G2JQ0P}
#'
#' Version 3
#' Funk, C., Peterson, P., Harrison, L. et al. (2026).
#' The Climate Hazards Center Infrared Precipitation with Stations,
#' Version 3. Scientific Data, 13, 718.
#' \doi{10.1038/s41597-026-07096-4}
#' 
#' Version 2
#' Funk C. et al. (2015). Scientific Data, 2, 150066.
#' \doi{10.1038/sdata.2015.66}
#'
#' @examples
#' if (interactive()) {
#' r1 = get_chirps_raw(dates = c("2017-12-15", "2017-12-31"),
#'                     version = "3.0",
#'                     type = "rnl")
#' 
#' r2 = get_chirps_raw(dates = c("2017-12-15", "2017-12-31"),
#'                     version = "3.0",
#'                     type = "sat")
#' }
#'
#' @importFrom terra rast time<-
#' @export
get_chirps_raw = function(dates,
                          version = "3.0",
                          resolution = 0.05,
                          type = c("rnl", "sat"),
                          verbose = FALSE,
                          ...) {
  
  dates = .validate_dates(dates)
  
  version = as.character(version)
  if (version %in% c("2", "3")) {
    version = paste0(version, ".0")
  }
  
  if (!version %in% c("2.0", "3.0")) {
    stop("`version` must be either '2.0' or '3.0'.", call. = FALSE)
  }
  
  if (version == "2.0") {
    .notice_v2()
  }
  
  seqdate = seq.Date(as.Date(dates[1]), as.Date(dates[2]), by = "day")
  
  label = if (version == "3.0") {
    paste0("CHIRPS v3.0 (", match.arg(type, c("rnl", "sat")), ")")
  } else {
    paste0("CHIRPS v2.0 (", resolution, "\u00b0)")
  }
  .inform_request(label, "daily rainfall (mm)", seqdate)
  
  urls = .chirps_raw_urls(
    dates = seqdate,
    version = version,
    resolution = resolution,
    type = type
  )
  
  if (isTRUE(verbose)) {
    message(paste(urls, collapse = "\n"))
  }
  
  .read_remote(urls, seqdate)
}

#' @noRd
.chirps_raw_urls = function(dates,
                            version = "3.0",
                            resolution = 0.05,
                            type = c("rnl", "sat")) {
  
  years = format(dates, "%Y")
  date_string = gsub("-", ".", dates)
  
  if (version == "2.0") {
    
    if (!resolution %in% c(0.05, 0.25)) {
      stop("For CHIRPS v2, `resolution` must be 0.05 or 0.25.", call. = FALSE)
    }
    
    resolution = gsub("p0.", "p", paste0("p", resolution))
    
    fnames = file.path(
      years,
      paste0("chirps-v2.0.", date_string, ".cog")
    )
    
    urls = file.path(
      "https://data.chc.ucsb.edu/products/CHIRPS/v2.0",
      "global_daily",
      "cogs",
      resolution,
      fnames
    )
    
    return(urls)
  }
  
  if (version == "3.0") {
    
    type = match.arg(type)
    
    urls = sprintf(
      "https://data.chc.ucsb.edu/products/CHIRPS/v3.0/daily/final/%s/%s/chirps-v3.0.%s.%s.tif",
      type,
      years,
      type,
      date_string
    )
    
    return(urls)
  }
}

# keeps notices that are shown once per session
.chirps_env = new.env(parent = emptyenv())

#' Tell the user once per session that CHIRPS v2 is retired
#' @noRd
.notice_v2 = function() {
  .notice_once("v2_noticed",
               "CHIRPS v2 is retired after the December 2026 release. ",
               "Use version = \"3.0\" for current data.")
}

#' Tell the user once per session that ClimateSERV is not the main source
#' @noRd
.notice_climateserv = function() {
  .notice_once("climateserv_noticed",
               "ClimateSERV serves CHIRPS v2 only, which CHC retires after ",
               "the December 2026 release. Use server = \"CHC\" for ",
               "CHIRPS v3. ClimateSERV support for CHIRPS may be removed ",
               "in a future release.")
}

#' Tell the user once per session that a ClimateSERV function will be
#' deprecated
#' @param fun character, the function name
#' @noRd
.notice_climateserv_fun = function(fun) {
  .notice_once(paste0(fun, "_noticed"),
               fun, "() reads from the ClimateSERV API and will be ",
               "deprecated in a future release of chirps.")
}
