#' Get raw CHIRTS raster data
#'
#' Download CHIRTS daily temperature data in its original raster format, from
#'  CHIRTS-daily (default) or CHIRTS-ERA5.
#'
#' This function returns CHIRTS raster layers as a
#' \code{\link[terra]{SpatRaster}} without extracting values, reshaping the data,
#' or converting it to a data frame.
#'
#' @param dates Character or Date vector "YYYY-MM-DD", with one date for a
#'   single day or two dates for the start and end of the period.
#' @param var Character. CHIRTS variable. Supported values are
#'   \code{"Tmax"}, \code{"Tmin"}, \code{"RHum"}, and \code{"HeatIndex"}.
#'   CHIRTS-ERA5 has \code{"Tmax"} and \code{"Tmin"} only.
#' @param product Character. \code{"CHIRTS-daily"} (default, 1983 to 2016)
#'   or \code{"CHIRTS-ERA5"} (1980 to near present).
#' @param resolution Numeric. Spatial resolution. Default is \code{0.05}.
#'   CHIRTS-daily only.
#' @param coverage Character. Spatial coverage. Default is \code{"global"}.
#'   CHIRTS-daily only.
#' @param interval Character. Temporal interval. Default is \code{"daily"}.
#'   Currently retained for API consistency.
#' @param format Character. File format folder. Default is \code{"tifs"}.
#'   CHIRTS-daily only.
#' @param version Character or numeric. CHIRTS-daily version. Default is
#'   \code{"1.0"}.
#' @param use_vsicurl Logical. If \code{TRUE}, prepends \code{"/vsicurl"} to
#'   remote URLs.
#' @param verbose Logical. If \code{TRUE}, prints the URLs used.
#' @param ... Additional arguments reserved for future use.
#'
#' @return A \code{\link[terra]{SpatRaster}} with one layer per day.
#'
#' @details
#' \strong{CHIRTS-daily} (Climate Hazards Center Infrared Temperature with
#' Stations) is a quasi-global (60°S to 70°N), high-resolution
#' (0.05° × 0.05°) dataset of daily maximum and minimum temperatures,
#' relative humidity and heat index covering 1983–2016. It combines monthly
#' CHIRTS temperature fields with daily temperature variability from the
#' ERA5 reanalysis. Daily minimum temperatures are derived using the ERA5
#' diurnal temperature range. See
#' \url{https://www.chc.ucsb.edu/data/chirtsdaily}.
#'
#' \strong{CHIRTS-ERA5} is a quasi-global (60°S to 70°N), high-resolution
#' (0.05° × 0.05°) dataset of daily maximum and minimum temperatures from
#' 1980 to near present. It combines CHIRTS (1983–2016) with the regularly
#' updated ERA5 reanalysis, downscaled and bias-corrected relative to CHIRTS.
#' It is an experimental CHC product. Users should cite "CHIRTS-ERA5 Data
#' Repository. https://doi.org/10.15780/G2F08J. Data accessed on [DATE]."
#' See \url{https://www.chc.ucsb.edu/data/chirts-era5}.
#'
#' \strong{Messages}: each request states what it downloads, here daily
#'  temperature or humidity from CHIRTS, with the period, so it is not
#'  mistaken for rainfall from \code{\link{get_chirps}}. Set
#'  \code{options(chirps.quiet = TRUE)} to hide these messages.
#'
#' @references
#' Verdin, A., Funk, C., Peterson, P., Landsfeld, M.,
#' Tuholske, C., and Grace, K. (2020).
#' Development and validation of the CHIRTS-daily quasi-global
#' high-resolution daily temperature data set.
#' Scientific Data, 7, 303.
#' \doi{10.1038/s41597-020-00643-7}
#'
#' Funk, C., Peterson, P., Peterson, S., Shukla, S.,
#' Davenport, F., Michaelsen, J., et al. (2019).
#' A High-Resolution 1983–2016 Tmax Climate Data Record Based on
#' Infrared Temperatures and Stations by the Climate Hazards Center.
#' Journal of Climate, 32, 5639–5658.
#' \doi{10.1175/JCLI-D-18-0698.1}
#'
#' Climate Hazards Center (CHC). CHIRTS-ERA5 Data Repository.
#' \doi{10.15780/G2F08J}
#'
#' @examples
#' if (interactive()) {
#'   tmax = get_chirts_raw(dates = c("2015-01-01", "2015-01-31"),
#'                         var = "Tmax")
#'
#'   # recent dates from CHIRTS-ERA5
#'   tmin = get_chirts_raw(dates = c("2024-01-01", "2024-01-31"),
#'                         var = "Tmin",
#'                         product = "CHIRTS-ERA5")
#' }
#'
#' @importFrom terra rast time<-
#' @export
get_chirts_raw = function(dates,
                          var,
                          product = c("CHIRTS-daily", "CHIRTS-ERA5"),
                          resolution = 0.05,
                          coverage = "global",
                          interval = "daily",
                          format = "tifs",
                          version = "1.0",
                          use_vsicurl = TRUE,
                          verbose = FALSE,
                          ...) {
  
  product = match.arg(product)
  
  if (product == "CHIRTS-ERA5") {
    
    dates = .validate_dates(dates, availability = c("1980-01-01", "0"))
    
    if (!var %in% c("Tmax", "Tmin")) {
      stop("CHIRTS-ERA5 has var = \"Tmax\" and \"Tmin\" only.",
           call. = FALSE)
    }
    
    seqdate = .days(dates)
    
    .inform_request("CHIRTS-ERA5", .chirts_label(var), seqdate)
    
    urls = .chirts_era5_raw_urls(dates = seqdate, var = var)
    
  } else {
    
    dates = .validate_dates(dates)
    
    var = match.arg(var, c("HeatIndex", "RHum", "Tmax", "Tmin"))
    
    version = as.character(version)
    if (version == "1") {
      version = "1.0"
    }
    
    if (!version %in% "1.0") {
      stop("`version` must be '1.0'.", call. = FALSE)
    }
    
    seqdate = .days(dates)
    
    urls = .chirts_raw_urls(
      dates = seqdate,
      var = var,
      resolution = resolution,
      coverage = coverage,
      interval = interval,
      format = format,
      version = version
    )
    
    .inform_request("CHIRTS-daily", .chirts_label(var), seqdate)
  }
  
  if (isTRUE(verbose)) {
    message(paste(urls, collapse = "\n"))
  }
  
  if (isTRUE(use_vsicurl)) {
    .read_remote(urls, seqdate)
  } else {
    .check_remote(urls)
    .set_gdal_options()
    r = terra::rast(urls)
    terra::time(r) = seqdate
    r
  }
}

#' @noRd
.chirts_raw_urls = function(dates,
                            var,
                            resolution = 0.05,
                            coverage = "global",
                            interval = "daily",
                            format = "tifs",
                            version = "1.0") {
  
  years = format(dates, "%Y")
  year_int = as.integer(years)
  
  if (any(!year_int %in% 1983:2016)) {
    stop("CHIRTS-daily data are only available from 1983 to 2016. ",
         "For other years use product = \"CHIRTS-ERA5\", which covers 1980 ",
         "to near present.",
         call. = FALSE)
  }
  
  if (!resolution %in% c(0.05)) {
    stop("For CHIRTS v1.0, only `resolution = 0.05` is currently supported.",
         call. = FALSE)
  }
  
  if (!identical(coverage, "global")) {
    stop("For CHIRTS v1.0, only `coverage = 'global'` is currently supported.",
         call. = FALSE)
  }
  
  if (!identical(format, "tifs")) {
    stop("For CHIRTS v1.0, only `format = 'tifs'` is currently supported.",
         call. = FALSE)
  }
  
  file_var = var
  if (identical(var, "RHum")) {
    file_var = "RH"
  }
  
  date_string = gsub("-", ".", dates)
  
  fnames = file.path(
    years,
    paste0(file_var, ".", date_string, ".tif")
  )
  
  resolution = gsub("0\\.", "p", as.character(resolution))
  
  file.path(
    "https://data.chc.ucsb.edu/products/CHIRTS/daily/v1.0",
    paste0(coverage, "_", format, "_", resolution),
    var,
    fnames
  )
}


