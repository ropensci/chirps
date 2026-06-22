#' Get raw CHIRTS-ERA5 raster data
#'
#' Download CHIRTS-ERA5 daily temperature data in its original raster format.
#'
#' @param dates Character vector of length two with start and end dates in
#'   \code{"YYYY-MM-DD"} format.
#' @param var Character. Variable to download. Supported values are
#'   \code{"Tmin"} and \code{"Tmax"}.
#' @param use_vsicurl Logical. If \code{TRUE}, prepends \code{"/vsicurl"} to
#'   remote URLs.
#' @param verbose Logical. If \code{TRUE}, prints the URLs used.
#' @param ... Additional arguments reserved for future use.
#'
#' @return A \code{\link[terra]{SpatRaster}} with one layer per day.
#' 
#' @details
#' CHIRTS-ERA5 is a quasi-global (60°S to 70°N), high-resolution
#' (0.05° × 0.05°) dataset of daily maximum and minimum temperatures,
#' heat index, and wet bulb globe temperature from 1980 to near-present.
#'
#' The dataset combines the Climate Hazards Center Infrared Temperature
#' with Stations (CHIRTS) product (1983–2016) with the regularly updated
#' ERA5 reanalysis. ERA5 fields are downscaled and bias-corrected relative
#' to CHIRTS to provide a temporally consistent climate record.
#'
#' Users of this dataset should cite:
#'
#' "CHIRTS-ERA5 Data Repository. https://doi.org/10.15780/G2F08J.
#' Data accessed on [DATE]."
#'
#' Additional information is available at:
#' \url{https://www.chc.ucsb.edu/data/chirts-era5}
#' 
#' @references
#' Climate Hazards Center (CHC). CHIRTS-ERA5 Data Repository.
#' \doi{10.15780/G2F08J}
#' 
#' @examples
#' if (interactive()) {
#'   tmin = get_chirts_era5_raw(dates = c("1980-01-01", "1980-01-31"),
#'                              var = "Tmin")
#' 
#'   tmax = get_chirts_era5_raw(dates = c("1980-01-01", "1980-01-31"),
#'                              var = "Tmax")
#' }
#'
#' @importFrom terra rast time<-
#' @export
get_chirts_era5_raw = function(dates,
                               var = c("Tmin", "Tmax"),
                               use_vsicurl = FALSE,
                               verbose = FALSE,
                               ...) {
  
  .validate_dates(dates, availability = c("1980-01-01", "0"))
  
  var = match.arg(var)
  
  seqdate = seq.Date(as.Date(dates[1]), as.Date(dates[2]), by = "day")
  
  if (any(as.integer(format(seqdate, "%Y")) < 1980)) {
    stop("CHIRTS-ERA5 data are only available from 1980 onwards.",
         call. = FALSE)
  }
  
  urls = .chirts_era5_raw_urls(
    dates = seqdate,
    var = var
  )
  
  if (isTRUE(use_vsicurl)) {
    urls = file.path("/vsicurl", urls)
  }
  
  if (isTRUE(verbose)) {
    message(paste(urls, collapse = "\n"))
  }
  
  r = terra::rast(urls)
  terra::time(r) = seqdate
  
  r
}

#' @noRd
.chirts_era5_raw_urls = function(dates,
                                 var = c("Tmin", "Tmax")) {
  
  var = match.arg(var)
  
  years = format(dates, "%Y")
  date_string = gsub("-", ".", dates)
  
  var_dir = tolower(var)
  
  sprintf(
    "https://data.chc.ucsb.edu/experimental/CHIRTS-ERA5/%s/tifs/daily/%s/CHIRTS-ERA5.daily_%s.%s.tif",
    var_dir,
    years,
    var,
    date_string
  )
}

