#' Get raw CHIRTS-ERA5 raster data (deprecated)
#'
#' This function is deprecated. Use
#'  \code{get_chirts_raw(dates, var, product = "CHIRTS-ERA5")} instead.
#'  It will be removed in a future release.
#'
#' @param dates Character or Date vector "YYYY-MM-DD", with one date for a
#'   single day or two dates for the start and end of the period.
#' @param var Character. Variable to download. Supported values are
#'   \code{"Tmin"} and \code{"Tmax"}.
#' @param use_vsicurl Logical. If \code{TRUE}, prepends \code{"/vsicurl"} to
#'   remote URLs.
#' @param verbose Logical. If \code{TRUE}, prints the URLs used.
#' @param ... Additional arguments reserved for future use.
#'
#' @return A \code{\link[terra]{SpatRaster}} with one layer per day.
#'
#' @seealso \code{\link{get_chirts_raw}}
#'
#' @examples
#' if (interactive()) {
#'   # use instead
#'   tmin = get_chirts_raw(dates = c("1980-01-01", "1980-01-31"),
#'                         var = "Tmin", product = "CHIRTS-ERA5")
#' }
#'
#' @keywords internal
#' @export
get_chirts_era5_raw = function(dates,
                               var = c("Tmin", "Tmax"),
                               use_vsicurl = TRUE,
                               verbose = FALSE,
                               ...) {
  
  .notice_once("era5_deprecated",
               "get_chirts_era5_raw() is deprecated. Use get_chirts_raw(",
               "dates, var, product = \"CHIRTS-ERA5\") instead.")
  
  var = match.arg(var)
  
  get_chirts_raw(dates,
                 var = var,
                 product = "CHIRTS-ERA5",
                 use_vsicurl = use_vsicurl,
                 verbose = verbose,
                 ...)
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

