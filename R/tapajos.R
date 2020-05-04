#' Tapajos National Forest
#'
#' Geometries for the Tapajos National Forest, a protected 
#' area in the Brazilian 
#' Amazon \url{http://www.icmbio.gov.br/flonatapajos/}
#' 
#' @format An object of class 'sfc_POLYGON' within the bounding box 
#' xmin: -55.41127 ymin: -4.114584 
#' xmax: -54.7973 ymax: -2.751706
#' @examples 
#' \donttest{
#' library("sf")
#' 
#' data("tapajos", package = "chirps")
#' 
#' # sample three points within the Tapajos area
#' set.seed(1234)
#' tp_point <- st_sample(tapajos, 3)
#' 
#' # coerce as sf points
#' tp_point <- st_as_sf(tp_point)
#' }
#' @source The data was provided by the Chico Mendes Institute via
#' \url{https://www.protectedplanet.net/}.
"tapajos"
