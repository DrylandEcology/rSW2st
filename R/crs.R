#' Determines the coordinate reference system
#'
#' @param crs An object which is a \var{crs} or from which one can be derived.
#'   \code{x} can
#'   be numeric as a \var{EPSG} number;
#'   a character string as a \var{wkt} or \var{proj4};
#'   or of a class including
#'     \code{\link[raster:Raster-class]{raster::Raster}},
#'     \code{\link[sp:Spatial-class]{sp::Spatial}},
#'     \code{\link[sp:CRS-class]{sp::CRS}},
#'     or a \code{\link[sf]{sf}} or \code{\link[sf]{sfc}} class.
#'
#' @seealso \code{\link[sf]{st_crs}}
#'
#' @name rSW2st_crs
NULL


#' @rdname rSW2st_crs
#'
#' @return The function \code{crs_units} returns the \var{crs} units
#'   as a character string or \code{NA}.
#'
#' @examples
#' crs_units(4326)
#' crs_units("EPSG:4326")
#' crs_units(sp::CRS("+init=EPSG:4326"))
#'
#' @export
crs_units <- function(crs) {
  sf::st_crs(crs)$units_gdal
}
