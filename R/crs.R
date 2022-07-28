#' Determines the coordinate reference system
#'
#' @param crs An object which is a \var{crs} or from which one can be derived.
#'   \code{x} can
#'   be numeric as a \var{EPSG} number;
#'   a character string as a \var{wkt};
#'   a character string as a \var{proj4} (not recommended because outdated);
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
  sf::st_crs(crs)$units_gdal # nolint: extraction_operator_linter.
}





#' \var{UTM} zone based on geographic location(s)
#'
#' The function determines the \var{UTM} number and south/north location
#' for the mid-point of \code{x}.
#'
#' @inheritParams as_points
#'
#' @return A list with two elements: \describe{
#'   \item{utm_zone}{The UTM zone number as integer value.}
#'   \item{utm_NS}{North/South indicator as character "N" or "S"}.
#' }
#'
#' @references Convert Latitude/Longitude to UTM
# nolint start: line_length_linter.
#'   \url{https://www.wavemetrics.com/code-snippet/convert-latitudelongitude-utm}
# nolint end
#'   (attributed to Chuck Gantz).
#'
#' @examples
#' locations <- matrix(
#'   data = c(-120.325, -111.245, 39.855, 36.753),
#'   nrow = 2
#' )
#'
#' utm_zone(locations)
#' utm_zone(locations[1, ])
#' utm_zone(locations[2, ])
#'
#' @export
utm_zone <- function(x, crs = 4326) {
  x <- as_points(x, to_class = "sf", crs = crs)
  x <- as_points(x, to_class = "sf", crs = 4326)
  xy <- sf::st_coordinates(x)
  mxy <- colMeans(xy)

  # Make sure longitude is between -180.00 .. 179.9
  long <- mxy[[1L]] - floor((mxy[[1L]] + 180) / 360) * 360

  utm_zone <- floor((long + 180) / 6) + 1

  if (mxy[[2L]] >= 56 && mxy[[2L]] < 64 && long >= 3 && long < 12) {
    utm_zone <- 32
  }

  # Special zones for Svalbard
  if (mxy[[2L]] >= 72 && mxy[[2L]] < 84) {
    if (long >= 0 && long < 9) {
      utm_zone <- 31
    } else if (long >= 9 && long < 21) {
      utm_zone <- 33
    } else if (long >= 21 && long < 33) {
      utm_zone <- 35
    } else if (long >= 33 && long < 42) {
      utm_zone <- 37
    }
  }

  list(
    utm_zone = as.integer(unname(utm_zone)),
    utm_NS = if (mxy[[2L]] > 0) "N" else "S"
  )
}



#' \var{EPSG} code for the \var{UTM} zone based on geographic location(s)
#'
#' The function determines the \var{UTM} zone for the mid-point of \code{x}.
#'
#' @inheritParams as_points
#'
#' @return The \var{EPSG} code as integer value.
#'
#' @seealso \code{\link{utm_zone}}
#'
#' @references
#' The python package \var{utm-zone} by Per Liedman available at
#' \url{https://pypi.org/project/utm-zone}.
#'
#' @examples
#' locations <- matrix(
#'   data = c(-120.325, -111.245, 39.855, 36.753),
#'   nrow = 2
#' )
#'
#' epsg_for_utm(locations)
#' sf::st_crs(epsg_for_utm(locations))
#'
#' @export
epsg_for_utm <- function(x, crs = 4326) {
  tmp <- utm_zone(x, crs)

  if (tmp[["utm_NS"]] == "S") {
    32700 + tmp[["utm_zone"]]
  } else {
    32600 + tmp[["utm_zone"]]
  }
}
