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
  sf::st_crs(crs)$units_gdal
}


#' Calculate the UTM zone based on geographic location
#'
#' @references Convert Latitude/Longitude to UTM
#nolint start
#'   \url{https://www.wavemetrics.com/code-snippet/convert-latitudelongitude-utm}
#nolint end
#'   (attributed to Chuck Gantz).
#'
#' @export
get_UTM_Zone <- function(longitude, latitude) {
  Long <- mean(longitude)
  Lat <- mean(latitude)

  # Make sure longitude is between -180.00 .. 179.9
  LongTemp <- Long - floor((Long + 180) / 360) * 360

  ZoneNumber <- floor((LongTemp + 180) / 6) + 1

  if (Lat >= 56 && Lat < 64 && LongTemp >= 3 && LongTemp < 12) {
    ZoneNumber <- 32
  }

  # Special zones for Svalbard
  if (Lat >= 72 && Lat < 84) {
    if (LongTemp >= 0 && LongTemp < 9) {
      ZoneNumber <- 31
    } else if (LongTemp >= 9 && LongTemp < 21) {
      ZoneNumber <- 33
    } else if (LongTemp >= 21 && LongTemp < 33) {
      ZoneNumber <- 35
    } else if (LongTemp >= 33 && LongTemp < 42) {
      ZoneNumber <- 37
    }
  }

  ZoneNumber
}
