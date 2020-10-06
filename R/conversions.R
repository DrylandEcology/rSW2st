
#' Convert two-dimensional locations to a spatially explicit object
#'
#' @param locations A numerical two-dimensional object
#'   (a \code{matrix}, \code{array}, or \code{data.frame})
#'   with longitude/X, latitude/Y as columns;
#'   a \code{\link[sp:SpatialPoints-class]{sp::SpatialPoints}} object; or
#'   a \var{sf} object with a point geometry,
#'   i.e., an object with a class \var{sf} or \var{sfc}.
#' @param to_class A character string. Convert either
#'   to \var{"sp"} for
#'   a \code{\link[sp:SpatialPoints-class]{sp::SpatialPoints}} object;
#'   convert to \var{"sf"} for a \code{\link[sf:sf]{sf}} object; or
#'   convert to \var{"sfc"} for
#'   a \code{\link[sf:sfc_POINT]{sf::sfc_POINT}} object.
#' @inheritParams rSW2st_crs
#'
#' @section Details:
#'   Argument \code{crs} is only used if \code{locations} is not already a
#'   spatial object with a \code{crs}.
#'
#' @return An object of the requested class. If the input
#'   object \code{locations} is already of the requested class, then it is
#'   returned unchanged; otherwise, non-geometry/non-spatial data may be
#'   discarded.
#'
#' @examples
#'  locations <- matrix(
#'    data = c(-120.325, -111.245, 39.855, 36.753),
#'    nrow = 2
#'  )
#'
#'  pts_sf1 <- convert_points(locations, to_class = "sf")
#'  pts_sfc1 <- convert_points(locations, to_class = "sfc")
#'  pts_sp1 <- convert_points(locations, to_class = "sp")
#'
#'  pts_sf2 <- convert_points(pts_sp1, to_class = "sf")
#'  pts_sfc2 <- convert_points(pts_sp1, to_class = "sfc")
#'  pts_sp2 <- convert_points(pts_sf1, to_class = "sp")
#'
#'  all.equal(pts_sf1, pts_sf2)
#'  all.equal(pts_sfc1, pts_sfc2)
#'  all.equal(pts_sp1, pts_sp2)
#'  all.equal(locations, sf::st_coordinates(pts_sf1), check.attributes = FALSE)
#'  all.equal(locations, sf::st_coordinates(pts_sfc1), check.attributes = FALSE)
#'  all.equal(locations, sp::coordinates(pts_sp1), check.attributes = FALSE)
#'
#' @export
convert_points <- function(
  locations,
  to_class = c("sf", "sfc", "sp"),
  crs = 4326
) {

  to_class <- match.arg(to_class)

  if (inherits(locations, to_class)) {
    return(locations)
  }

  is_sp <- inherits(locations, "SpatialPoints")
  is_sf <- inherits(locations, c("sf", "sfc", "sfg"))

  # Convert
  res <- if (is_sf) {
    tmp <- sf::st_geometry(locations)
    switch(
      EXPR = to_class,
      sp = as(tmp, "Spatial"),
      sf = , #nolint
      sfc = tmp
    )

  } else if (is_sp) {
    tmp <- as(locations, "SpatialPoints")
    switch(
      EXPR = to_class,
      sp = tmp,
      sf = , #nolint
      sfc = as(tmp, "sfc")
    )

  } else if (!(is_sp || is_sf)) {
    crs <- sf::st_crs(crs)
    switch(
      EXPR = to_class,
      sp = sp::SpatialPoints(
        coords = locations,
        proj4string = sp::CRS(crs$proj4string)
      ),
      sf = , #nolint
      sfc = sf::st_cast(
        x = sf::st_sfc(
          sf::st_multipoint(
            x = if (inherits(locations, "matrix")) {
              locations
            } else {
              data.matrix(locations)
            }
          ),
          crs = crs
        ),
        to = "POINT"
      )
    )
  }

  if (to_class == "sf") sf::st_as_sf(res) else res
}
