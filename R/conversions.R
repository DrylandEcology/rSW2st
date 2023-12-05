
#' Convert two-dimensional locations to a spatially explicit object
#'
#' @param x A numerical two-dimensional object
#'   (a \code{matrix}, \code{array}, or \code{data.frame})
#'   with longitude/X, latitude/Y as columns;
#'   a \code{\link[sp:SpatialPoints-class]{sp::SpatialPoints}} object; or
#'   a \code{\link[terra:SpatVector-class]{terra::SpatVector}} object; or
#'   a \var{sf} object with a point geometry,
#'   i.e., an object with a class \var{sf} or \var{sfc}.
#' @param to_class A character string. Convert
#'   to \var{"sp"} for
#'   a \code{\link[sp:SpatialPoints-class]{sp::SpatialPoints}} object;
#'   to \var{"sv"} for
#'   a \code{\link[terra:SpatVector-class]{terra::SpatVector}} object;
#'   to \var{"sf"} for a \code{\link[sf:sf]{sf}} object; or
#'   convert to \var{"sfc"} for
#'   a \code{\link[sf:sfc_POINT]{sf::sfc_POINT}} object.
#' @inheritParams rSW2st_crs
#'
#' @section Details:
#'   Argument \code{crs} is only used if \code{x} is not a spatial object
#'   with an embedded \code{crs}; the argument is otherwise ignored and
#'   should be missing.
#'
#' @section Notes:
#'   This function does not carry out \var{crs} transformation.
#'
#' @return An object of the requested class.
#'
#' @examples
#' locations <- matrix(
#'   data = c(-120.325, -111.245, 39.855, 36.753),
#'   nrow = 2
#' )
#'
#' pts_sf1 <- as_points(locations, crs = 4326, to_class = "sf")
#' pts_sfc1 <- as_points(locations, crs = 4326, to_class = "sfc")
#' pts_sv1 <- as_points(locations, crs = 4326, to_class = "sv")
#'
#' pts_sf2 <- as_points(pts_sv1, to_class = "sf")
#' pts_sfc2 <- as_points(pts_sv1, to_class = "sfc")
#' pts_sv2 <- as_points(pts_sf1, to_class = "sv")
#'
#' all.equal(pts_sf1, pts_sf2, check.attributes = FALSE)
#' all.equal(pts_sfc1, pts_sfc2, check.attributes = FALSE)
#' all.equal(pts_sv1, pts_sv2)
#' all.equal(locations, sf::st_coordinates(pts_sf1), check.attributes = FALSE)
#' all.equal(locations, sf::st_coordinates(pts_sfc1), check.attributes = FALSE)
#' all.equal(locations, terra::crds(pts_sv1), check.attributes = FALSE)
#'
#' if (requireNamespace("sp")) {
#'   pts_sp1 <- as_points(locations, crs = 4326, to_class = "sp")
#'   pts_sp2 <- as_points(pts_sf1, to_class = "sp")
#'   all.equal(pts_sp1, pts_sp2)
#'   all.equal(locations, sp::coordinates(pts_sp1), check.attributes = FALSE)
#' }
#'
#' # A vector of length two is interpreted as a single point location
#' pts_sf11 <- as_points(locations[1, ], crs = 4326, to_class = "sf")
#' @export
as_points <- function(
  x,
  crs,
  to_class = c("sf", "sfc", "sp", "sv")
) {

  to_class <- match.arg(to_class)

  if (inherits(x, to_class)) {
    return(x)
  }

  is_sp <- inherits(x, "SpatialPoints")
  is_sf <- inherits(x, c("sf", "sfc", "sfg"))
  is_sv <- inherits(x, "SpatVector")

  if (is_sp || to_class == "sp") {
    stopifnot(requireNamespace("sp"))
  }

  # Convert
  res <- if (is_sf) {
    switch(
      EXPR = to_class,
      sp = {
        has_data <- length(setdiff(colnames(x), attr(x, "sf_column"))) > 0
        if (has_data) as(x, "Spatial") else as(sf::st_geometry(x), "Spatial")
      },
      sv = terra::vect(x),
      sf = x,
      sfc = sf::st_geometry(x)
    )

  } else if (is_sp) {
    switch(
      EXPR = to_class,
      sp = x,
      sv = terra::vect(x),
      sf = as(x, "sf"),
      sfc = as(x, "sfc")
    )

  } else if (is_sv) {
    switch(
      EXPR = to_class,
      # direct conversion sv -> sp would be possible
      # of package "raster" were first loaded with `library(raster)`
      sp = as(sf::st_as_sf(x), "Spatial"),
      sv = x,
      sf = sf::st_as_sf(x),
      sfc = sf::st_geometry(sf::st_as_sf(x))
    )

  } else if (!(is_sp || is_sf || is_sv)) {

    if (is.null(dim(x)) && length(x) == 2) {
      # Assume that this is supposed to be one point (and the object lost its
      # 2-dim structure inadvertently, e.g., locations[1, , drop = TRUE])
      x <- matrix(x, nrow = 1, ncol = 2)
    }

    if (missing(crs) || is.null(crs)) {
      stop("`crs` is missing and `x` is not a spatial object.")
    }

    crs_sf <- sf::st_crs(crs)

    switch(
      EXPR = to_class,

      sp = sp::SpatialPoints(
        coords = unname(x),
        proj4string = as(crs_sf, "CRS")
      ),

      sv = terra::vect(
        if (inherits(x, "matrix")) {
          x
        } else {
          data.matrix(x)
        },
        type = "points",
        crs = terra::crs(crs_sf$Wkt) # nolint: extraction_operator_linter.
      ),

      sf = ,
      sfc = sf::st_cast(
        x = sf::st_sfc(
          sf::st_multipoint(
            x = if (inherits(x, "matrix")) {
              x
            } else {
              data.matrix(x)
            }
          ),
          crs = crs_sf
        ),
        to = "POINT"
      )
    )
  }

  if (to_class == "sf" && !inherits(res, "sf")) sf::st_as_sf(res) else res
}
