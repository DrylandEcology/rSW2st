
#' Automatic calculation of a variogram range
#'
#' @param x A two-dimensional
#'   \code{\link[raster:RasterLayer-class]{raster::RasterLayer}},
#'   \code{\link[terra:SpatRaster-class]{terra::SpatRaster}},
#'   \code{stars} object, or
#'   an object that can be passed to \code{\link{as_points}}.
#' @param project_to_utm A logical value. If \code{TRUE}, then \code{x}
#'   will be projected to a suitable \var{UTM}; otherwise, \code{x} will
#'   be projected to \var{WGS84}.
#' @param sub_samplepoints_N An integer value. If not \code{NULL},
#'   then a random sample of \code{x} will be taken of the specified size.
#' @inheritParams as_points
#' @param seed Passed to \code{\link{set.seed}}; used only if
#'   \code{sub_samplepoints_N} is a number.
#'
#' @seealso \code{\link[automap]{autofitVariogram}}
#'
#' @return A numeric value representing the variogram range
#'   in units of \code{x}.
#'
#' @examples
#' xy <- as_points(0.5 + cbind(0:9, 0:9), crs = 6350, to_class = "sf")
#' try(variogram_range(xy)) ## requires packages "automap" and "gstat"
#'
#' @export
variogram_range <- function(
  x,
  project_to_utm = TRUE,
  sub_samplepoints_N = NULL,
  crs = NULL,
  seed = NULL
) {

  stopifnot(
    requireNamespace("automap"), # nolint: missing_package_linter
    requireNamespace("gstat")
  )

  if (inherits(x, c("RasterLayer", "SpatRaster"))) {
    x <- stars::st_as_stars(x)
  }

  pts <- if (inherits(x, "stars")) {
    sf::st_as_sf(x, as_points = TRUE, merge = FALSE)

  } else {
    as_points(x, crs = crs, to_class = "sf")
  }

  # question:
  # should all points have equal or unique value, e.g., seq_len(nrow(pts))
  pts[, "target"] <- 1


  if (!is.null(sub_samplepoints_N)) {
    set.seed(seed)
    tmp <- sample.int(
      n = nrow(pts),
      size = sub_samplepoints_N,
      replace = FALSE
    )
    pts <- pts[tmp, ]
  }

  pts_prj <- sf::st_transform(
    pts,
    crs = if (project_to_utm) {
      # --> variogram will calculate Euclidean distances in map units
      rSW2st::epsg_for_utm(pts)
    } else {
      # --> variogram will calculate great circle distances(!)
      "OGC:CRS84"
    }
  )


  #--- determine variogram; see gstat::variogram
  # nolint start: missing_package_linter, namespace_linter.
  fittedVar <- automap::autofitVariogram(
    target ~ 1,
    input_data = pts_prj,
    miscFitOptions = list(merge.small.bins = TRUE)
  )
  # nolint end

  #--- variogram range
  # estimate is only valid if some variation in data (i.e., sill > 0)
  psill_sum <- sum(fittedVar[["var_model"]][, "psill"])

  if (abs(psill_sum) > sqrt(.Machine[["double.eps"]])) {
    fittedVar[["var_model"]][2, "range"]
  } else {
    NaN
  }
}
