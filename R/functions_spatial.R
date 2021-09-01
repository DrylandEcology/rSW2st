
#' Automatic calculation of a variogram range
#'
#' @param x A \code{\link[raster:RasterLayer-class]{raster::RasterLayer}}
#'   or an object that can be passed to \code{\link{as_points}}.
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
#' variogram_range(xy)
#'
#' @export
variogram_range <- function(
  x,
  project_to_utm = TRUE,
  sub_samplepoints_N = NULL,
  crs,
  seed = NULL
) {

  stopifnot(
    requireNamespace("automap"),
    requireNamespace("gstat")
  )

  if (inherits(x, "RasterLayer")) {
    points <- raster::rasterToPoints(x, spatial = TRUE)
    names(points) <- "target"

  } else {
    tmp <- as_points(x, crs = crs, to_class = "sp")
    points <- sp::SpatialPointsDataFrame(
      coords = tmp,
      data = data.frame(target = rep(1, length(tmp)))
    )
  }

  if (!is.null(sub_samplepoints_N)) {
    set.seed(seed)
    tmp <- sample(
      seq_len(nrow(points)),
      sub_samplepoints_N,
      replace = FALSE
    )
    points <- points[tmp, ]
  }

  if (project_to_utm) {
    # --> variogram will calculate Euclidean distances in map units
    points_prj <- sp::spTransform(
      points,
      CRSobj = as(sf::st_crs(rSW2st::epsg_for_utm(points)), "CRS")
    )

  } else {
    # --> variogram will calculate great circle distances(!)
    points_prj <- sp::spTransform(
      points,
      CRSobj = as(sf::st_crs("OGC:CRS84"), "CRS")
    )
  }


  #--- determine variogram; see gstat::variogram
  fittedVar <- automap::autofitVariogram(
    target ~ 1,
    input_data = points_prj,
    miscFitOptions = list(merge.small.bins = TRUE)
  )

  #--- variogram range
  # estimate is only valid if some variation in data (i.e., sill > 0)
  psill_sum <- sum(fittedVar[["var_model"]][, "psill"])

  if (abs(psill_sum) > sqrt(.Machine$double.eps)) {
    fittedVar[["var_model"]][2, "range"]
  } else {
    NaN
  }
}
