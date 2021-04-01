
#' Calculate area extent of grid cells
#'
#' @inheritParams as_points
#' @param grid A \code{\link[raster:Raster-class]{raster::Raster}}
#'   object used for coordinate system and cells.
#' @param tol A numerical value to inform the site to gridcell matching.
#'
#' @seealso \code{\link[geosphere]{areaPolygon}}
#'
#' @return A \code{data.frame} with four columns: \var{\dQuote{Longitude}},
#'   \var{\dQuote{Latitude}}, \var{\dQuote{km2}}, and \var{\dQuote{rel}};
#'   and one row for each site of \code{sites}.
#'   Cell area on ellipsoid, based on \code{grid}, in square-kilometer
#'   \code{km2} and as fraction of maximal cell area on the equator \code{rel}.
#'
#' @examples
#' r <- raster::raster(
#'   xmn = 0, xmx = 1,
#'   ymn = -90, ymx = 90,
#'   crs ="EPSG:4326",
#'   resolution = c(1, 1)
#' )
#' n <- prod(dim(r))
#' r[] <- seq_len(n)
#' xy <- raster::sampleRegular(r, size = n, sp = TRUE)
#'
#' ## Calculate area for a subset of cells in grid
#' cell_areas <- calculate_cell_area(xy, grid = r)
#'
#' ## Calculate are for all cells in grid
#' cell_areas2 <- calculate_cell_area(grid = r)
#'
#'
#' ## Visualize cell area by latitude
#' with(cell_areas, graphics::plot(y, km2, type = "l"))
#'
#' ## Comparison with a spherical Earth
#' # A spherical Earth underestimates cell areas
#' # at mid latitudes compared to cell areas on a WGS84 ellipsoid as here
#' rel_spherical <- cos(cell_areas[, "y"] * pi / 180)
#' graphics::plot(
#'   abs(cell_areas[, "y"]),
#'   max(cell_areas[, "km2"]) * (cell_areas[, "rel"] - rel_spherical),
#'   pch = 46,
#'   xlab = "abs(Latitude)",
#'   ylab = "Cell area difference\n(ellipsoid - sphere; km2)"
#' )
#'
#' @export
calculate_cell_area <- function(
  x,
  grid,
  crs = sf::st_crs(x),
  tol = sqrt(.Machine$double.eps)
) {

  m2_to_km2 <- 1e-6

  if (!missing(x)) {
    x <- as_points(x, to_class = "sf", crs = crs)

    if (sf::st_crs(x) != sf::st_crs(grid)) {
      x <- sf::st_transform(x, crs = sf::st_crs(grid))
    }

    coords <- sf::st_coordinates(x)[, 1:2]
    colnames(coords) <- c("x", "y")

  } else {
    coords <- cbind(
      x = raster::xFromCol(grid, seq_len(raster::ncol(grid))),
      y = raster::yFromRow(grid, seq_len(raster::nrow(grid)))
    )
  }

  cells <- data.frame(coords, km2 = NA, rel = NA)


  if (raster::isLonLat(grid)) {
    # Use function `areaPolygon` which works on
    # angular coordinates (longitude/latitude) on an ellipsoid
    stopifnot(requireNamespace("geosphere"))

    unique_lats <- unique(cells[, "y"])

    # Create empty raster except for cells at specified latitudes
    rtmp <- etmp <- raster::init(grid, fun = function(x) rep(NA, x))
    xy <- cbind(rep(raster::xmin(rtmp), length(unique_lats)), unique_lats)
    rtmp[raster::cellFromXY(rtmp, xy)] <- 1

    # Convert cells into polygons
    ptmp <- raster::rasterToPolygons(rtmp, dissolve = FALSE)

    # Calculate area of polygon for each cell
    for (k in seq_along(ptmp)) {
      icols <- abs(cells[, "y"] - sp::coordinates(ptmp[k, ])[, 2]) < tol
      # Return value of `areaPolygon` is square-meter
      cells[icols, "km2"] <- m2_to_km2 * geosphere::areaPolygon(ptmp[k, ])
    }

    # Calculate area of maximal polygon for a cell on the equator
    rid <- raster::cellFromXY(etmp, matrix(c(0, 0), nrow = 1))
    if (is.na(rid)) {
      dxy <- - raster::res(etmp) - c(raster::xmin(etmp), raster::ymin(etmp))
      etmp <- raster::shift(etmp, dx = dxy[1], dy = dxy[2])
      rid <- raster::cellFromXY(etmp, matrix(c(0, 0), nrow = 1))
    }

    etmp[rid] <- 1
    etmp <- raster::rasterToPolygons(etmp, dissolve = FALSE)
    cell_maxarea_km2 <- m2_to_km2 * geosphere::areaPolygon(etmp)

  } else {
    # Use Euclidean area for projected/flat grids
    ar <- prod(raster::res(grid))

    # Determine distance units: meters or kilometers?
    ar_km2 <- ar * switch(
      crs_units(grid),
      meter =, meters =, metre =, metres =, m = m2_to_km2, # nolint
      kilometer =, kilometers =, kilometre =, kilometres = , km2 = 1, # nolint
      NA
    )
    cells[, "km2"] <- ar_km2

    cell_maxarea_km2 <- ar_km2
  }

  cells[, "rel"] <- cells[, "km2"] / cell_maxarea_km2

  cells
}


#' Calculate "nominal resolution" of grid
#'
#' @param grid A raster object
#' @param sites A raster object. A numeric array, matrix, or data.frame where
#' each row is a site and the columns contain values for long and lat.
#' @param cell_areas_km2 A string of numeric values. Equal in length to the
#' length of sites. Area each cell represents in km2.
#'
#' @references CMIP6 Global Attributes, DRS, Filenames, Directory Structure,
#'   and CV’s 10 September 2018 (v6.2.7)
#'   Appendix 2: Algorithms for Defining the "nominal_resolution" Attribute
#nolint start
#'   \url{https://docs.google.com/document/d/1h0r8RZr_f3-8egBMMh7aqLwy3snpD6_MrDz1q8n5XUk/edit#bookmark=id.ibeh7ad2gpdi}
#nolint end
#'
#' @examples
#' r <- raster::raster(
#'   xmn = -120, xmx = -90,
#'   ymn = 30, ymx = 50,
#'   crs ="+init=epsg:4326",
#'   resolution = c(0.5, 0.5)
#' )
#' r[] <- seq_len(prod(dim(r)))
#' xy <- raster::sampleRandom(r, size = 50, sp = TRUE)
#' grid_cell_area <- calculate_cell_area(sites = xy, grid = r)
#'
#' calculate_nominal_resolution(
#'   grid = r,
#'   sites = xy,
#'   cell_areas_km2 = grid_cell_area[, "km2"]
#' )
#'
#' @export
calculate_nominal_resolution <- function(grid, sites, cell_areas_km2) {
  stopifnot(requireNamespace("geosphere"))
  # For a land surface model calculated on its own grid,
  # include all land grid cells

  # For each grid cell, calculate the distance (in km) between each pair of
  # cell vertices and select the maximum distance ("dmax").
  # For latxlon grid cells, for example, dmax would be the diagonal distance.
  res <- raster::res(grid)

  if (raster::isLonLat(grid)) {
    xy <- sp::coordinates(sites)
    xy_lowerleft <- xy - res / 2
    xy_upperright <- xy + res / 2

    id_use <-
      xy_lowerleft[, 1] >= -180 & xy_lowerleft[, 1] <= 180 &
      xy_lowerleft[, 2] >= -90 & xy_lowerleft[, 2] <= 90 &
      xy_upperright[, 1] >= -180 & xy_upperright[, 1] <= 180 &
      xy_upperright[, 2] >= -90 & xy_upperright[, 2] <= 90

    dmax_km <- 1e-3 *
      geosphere::distGeo(xy_lowerleft[id_use, ], xy_upperright[id_use, ])

    # Calculate the mean over all cells of dmax, weighting each by the
    # grid-cell's area (A)
    mean_resolution_km <- stats::weighted.mean(dmax_km, cell_areas_km2[id_use])

  } else {

    mean_resolution_km <- dmax_km <- 1e-3 * sqrt(sum(res ^ 2))
  }



  # Nominal resolution
  ifelse(mean_resolution_km < 0.72, "0.5 km",
    ifelse(mean_resolution_km < 1.6, "1 km",
      ifelse(mean_resolution_km < 3.6, "2.5 km",
        ifelse(mean_resolution_km < 7.2, "5 km",
          ifelse(mean_resolution_km < 16, "10 km",
            ifelse(mean_resolution_km < 36, "25 km",
              ifelse(mean_resolution_km < 72, "50 km",
                ifelse(mean_resolution_km < 160, "100 km",
                  ifelse(mean_resolution_km < 360, "250 km",
                    ifelse(mean_resolution_km < 720, "500 km",
                      ifelse(mean_resolution_km < 1600, "1000 km",
                        ifelse(mean_resolution_km < 3600, "2500 km",
                          ifelse(mean_resolution_km < 7200, "5000 km",
                            "10000 km")))))))))))))
}
