
#' Calculate area extent of grid cells
#'
#' @inheritParams as_points
#' @param grid
#'   A \code{\link[raster:Raster-class]{raster::Raster}} or
#'   a \code{stars::stars} object for coordinate system and cells.
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
#'   crs = "OGC:CRS84",
#'   resolution = c(1, 1)
#' )
#' n <- prod(dim(r))
#' r[] <- seq_len(n)
#' xy <- raster::sampleRegular(r, size = n, sp = TRUE)
#'
#' ## Calculate area for a subset of cells in grid
#' cell_areas <- calculate_cell_area(xy, grid = r)
#' cell_areas <- calculate_cell_area(xy, grid = stars::st_as_stars(r))
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

  stopifnot(inherits(grid, c("Raster", "stars")))

  if (!missing(x)) {
    x <- as_points(x, to_class = "sf", crs = crs)

    if (sf::st_crs(x) != sf::st_crs(grid)) {
      x <- sf::st_transform(x, crs = sf::st_crs(grid))
    }

    coords <- sf::st_coordinates(x)[, 1:2]
    colnames(coords) <- c("x", "y")

  } else {
    coords <- if (inherits(grid, "Raster")) {
      raster::coordinates(grid)

    } else if (inherits(grid, "stars")) {
      sf::st_coordinates(grid, center = TRUE)[, 1:2, drop = FALSE]
    }
  }

  cells <- data.frame(coords, km2 = NA, rel = NA)


  if (sf::st_is_longlat(grid)) {
    # Use function `areaPolygon` which works on
    # angular coordinates (longitude/latitude) on an ellipsoid
    stopifnot(requireNamespace("geosphere"))

    unique_lats <- unique(cells[, "y"])

    # Create empty 2d-raster except for cells at specified latitudes
    rgrid <- if (inherits(grid, "Raster")) {
      grid

    } else if (inherits(grid, "stars")) {
      as(
        if (length(dim(grid)) > 2) {
          grid[1, , , 1, drop = TRUE]
        } else {
          grid
        },
        Class = "Raster"
      )
    }

    rtmp <- etmp <- raster::init(rgrid, fun = function(x) rep(NA, x))
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
    if (inherits(grid, "Raster")) {
      tmp_res <- raster::res(grid)

    } else if (inherits(grid, "stars")) {
      tmp_res <- abs(unname(
        sapply(stars::st_dimensions(grid), function(x) x[["delta"]])
      ))[1:2]

      if (anyNA(tmp_res)) {
        stop("Can currently only handle regular, rectangular grids.")
      }
    }

    ar <- prod(tmp_res)

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


#' Calculate "nominal resolution" of a grid
#'
#' @param grid
#'   A \code{\link[raster:Raster-class]{raster::Raster}} or
#'   a \code{stars::stars} object.
#'   Gridcells with values that are not equal to a
#'   \code{maskvalue} are included in the "nominal resolution" calculation.
#' @param maskvalue A vector. Values to mask out from \code{grid}.
#'
#' @return A character string with a "nominal resolution" value in kilometers.
#'
#' @references
#'   \var{CMIP6 Global Attributes, DRS, Filenames, Directory Structure,
#'   and CV’s}, 10 September 2018 (v6.2.7).
#nolint start
#'   \href{https://docs.google.com/document/d/1h0r8RZr_f3-8egBMMh7aqLwy3snpD6_MrDz1q8n5XUk/edit#bookmark=id.ibeh7ad2gpdi}{Appendix 2: Algorithms for Defining the "nominal_resolution" Attribute}
#nolint end
#'
#' @examples
#' r1 <- raster::raster(
#'   xmn = -120, xmx = -90,
#'   ymn = 30, ymx = 50,
#'   crs = "OGC:CRS84",
#'   resolution = c(0.5, 0.5)
#' )
#' ext <- raster::cellsFromExtent(r1, raster::extent(-110, -100, 35, 45))
#' r1[ext] <- 1
#'
#' calculate_nominal_resolution(r1)
#' calculate_nominal_resolution(stars::st_as_stars(r1))
#'
#' r2 <- raster::raster(
#'   xmn = -2480000, xmx = 90000,
#'   ymn = 650000, ymx = 4020000,
#'   crs = "EPSG:6350",
#'   resolution = c(1e4, 1e4)
#' )
#' ext <- raster::cellsFromExtent(
#'   r2,
#'   raster::extent(-2080000, 0, 1000000, 3500000)
#' )
#' r2[ext] <- 1
#'
#' calculate_nominal_resolution(r2)
#'
#' @export
calculate_nominal_resolution <- function(grid, maskvalue = NA) {
  # For a land surface model calculated on its own grid,
  # include all land grid cells

  # For each grid cell, calculate the distance (in km) between each pair of
  # cell vertices and select the maximum distance ("dmax").
  # For latxlon grid cells, for example, dmax would be the diagonal distance.

  if (inherits(grid, "Raster")) {
    res <- raster::res(grid)

  } else if (inherits(grid, "stars")) {
    res <- abs(unname(
      sapply(stars::st_dimensions(grid), function(x) x[["delta"]])
    ))[1:2]

    if (anyNA(res)) {
      stop("Can currently only handle regular, rectangular grids.")
    }

  } else {
    stop("`grid` of class ", shQuote(class(grid)), " is not implemented.")
  }


  if (sf::st_is_longlat(grid)) {
    stopifnot(requireNamespace("geosphere"))

    if (inherits(grid, "Raster")) {
      has_values <- !(raster::getValues(grid) %in% maskvalue)
      xy <- raster::coordinates(grid)[has_values, , drop = FALSE]

    } else if (inherits(grid, "stars")) {
      tmp <- as.data.frame(grid, center = TRUE)
      xy <- tmp[complete.cases(tmp), 1:2, drop = FALSE]
    }

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
    cell_areas_km2 <- calculate_cell_area(
      xy[id_use, , drop = FALSE],
      grid = grid,
      crs = sf::st_crs(grid)
    )[, "km2"]
    mean_resolution_km <- stats::weighted.mean(dmax_km, cell_areas_km2)

  } else {
    tmp <- sqrt(sum(res ^ 2))
    cu <- switch(
      EXPR = crs_units(grid),
      meter = , meters = , metre = , metres = , m = 1e-3, # nolint
      kilometer = , kilometers = , kilometer = , kilometres = , km = 1, # nolint
      stop("Unknown unit")
    )
    mean_resolution_km <- cu * tmp
  }


  # Nominal resolution
  nr <- data.frame(
    cuts =
      c(0, 0.72, 1.6, 3.6, 7.2, 16, 36, 72, 160, 360, 720, 1600, 3600, 7200),
    label = paste0(
      c(0.5, 1, 2.5, 5, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000),
      " km"
    )
  )

  nr[findInterval(mean_resolution_km, nr[, "cuts"], left.open = TRUE), "label"]
}
