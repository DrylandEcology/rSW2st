
#' Calculate area extent of grid cells
#'
#' @inheritParams as_points
#' @param grid
#'   A \code{\link[terra:SpatRaster-class]{terra::rast}},
#'   a \code{\link[raster:Raster-class]{raster::Raster}} or
#'   a \code{stars::stars} object for coordinate system and cells.
#' @param ... Currently, unused and silently ignored.
#'
#'
#' @return A \code{data.frame} with four columns: \var{\dQuote{Longitude}},
#'   \var{\dQuote{Latitude}}, \var{\dQuote{km2}}, and \var{\dQuote{rel}};
#'   and one row for each site of \code{sites}.
#'   Cell area on ellipsoid, based on \code{grid}, in square-kilometer
#'   \code{km2} and as fraction of maximal cell area on the equator \code{rel}.
#'
#' @examples
#' r <- terra::rast(
#'   xmin = 0, xmax = 1,
#'   ymin = -90, ymax = 90,
#'   crs = "OGC:CRS84",
#'   resolution = c(1, 1),
#'   vals = 1:180
#' )
#' xy <- terra::spatSample(r, size = 20L, method = "random", as.points = TRUE)
#'
#' ## Calculate area for a subset of cells in grid
#' cell_areas <- calculate_cell_area(xy, grid = r)
#' cell_areas <- calculate_cell_area(xy, grid = stars::st_as_stars(r))
#'
#' ## Calculate area for all cells in grid
#' cell_areas2 <- calculate_cell_area(grid = r)
#'
#'
#' ## Visualize cell area by latitude
#' with(cell_areas2, graphics::plot(y, km2, type = "l"))
#'
#' ## Comparison with a spherical Earth
#' # A spherical Earth underestimates cell areas
#' # at mid latitudes compared to cell areas on a WGS84 ellipsoid as here
#' rel_spherical <- cos(cell_areas2[, "y"] * pi / 180)
#' graphics::plot(
#'   abs(cell_areas2[, "y"]),
#'   max(cell_areas2[, "km2"]) * (cell_areas2[, "rel"] - rel_spherical),
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
  ...
) {

  if (inherits(grid, "Raster")) {
    stopifnot(requireNamespace("raster"))
    grid <- terra::rast(grid)
  }

  stopifnot(inherits(grid, c("stars", "SpatRaster")))

  has_x <- !(missing(x) || is.null(x))

  if (has_x) {
    x <- as_points(x, to_class = "sf", crs = crs)

    if (sf::st_crs(x) != sf::st_crs(grid)) {
      x <- sf::st_transform(x, crs = sf::st_crs(grid))
    }

    coords <- sf::st_coordinates(x)[, 1:2]
    colnames(coords) <- c("x", "y")

  } else {
    coords <- if (inherits(grid, "SpatRaster")) {
      terra::crds(grid)

    } else if (inherits(grid, "stars")) {
      sf::st_coordinates(grid, center = TRUE)[, 1:2, drop = FALSE]
    }
  }

  cells <- data.frame(coords, km2 = NA, rel = NA)


  # Prepare grid on equator
  res <- if (inherits(grid, "SpatRaster")) {
    terra::res(grid)
  } else if (inherits(grid, "stars")) {
    stars::st_res(grid)
  }

  eq0 <- terra::rast(
    xmin = - res[[1L]] / 2L, xmax = res[[1L]] / 2L,
    ymin = - res[[2L]] / 2L, ymax = res[[2L]] / 2L,
    resolution = res,
    crs = "OGC:CRS84"
  )
  eq0[] <- 1L # nolint: extraction_operator_linter.


  if (inherits(grid, "SpatRaster")) {
    # Calculate area of requested cells -- terra
    tmpa <- terra::cellSize(grid, unit = "km")
    if (has_x) {
      tmpa <- terra::extract(tmpa, coords)
    }
    cells[["km2"]] <- as.data.frame(tmpa)[[1L]]

    # Calculate area of a cell on the equator -- terra
    eq <- suppressWarnings(
      try(
        terra::project(eq0, y = terra::crs(grid), res = res),
        silent = TRUE
      )
    )

    cell_maxarea_km2 <- if (inherits(eq, "try-error")) {
      NA_real_
    } else {
      max(as.data.frame(terra::cellSize(eq, unit = "km"))[[1L]])
    }

  } else if (inherits(grid, "stars")) {
    stopifnot(requireNamespace("units"))

    # Calculate area of requested cells -- stars
    tmpa <- sf::st_area(grid)
    units(tmpa[["area"]]) <- "km^2"

    tmp <- if (has_x) {
      stars::st_extract(tmpa, x)[["area"]]
    } else {
      as.vector(tmpa[["area"]])
    }

    cells[["km2"]] <- as.numeric(tmp)

    # Calculate area of a cell on the equator -- stars
    stopifnot(requireNamespace("lwgeom")) # required but not loaded by stars
    eq <- suppressWarnings(
      try(
        stars::st_warp(
          stars::st_as_stars(eq0), crs = sf::st_crs(grid), cellsize = res
        ),
        silent = TRUE
      )
    )

    cell_maxarea_km2 <- if (inherits(eq, "try-error")) {
      NA_real_
    } else {
      max(as.vector(units::set_units(sf::st_area(eq)[["area"]], "km^2")))
    }
  }

  cells[["rel"]] <- cells[["km2"]] / cell_maxarea_km2

  cells
}


#' Calculate "nominal resolution" of a grid
#'
#' @param grid
#'   A \code{\link[terra:SpatRaster-class]{terra::rast}},
#'   a \code{\link[raster:Raster-class]{raster::Raster}} or
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
# nolint start: line_length_linter.
#'   \href{https://docs.google.com/document/d/1h0r8RZr_f3-8egBMMh7aqLwy3snpD6_MrDz1q8n5XUk/edit#bookmark=id.ibeh7ad2gpdi}{Appendix 2: Algorithms for Defining the "nominal_resolution" Attribute}
# nolint end
#'
#' @examples
#' r1 <- terra::rast(
#'   xmin = -120, xmax = -90,
#'   ymin = 30, ymax = 50,
#'   crs = "OGC:CRS84",
#'   resolution = c(0.5, 0.5)
#' )
#' xy <- terra::spatSample(r1, size = 200L, as.points = TRUE)
#' r1[xy] <- 1
#'
#' calculate_nominal_resolution(r1)
#' calculate_nominal_resolution(stars::st_as_stars(r1))
#'
#' r2 <- terra::rast(
#'   xmin = -2480000, xmax = 90000,
#'   ymin = 650000, ymax = 4020000,
#'   crs = "EPSG:6350",
#'   resolution = c(10000, 10000)
#' )
#' xy <- terra::spatSample(r2, size = 200L, as.points = TRUE)
#' r2[xy] <- 1
#'
#' calculate_nominal_resolution(r2)
#' calculate_nominal_resolution(stars::st_as_stars(r2))
#'
#' @export
calculate_nominal_resolution <- function(grid, maskvalue = NA) {
  # For a land surface model calculated on its own grid,
  # include all land grid cells

  if (inherits(grid, "Raster")) {
    stopifnot(requireNamespace("raster"))
    grid <- terra::rast(grid)
  }

  stopifnot(
    inherits(grid, c("stars", "SpatRaster")),
    requireNamespace("units")
  )

  if (!isTRUE(is.na(maskvalue))) {
    warning(
      "maskvalue = ", shQuote(maskvalue), ": currently only NA is supported"
    )
  }


  # For each grid cell, calculate the distance (in km) between each pair of
  # cell vertices and select the maximum distance ("dmax").
  # For latxlon grid cells, for example, dmax would be the diagonal distance.

  if (inherits(grid, "SpatRaster")) {
    tmpg <- terra::as.polygons(grid, aggregate = FALSE, na.rm = TRUE)
    tmpc <- terra::geom(tmpg)[, c(3L, 4L, 1L)]

  } else if (inherits(grid, "stars")) {
    tmpg <- sf::st_as_sf(grid, as_points = FALSE, na.rm = TRUE)
    tmpc <- sf::st_coordinates(
      sf::st_cast(sf::st_geometry(tmpg), "MULTIPOINT")
    )
  }

  ns_coords <- colnames(tmpc)[1:2]
  crs <- sf::st_crs(grid)

  tmpd <- by(
    tmpc[, 1:2, drop = FALSE],
    INDICES = tmpc[, 3L],
    FUN = function(pts) {
      tmp <- sf::st_as_sf(pts, coords = ns_coords, crs = crs)
      as.numeric(units::set_units(max(sf::st_distance(tmp)), "km"))
    },
    simplify = FALSE
  )

  mean_resolution_km <- mean(do.call(c, tmpd))


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
