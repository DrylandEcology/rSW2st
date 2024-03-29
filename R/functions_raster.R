
#' Fills a raster grid with variable values associated with geographic locations
#'
#' @param data A vector or two-dimensional object. Elements/rows correspond to
#'   \code{site_locations}; columns (if present) represent variables.
#' @param site_locations An object that described geographic locations of
#'   \code{data} that can be sent to \code{\link[rSW2st]{as_points}}
#' @param site_crs The \code{crs} of \code{site_locations} passed to
#'   \code{\link[rSW2st]{as_points}}.
#' @param grid A \var{Raster*} object used as template.
#' @param filename A character string. Passed to \code{\link[raster]{brick}}.
#'
#' @return A \code{\link[raster:RasterLayer-class]{raster::RasterLayer}}
#'   (if \code{data} is a vector) or
#'   \code{\link[raster:RasterBrick-class]{raster::RasterBrick}}
#'   (if \code{data} is two-dimensional).
#'
#' @examples
#' if (requireNamespace("raster")) {
#'   r <- raster::raster(
#'     xmn = 0, xmx = 10,
#'     ymn = 0, ymx = 10,
#'     crs = "OGC:CRS84",
#'     resolution = c(1, 1)
#'   )
#'
#'   rv <- create_raster_from_variables(
#'     data = 1:10,
#'     site_locations = as_points(
#'       0.5 + cbind(0:9, 0:9),
#'       crs = "OGC:CRS84",
#'       to_class = "sf"
#'     ),
#'     grid = r
#'   )
#' }
#'
#' @export
create_raster_from_variables <- function(
  data,
  site_locations,
  grid,
  site_crs = sf::st_crs(site_locations),
  filename = ""
) {
  stopifnot(requireNamespace("raster"))

  # prepare locations
  loc <- rSW2st::as_points(site_locations, to_class = "sf", crs = site_crs)

  if (sf::st_crs(loc) != sf::st_crs(grid)) {
    loc <- sf::st_transform(loc, crs = sf::st_crs(grid))
  }

  coords <- sf::st_coordinates(loc)

  # prepare data
  nl <- NCOL(data)
  cnames <- colnames(data)

  # Attempt to convert data to a numeric type, if not, so that it can be
  # converted to a raster object
  if (!(is.numeric(data) || all(vapply(data, is.numeric, FUN.VALUE = NA)))) {
    if (nl > 1) {
      for (k in seq_len(nl)) {
        tmp <- try(
          if (is.factor(data[, k])) {
            as.integer(data[, k])
          } else {
            as.double(data[, k])
          }
        )
        stopifnot(!inherits(tmp, "try-error"))
        data[, k] <- tmp
      }

    } else {
      data <- try(
        if (is.factor(data)) {
          as.integer(data)
        } else {
          as.double(data)
        }
      )
      stopifnot(!inherits(data, "try-error"))
    }
  }

  if (nl == 1) {
    data <- matrix(data, ncol = 1)
  }

  # create raster, init with NAs, and add data
  ids <- NULL
  rl <- list()
  if (nl > 1) {
    filenameks <- vapply(
      seq_len(nl),
      raster::rasterTmpFile,
      FUN.VALUE = NA_character_
    )
  }

  for (k in seq_len(nl)) {
    rk <- raster::init(grid, fun = function(x) rep(NA, x))
    if (k == 1) {
      ids <- raster::cellFromXY(rk, xy = coords)
    }

    rk[ids] <- data[, k]

    if (nl > 1) {
      rk <- raster::writeRaster(rk, filename = filenameks[k])
    }

    rl <- c(rl, rk)
  }

  if (nl > 1) {
    names(rl) <- cnames
    # first convert list to stack before passing to brick because
    # raster v2.9.6 the list-method of brick ignores all ... arguments
    r <- raster::brick(raster::stack(rl), filename = filename)
    unlink(filenameks)

  } else {
    r <- rl[[1L]]
  }


  # set datatype
  raster::dataType(r) <- get_raster_datatype(data)

  r
}



#' Convert \code{\link{typeof}} to \code{\link[raster]{dataType}} types
#'
#' @references Relevant code adapted from \code{`raster:::dataType<-`}
#' @noRd
get_raster_datatype <- function(data) {
  supported_types <- c(
    "DOUBL", "NUMER", "FLOAT", "SINGL", "REAL", "INTEG", "SMALL",
    "BYTE", "LOGIC"
  )

  tmp <- substr(toupper(typeof(data)), 1, 5)
  stopifnot(tmp %in% c(supported_types, "LIST"))

  if (tmp == "LIST") {
    tmp <- unique(
      vapply(
        data,
        function(x) substr(toupper(typeof(x)), 1, 5),
        FUN.VALUE = NA_character_
      )
    )
    stopifnot(tmp %in% supported_types)

    tmp <- if (any(tmp == "DOUBL")) {
      "DOUBL"
    } else if (any(tmp %in% c("NUMER", "FLOAT", "SINGL", "REAL"))) {
      "REAL"
    } else if (any(tmp == "INTEG")) {
      "INTEG"
    } else if (any(tmp == "SMALL")) {
      "SMALL"
    } else if (any(tmp == "BYTE")) {
      "BYTE"
    } else if (any(tmp == "LOGIC")) {
      "LOGIC"
    }
  }

  switch(
    EXPR = tmp,
    LOGIC = "LOG1S",
    BYTE = "INT1U",
    SMALL = "INT2S",
    INTEG = "INT4S",
    NUMER = , FLOAT = , SINGL = , REAL = "FLT4S",
    DOUBL = "FLT8S"
  )
}


#' Polygon around \var{gridcells} with a value larger than or equal to the
#' specified threshold
#'
#' @param grid A two-dimensional
#'   \code{\link[raster:RasterLayer-class]{raster::RasterLayer}},
#'   \code{\link[terra:SpatRaster-class]{terra::SpatRaster}}, or
#'   \code{stars} object.
#' @param alpha A numeric value. Threshold value.
#'
#' @return A \code{sf} polygon
#'
#' @examples
#' r <- stars::st_as_stars(
#'   data.frame(
#'     x = rep(1:10, times = 10),
#'     y = rep(10:1, each = 10),
#'     value = 1:100
#'   ),
#'   dims = c("x", "y")
#' )
#'
#' ip1 <- isoline_from_raster(r, alpha = 45)
#' ip2 <- isoline_from_raster(r, alpha = 87)
#'
#' if (requireNamespace("grDevices")) {
#'   plot(r, reset = FALSE)
#'   plot(ip1, col = NA, border = "black", add = TRUE)
#'   plot(ip2, col = NA, border = "blue", add = TRUE)
#'   grDevices::dev.off()
#' }
#'
#' @export
isoline_from_raster <- function(grid, alpha) {
  stopifnot(inherits(grid, c("Raster", "SpatRaster", "stars")))

  if (inherits(grid, c("Raster", "SpatRaster"))) {
    grid <- stars::st_as_stars(grid)
  }

  stopifnot(length(dim(grid)) >= 2)

  tmp <- stars::st_apply(
    grid,
    MARGIN = 1:2,
    FUN = function(x) ifelse(x >= alpha, 1L, NA)
  )
  sf::st_make_valid(sf::st_as_sf(tmp, as_points = FALSE, merge = TRUE))
}
