#' Interaction of \var{rSW2} objects with \var{netCDFs}
#'
#' @param data A numeric array or vector (optional). A vector is converted
#'   to a one-column matrix.
#'
#' @param grid An object that describes a gridded \var{xy-space}.
#'  Regular, rectangular grids are the only currently supported type.
#'  This can be \itemize{
#'    \item a \code{\link[raster:RasterLayer-class]{raster::RasterLayer}}
#'          object,
#'    \item a \code{stars::stars} object,
#'    \item a file name pointing to such a raster on disk;
#'    \item a list, such as the one produced by \code{\link{xy_from_grid}}.
#'    \item an object with coordinate values for all \var{gridcell} centers
#'          that can be passed to \code{\link{as_points}};
#'  }
#'  The \var{crs} of the grid coordinate values must match the one of the
#'  data locations.
#'
#' @param locations An object from which \var{x} and \var{y} coordinate values
#'   can be extracted that describe the \var{xy} locations of each \code{data}
#'   row, e.g., a matrix or \var{data.frame} or a spatial points object
#'   inheriting from \var{sf} or \var{Spatial*}.
#' @param locations_crs An object which is a \var{crs} or
#'   from which one can be derived
#'   that describes the \var{crs} of \code{locations}.
#'
#' @param data_str A character string describing the dimensions of \code{data}
#'   where \var{"xy"} stands for \var{x} and \var{y} spatial dimensions
#'   if the spatial structure is gridded,
#'   while \var{"s"} stands for \var{site} if the spatial structure are
#'   discrete points;
#'   \var{z} stands for a vertical dimension; and \var{t} stands for a
#'   temporal dimension.
#'
#' @name rSW2st_netCDF
#' @aliases netCDF netcdf
NULL


#' Create a \var{netCDF} file (with or without data)
#'
#' The user describes a data array and specifies
#' spatial, vertical, and time information,
#' and metadata to create a \var{netCDF} in \var{netcdf-4 format}
#' according to \var{CF-1.8} standards.
#'
#'
#' @inheritParams rSW2st_netCDF
#' @param filename A character string. The name of the \var{netCDF} file.
#' @param xyspace An object that describes the \var{xy-space} of the
#'   \var{netCDF} file.
#'   If \code{xyspace} does not contain a \var{crs},
#'   then it is assumed that the \var{crs} is \var{crs_attributes[["crs_wkt"]]}.
#'   If data are gridded, then passed to \code{\link{xy_from_grid}};
#'   if non-gridded, then passed to \code{\link{as_points}}.
#' @param data_dims A list as returned by \code{\link{get_data_dims}}.
#'   If \code{NULL} and \code{data} is not missing, then calculated from
#'   \code{data}.
#' @param data_type A character string. The \var{netCDF} data type.
#' @param var_attributes A list of named character strings defining the
#'   \var{netCDF} variable(s).
#'   Elements \var{name} and \var{units} are required.
#' @param xy_attributes A list of named character strings defining the
#'   \var{netCDF} dimensions for the \var{xy-space}.
#'   Elements \var{name}, \var{standard_name}, \var{long_name},
#'   and \var{units} are required.
#' @param crs_attributes A list of named character strings defining the
#'   \var{netCDF} \var{crs} of the \var{xy-space}.
#'   Elements \var{crs_wkt} and \var{grid_mapping_name} are required.
#' @param check_crs A logical value. If \code{TRUE} then check that
#'   the \var{crs} provided via \code{crs_attributes} matches the ones
#'   from \code{locations} and \code{grid} if available.
#' @param time_values A numeric vector or \code{NULL}. The values along the
#'   time dimension (if present).
#'   In units as described by \code{time_attributes}.
#' @param type_timeaxis A character string. Describing if the time dimension
#'   represents a time series or a climatological time.
#' @param time_attributes A list of named character strings defining the
#'   \var{netCDF} time dimension.
#'   Elements \var{calendar}, \var{units}, and \var{unlim} are required.
#' @param time_bounds A numeric vector or two-dimensional matrix.
#'   The start and end of each time (or climatological) unit.
#' @param vertical_values A numeric vector or \code{NULL}. The values along the
#'   vertical dimension (if present).
#'   In units as described by \code{vertical_attributes}.
#' @param vertical_attributes A list of named character strings defining the
#'   \var{netCDF} vertical dimension, e.g., soil depth.
#'   Elements \var{units} and \var{positive} are required.
#' @param vertical_bounds A numeric vector or two-dimensional matrix.
#'   The upper/lower limits of each vertical unit.
#' @param global_attributes A list of named character strings defining the
#'   global attributes of the \var{netCDF}.
#' @param overwrite A logical value. If \code{TRUE}, file will be overwritten
#'   if it already exists.
#' @param nc_compression A logical value. If \code{TRUE}, then the \var{netCDF}
#'   is created using compression arguments
#'   \code{nc_shuffle}, \code{nc_deflate}, and \var{nc_chunks}. Compression
#'   is turned off by default.
#' @param nc_shuffle A logical value. If \code{TRUE}, then the shuffle filter
#'   is turned on which can improve compression.
#'   Used only if \code{nc_compression} is activated \code{TRUE}.
#' @param nc_deflate An integer between 1 and 9 (with increasing)
#'   compression or \code{NA} to turn off compression.
#'   Used only if \code{nc_compression} is activated \code{TRUE}.
#' @param nc_chunks A character string, \code{NA}, or an integer vector.
#'   See details. The default \var{"by_zt"} is to create chunks for the
#'   entire \var{xy-space} and for each vertical and each time step.
#'   Used only if \code{nc_compression} is activated \code{TRUE}.
#' @param verbose A logical value.
#'
#' @return This function is used for the side-effect of creating a \var{netCDF}
#'   file.
#'   Data values are written to the file if provided as argument \code{data}.
#'
#'
#' @section Details:
#' Values can be written to the file at a later time using function
#' \code{\link{populate_netCDF}}.
#'
#' The created \var{netCDF} is suitable for three data situations:
#' \enumerate{
#'    \item one variable and \var{xy-space}, time and vertical dimensions
#'    \item one variable and \var{xy-space} and time or vertical dimensions
#'    \item one or multiple variables and \var{xy-space} dimensions
#'          without time/vertical dimensions
#' }
#'
#'
#' @section Spatial setup:
#' Spatial information about the \var{xy-space} is derived from the arguments
#' \code{xyspace}, \code{xy_attributes}, \code{crs_attributes}, \code{data_str},
#' and \code{data_dims}.
#'
#' The \var{xy-space} is either gridded (determined by the
#' first two characters of \code{data_str} equal to \var{"xy"}),
#' or list of discrete points/sites
#' (determined by the first character of \code{data_str} equal to \var{"s"}).
#'
#' \itemize{
#'   \item The gridded situation creates \var{x} and \var{y} dimensions and
#'     associated variables in the \var{netCDF} file.
#'     The size of the \var{xy-space} must agree
#'     with the elements \var{"n_x"} and \var{"n_y"} of \code{data_dims} and,
#'     thus, with the two first dimensions of \code{data}, if available.
#'
#'   \item The discrete point/site situation creates a \var{site} dimension and
#'     associated variable as well as \var{x} and \var{y} variables
#'     for the spatial coordinate values of the sites; see
#nolint start
#'     \href{http://cfconventions.org/Data/cf-conventions/cf-conventions-1.8/cf-conventions.html#point-data}{CF point-data}.
#nolint end
#'     The code will add a \var{coordinates} attribute to the variable(s) and
#'     a \var{featureType = "point"} global attribute.
#'     The size of the \var{xy-space}, i.e., the number of sites, must agree
#'     with the element \var{"n_s"} of \code{data_dims} and,
#'     thus, with the first dimension of \code{data}, if available.
#' }
#'
#' The \var{crs} are checked by default (see argument \code{check_crs}) for
#' consistency among \code{crs_atttributes}, \code{locations}, and/or
#' \code{grid}. However, this check may fail when \code{locations} and/or
#' \code{grid} use a \var{PROJ.4} representation that doesn't compare well with
#' a \var{WKT2} representation provided by \code{crs_atttributes} even if they
#' are the same. Turn off these checks in such cases.
#'
#'
#' @section Spatial dimensions of data:
#' For the gridded situation, \code{data} array must be arranged
#' in "expanded" spatial format, i.e.,
#' the two dimensions of the \code{data} array span to the
#' \var{xy-space}.
#' The first dimension, i.e., \var{X}, matches \var{gridcells}
#' along \var{longitude} or a projected \var{x} coordinate and
#' the second dimension, i.e., \var{Y}, matches \var{gridcells}
#' along \var{latitude} or a projected \var{y} coordinate.
#' The \code{xy_attributes[["name"]][1:2]} defines the names of
#' the \var{x} and \var{y} dimensions/variables.
#'
#' However, "gridded" data objects are frequently organized by "collapsed"
#' \var{x} and \var{y} dimensions, e.g., to achieve a sparse representation.
#' Use the function \code{\link{convert_xyspace}} to expand sparse data arrays
#' before their use by function \code{\link{create_netCDF}}.
#'
#' For the discrete point/site situation, \code{data} array must be arranged
#' in "collapsed" spatial format, i.e.,
#' the first dimension (rows) of \code{data} corresponds to the number of
#' points/sites.
#'
#'
#' @section Non-spatial dimensions of data:
#' The first non-spatial dimension of a \code{data} array, if present,
#' corresponds to
#' \itemize{
#'   \item multiple variables, if \code{data_str} is \var{"xy"} or \var{"s"}
#'   \item time dimension, if \code{data_str} is \var{"xyt"} or \var{"st"}
#'   \item vertical dimension, if \code{data_str} is
#'         \var{"xyzt"}, \var{"xyz"}, \var{"szt"}, or \var{"sz"}
#' }
#'
#' The second non-spatial dimension of the \code{data} array, if present,
#' corresponds to the time dimension;
#' this situation arises only in the presence of
#' both a time and vertical dimension,
#' i.e., \code{data_str} is \var{"xyzt"} or \var{"szt"}.
#'
#'
#' @section Variables:
#' Use \var{CMIP6} standard variable names, units, etc., where available.
#' Standardized variable names can be searched in the
#nolint start
#' \href{https://github.com/PCMDI/cmip6-cmor-tables/tree/master/Tables}{CMIP6-cmor-tables}
#nolint end
#'
#' @section Chunking:
#' The argument \code{nc_chunks} offers two auto-determined chunking schemes:
#' \describe{
#'   \item{"by_zt"}{
#'     create chunks for the entire \var{xy-space} and
#'     for each vertical and each time step
#'   }
#'   \item{"by_t"}{
#'     create chunks for the entire \var{xy-space} and all vertical steps
#'     (if present) and for each time step
#'   }
#' }
#' Alternatively, the user can provide an integer vector with a length
#' equal to the number of the dimensions according to \code{data_dims}.
#'
#' @seealso \code{\link{populate_netCDF}}, \code{\link{read_netCDF}}
#nolint start
#' @references \href{https://cfconventions.org/Data/cf-conventions/cf-conventions-1.8/cf-conventions.html}{CF conventions}
#nolint end
#'
#'
#' @examples
#' # Prepare data for examples
#' tmp_nc <- create_example_netCDFs(tempdir(), c("xyt", "szt"), "timeseries")
#' data_xyt <- read_netCDF(tmp_nc[["xyt"]], "array", xy_names = c("x", "y"))
#' data_szt <- read_netCDF(tmp_nc[["szt"]], "array", xy_names = c("x", "y"))
#'
#' # Prepare attribute lists
#' nc_att_global <- list(
#'   title = "Example netCDF of package rSW2st",
#'   version = paste0("v", format(Sys.Date(), "%Y%m%d")),
#'   source_id = "SOILWAT2",
#'   further_info_url = "https://github.com/DrylandEcology/",
#'   source_type = "LAND",
#'   realm = "land",
#'   product = "model-output",
#'   grid = "native Alberts projection grid with NAD83 datum",
#'   grid_label = "gn",
#'   nominal_resolution = "1 m"
#' )
#'
#' nc_att_crs <- list(
#'   crs_wkt = sf::st_crs("EPSG:6350")$Wkt,
#'   grid_mapping_name = "albers_conical_equal_area",
#'   standard_parallel = c(29.5, 45.5),
#'   longitude_of_central_meridian = -96.0,
#'   latitude_of_projection_origin = 23.0,
#'   false_easting = 0.0,
#'   false_northing = 0.0,
#'   # GRS 1980 ellipsoid
#'   longitude_of_prime_meridian = 0,
#'   semi_major_axis = 6378137.0,
#'   inverse_flattening = 298.257222101
#' )
#'
#' nc_att_xy <- list(
#'   name = c("x", "y"),
#'   standard_name = c("projection_x_coordinate", "projection_y_coordinate"),
#'   long_name = c("x coordinate of projection", "y coordinate of projection"),
#'   units = c("m", "m")
#' )
#'
#' ## Write netCDF for gridded data
#' tmp_nc[["xyt2"]] <- sub(".nc", "2.nc", tmp_nc[["xyt"]])
#'
#' create_netCDF(
#'   filename = tmp_nc[["xyt2"]],
#'   xyspace = data_xyt[["xyspace"]],
#'   data = data_xyt[["data"]],
#'   data_str = "xyt",
#'   var_attributes = list(name = "sine", units = "1"),
#'   xy_attributes = nc_att_xy,
#'   crs_attributes = nc_att_crs,
#'   time_values = data_xyt[["time_values"]],
#'   type_timeaxis = "timeseries",
#'   global_attributes = nc_att_global
#' )
#'
#' data_xyt2 <- read_netCDF(tmp_nc[["xyt2"]], "array", xy_names = c("x", "y"))
#' all.equal(data_xyt2[["data"]], data_xyt[["data"]])
#'
#'
#' ## Write netCDF for discrete data
#' tmp_nc[["szt2"]] <- sub(".nc", "2.nc", tmp_nc[["szt"]])
#'
#' create_netCDF(
#'   filename = tmp_nc[["szt2"]],
#'   xyspace = as.data.frame(data_szt[["xyspace"]][1:2]),
#'   data = data_szt[["data"]],
#'   data_str = "szt",
#'   var_attributes = list(name = "sine", units = "1"),
#'   xy_attributes = nc_att_xy,
#'   crs_attributes = nc_att_crs,
#'   time_values = data_szt[["time_values"]],
#'   type_timeaxis = "timeseries",
#'   vertical_values = data_szt[["vertical_values"]],
#'   vertical_attributes = list(units = "m", positive = "down"),
#'   global_attributes = nc_att_global
#' )
#'
#'
#' data_szt2 <- read_netCDF(tmp_nc[["szt2"]], "array", xy_names = c("x", "y"))
#' all.equal(data_szt2[["data"]], data_szt[["data"]])
#'
#' # Cleanup
#' unlink(unlist(tmp_nc))
#'
#' @export
create_netCDF <- function(
  filename,
  xyspace,
  data = NULL,
  data_str = c("xyzt", "xyt", "xyz", "xy", "szt", "st", "sz", "s"),
  data_dims = get_data_dims(data_str, dim(data)),
  data_type = c("double", "float", "integer", "short", "byte", "char"),
  var_attributes = list(
    name = "",
    standard_name = "",
    long_name = "",
    units = "",
    grid_mapping = "crs",
    cell_methods = "",
    cell_measures = ""
  ),
  xy_attributes = list(
    name = c("lon", "lat"),
    standard_name = c("longitude", "latitude"),
    long_name = c("Longitude", "Latitude"),
    units = c("degrees_east", "degrees_north")
  ),
  crs_attributes = list(
    crs_wkt = sf::st_crs("OGC:CRS84")$Wkt,
    grid_mapping_name = "latitude_longitude",
    longitude_of_prime_meridian = 0.0,
    semi_major_axis = 6378137.0,
    inverse_flattening = 298.257223563
  ),
  check_crs = TRUE,
  time_values = NULL,
  type_timeaxis = c("timeseries", "climatology"),
  time_attributes = list(
    units = "days since 1900-01-01",
    calendar = "standard",
    unlim = FALSE
  ),
  time_bounds = matrix(NA, nrow = length(time_values), ncol = 2),
  vertical_values = NULL,
  vertical_attributes = list(
    units = "",
    positive = "down"
  ),
  vertical_bounds = matrix(NA, nrow = length(vertical_values), ncol = 2),
  global_attributes = list(title = "Title"),
  overwrite = FALSE,
  nc_compression = FALSE,
  nc_shuffle = TRUE,
  nc_deflate = 5,
  nc_chunks = "by_zt",
  verbose = FALSE
) {
  #------ 1) Checks/preparations -----------------------------------------------
  stopifnot(requireNamespace("ncdf4"))

  has_compression <- isTRUE(nc_compression)
  stopifnot(nc_deflate %in% c(NA, 1:9))

  nc_deflate <- if (has_compression) nc_deflate else NA

  has_chunks <- has_compression && !is.na(nc_chunks)
  has_predet_chunks <- is.character(nc_chunks)
  if (has_compression && has_predet_chunks) {
    stopifnot(nc_chunks %in% c("by_zt", "by_t"))
  }

  nc_shuffle <-
    has_compression && isTRUE(nc_shuffle) &&
    data_type %in% c("integer", "short")


  #------ netCDF filename ------
  if (file.exists(filename)) {
    if (overwrite) {
      unlink(filename)
    } else {
      warning(
        "File ", shQuote(basename(filename)),
        " exists and 'overwrite' is FALSE; returning early."
      )
      return(invisible(FALSE))
    }
  }


  #------ data characterization ------
  has_data <- !missing(data) && !is.null(data)

  data_type <- match.arg(data_type)

  NAflag <- switch(
    data_type,
    char = NULL,
    byte = NULL,
    short = -128L,
    integer = -2147483647L,
    float = -3.4e+38,
    double = -1.7e+308
  )


  # Three data structure situations:
  #   i) one variable and vertical axis and time ("xyzt", "szt")
  #   ii) one variable and time OR vertical axis ("xyt", "xyz", "st", "sz")
  #   iii) one/multiple variables and no time/vertical axis ("xy", "s")
  data_str <- match.arg(data_str)
  is_gridded <- isTRUE(substr(data_str, 1, 2) == "xy")


  #--- Check data dimensions/structure
  if (has_data) {
    data_dims_from_data <- get_data_dims(data_str, dim(data))
  }


  if (is.null(data_dims)) {
    data_dims <- data_dims_from_data

  } else {
    # Convert NULLs and non-finite values to NA
    data_dims <- lapply(
      data_dims,
      function(x) if (is.null(x) || !is.finite(x)) NA else x
    )

    # Convert list to named vector (now that we took care of possible NULLs)
    data_dims <- unlist(data_dims)

    # Check that provided `data_dims` match dimensions of argument `data`
    if (has_data) {
      if (
        !isTRUE(all.equal(
          data_dims_from_data,
          data_dims[names(data_dims_from_data)]
        ))
      ) {
        stop("Disagreement in dimensions between `data_dims` and `data`.")
      }
    }
  }


  # Check argument data dimensions
  stopifnot(
    c("ns", "nx", "ny", "nt", "nz", "nv") %in% names(data_dims),
    !anyNA(data_dims),
    data_dims["ns"] > 0 || data_dims["nx"] > 0 && data_dims["ny"] > 0
  )

  # Check that data structure is possible given data dimensions
  tmp <- switch(
    EXPR = data_str,

    `xyzt` = stopifnot(
      data_dims["nt"] > 0,
      data_dims["nz"] > 0,
      data_dims["nv"] == 0
    ),

    `xyt` = stopifnot(
      data_dims["nt"] > 0,
      data_dims["nz"] == 0,
      data_dims["nv"] == 0
    ),

    `xyz` = stopifnot(
      data_dims["nt"] == 0,
      data_dims["nz"] > 0,
      data_dims["nv"] == 0
    ),

    `xy` = stopifnot(
      data_dims["nt"] == 0,
      data_dims["nz"] == 0,
      data_dims["nv"] >= 0
    )
  )



  #------ xy-space ------
  if (is.null(xyspace) || missing(xyspace)) {
    stop("Must provide `xyspace` as argument.")
  }


  #--- xy attributes
  if (any(lengths(xy_attributes) != 2)) {
    stop("All `xy_attributes` must be of lenght two (xy-space).")
  }

  tmp_xy_atts <- c("name", "standard_name", "long_name", "units")
  if (any(!(tmp_xy_atts %in% names(xy_attributes)))) {
    stop(
      "`xy_attributes` must include ",
      paste0(shQuote(tmp_xy_atts), collapse = ", ")
    )
  }

  if ("axis" %in% names(xy_attributes)) {
    if (any(c("X", "Y") != toupper(xy_attributes[["axis"]]))) {
      stop(
        "`xy_attributes`: if `axis` is included, then its value must be ",
        "`X` and `Y`."
      )
    }
    xy_attributes[["axis"]] <- NULL
  }


  #--- crs attributes setup & info
  if ("crs_wkt" %in% names(crs_attributes)) {
    crs_wkt <- crs_attributes[["crs_wkt"]]
    crs_attributes[["crs_wkt"]] <- NULL

    # check that CRS is valid
    crs_wkt <- try(sf::st_crs(crs_wkt), silent = TRUE)
    if (!inherits(crs_wkt, "crs") || crs_wkt == sf::NA_crs_) {
      stop("`crs_attributes[[\"crs_wkt\"]]` does not represent a valid CRS.")
    }

  } else {
    stop("Need `crs_attributes[[\"crs_wkt\"]]`")
  }

  if (!("grid_mapping_name" %in% names(crs_attributes))) {
    stop("Need `crs_attributes[[\"grid_mapping_name\"]]`")
  }

  ns_att_crs <- names(crs_attributes)


  # `xyspace` is allowed to be an object without a crs
  crs_xyspace <- try(suppressWarnings(sf::st_crs(xyspace)), silent = TRUE)
  if (!inherits(crs_xyspace, "crs") || crs_xyspace == sf::NA_crs_) {
    crs_xyspace <- crs_wkt

    if (verbose) {
      message(
        "No `crs` could be extracted from `xyspace`; assuming that it is ",
        "`crs_attributes[[\"crs_wkt\"]]`."
      )
    }
  }


  # check that CRS definition matches CRS of xyspace (grid or locations)
  if (crs_xyspace != crs_wkt) {
    msg <- paste0(
      "The CRS given in `crs_attributes[[\"crs_wkt\"]]` needs to ",
      "match the CRS of the `xyspace` object. Currently, ",
      "`crs_attributes[[\"crs_wkt\"]]` is ", shQuote(crs_wkt$Wkt),
      " and the CRS of `xyspace` is ", shQuote(crs_xyspace$Wkt)
    )

    if (check_crs) stop(msg) else if (verbose) warning(msg)
  }


  if (is_gridded) {
    # Note: xvals are organized from west to east, yvals from south to north
    xy_grid <- try(
      xy_from_grid(xyspace, crs = crs_xyspace),
      silent = TRUE
    )

    if (inherits(xy_grid, "try-error")) {
      stop("Argument `xyspace` could not be interpreted as grid.")
    }

    # xy values
    xvals <- xy_grid[[1]]
    yvals <- xy_grid[[2]]
    n_xvals <- length(xvals)
    n_yvals <- length(yvals)

    if (data_dims["nx"] > n_xvals || data_dims["ny"] > n_yvals) {
      stop(
        "For gridded data, `data_dims[\"nx\"]` and `data_dims[\"ny\"]` ",
        "must be smaller or equal to the number of unique x or, respectively, ",
        "y coordinate values in `xyspace` for gridded data."
      )
    }

    # Grid resolution/bounds
    grid_res <- lapply(xy_grid, function(x) unique(diff(x)))
    check_res <- sapply(grid_res, function(x) diff(range(x)))

    if (any(check_res > sqrt(.Machine$double.eps))) {
      stop(
        "Coordinate intervals of `xyspace` are not constant ",
        "as is required for a grid."
      )
    }

    # Calculate x and y bounds
    grid_halfres <- c(grid_res[[1]][1], grid_res[[2]][1]) / 2
    x_bounds <- rbind(xvals - grid_halfres[1], xvals + grid_halfres[1])
    y_bounds <- rbind(yvals - grid_halfres[2], yvals + grid_halfres[2])

  } else {
    locs <- try(
      as_points(xyspace, to_class = "sf", crs = crs_wkt),
      silent = TRUE
    )

    if (inherits(locs, "try-error")) {
      stop("Argument `xyspace` could not be interpreted as location.")
    }

    n_sites <- nrow(locs)
    tmp_coord <- sf::st_coordinates(locs) # coordinates of point locations
    xvals <- tmp_coord[, 1]
    yvals <- tmp_coord[, 2]

    if (data_dims["ns"] > n_sites) {
      stop(
        "`data_dims[\"ns\"]` must be smaller or equal to ",
        "the number of sites in `xyspace` for discrete data sites or points."
      )
    }
  }



  #------ time axis ------
  type_timeaxis <- match.arg(type_timeaxis)

  n_time <- length(time_values)

  has_T_timeAxis <- if (data_dims["nt"] > 0) {
    "explicit"
  } else if (n_time > 0) {
    "implicit"
  } else {
    "none"
  }

  if (has_T_timeAxis %in% c("explicit", "implicit")) {
    #--- Check time values/dimension match
    if (has_T_timeAxis == "explicit" && data_dims["nt"] != n_time) {
      stop(
        "`data_dims[\"nt\"]` must match ",
        "the number of elements in `time_values`."
      )
    }

    if (has_T_timeAxis == "implicit" && n_time != 1) {
      stop(
        "If `data_dims[\"nt\"]` is zero, ",
        "then `time_values` can only have one value."
      )
    }

    #--- Check and conform time_bounds
    if (length(dim(time_bounds)) == 0) {
      if (n_time * 2 != length(time_bounds)) {
        stop(
          "Start and end required for each `time_values` ",
          "to define `time_bounds`"
        )      }

      time_bounds <- matrix(time_bounds, nrow = n_time, ncol = 2, byrow = TRUE)

    } else {
      if (!identical(dim(time_bounds), c(as.integer(n_time), 2L))) {
        stop(
          "Start and end required for each `time_values` ",
          "to define `time_bounds`"
        )
      }
    }

    #--- Identify type of time axis
    if (type_timeaxis == "timeseries") {
      varid_timebnds <- "time_bnds"
      att_timebnds <- "bounds"

    } else if (type_timeaxis == "climatology") {
      #nolint start
      # http://cfconventions.org/Data/cf-conventions/cf-conventions-1.8/cf-conventions.html#climatological-statistics
      #nolint end
      varid_timebnds <- "climatology_bounds" # not climatology_bnds!
      att_timebnds <- "climatology"
    }

    #--- Check time attributes
    if ("units" %in% names(time_attributes)) {
      time_units <- time_attributes[["units"]]
      time_attributes[["units"]] <- NULL
    } else {
      stop("Need units attribute in time attribute list")
    }

    if ("calendar" %in% names(time_attributes)) {
      time_cal <- time_attributes[["calendar"]]
      time_attributes[["calendar"]] <- NULL
    } else {
      stop("Need calendar attribute in time attribute list")
    }

    if ("unlim" %in% names(time_attributes)) {
      time_unlim <- as.logical(time_attributes[["unlim"]])
      time_attributes[["unlim"]] <- NULL
    } else {
      stop("Need unlim attribute in time attribute list")
    }

    if ("axis" %in% names(time_attributes)) {
      if ("T" != toupper(time_attributes[["axis"]])) {
        stop(
          "`time_attributes`: ",
          "if `axis` is included, then its value must be `T`"
        )
      }
      time_attributes[["axis"]] <- NULL
    }

    if (att_timebnds %in% names(time_attributes)) {
      if (varid_timebnds != time_attributes[[att_timebnds]]) {
        stop(
          "`time_attributes`: ",
          "if ", shQuote(att_timebnds), " is included, then its value must be ",
          shQuote(varid_timebnds)
        )
      }
      time_attributes[[att_timebnds]] <- NULL
    }

    ns_att_time <- names(time_attributes)
  }


  #------ vertical axis ------
  n_vertical <- length(vertical_values)

  has_Z_verticalAxis <- if (data_dims["nz"] > 0) {
    "explicit"
  } else if (n_vertical > 0) {
    "implicit"
  } else {
    "none"
  }

  if (has_Z_verticalAxis %in% c("explicit", "implicit")) {

    #--- Check time values/dimension match
    if (has_Z_verticalAxis == "explicit" && data_dims["nz"] != n_vertical) {
      stop(
        "`data_dims[\"nz\"]` must match ",
        "the number of elements in `vertical_values`."
      )
    }

    if (has_Z_verticalAxis == "implicit" && n_vertical != 1) {
      stop(
        "If `data_dims[\"nz\"]` is zero, ",
        "then `vertical_values` can only have one value."
      )
    }

    #--- Check and conform vertical_bounds
    if (length(dim(vertical_bounds)) == 0) {
      if (n_vertical * 2 != length(vertical_bounds)) {
        stop(
          "Start and end values required for each `vertical_values` ",
          "to define `vertical_bounds`"
        )
      }

      vertical_bounds <- matrix(
        vertical_bounds,
        nrow = n_vertical,
        ncol = 2,
        byrow = TRUE
      )

    } else {
      if (!identical(dim(vertical_bounds), c(as.integer(n_vertical), 2L))) {
        stop(
          "Start and end values required for each `vertical_values` ",
          "to define `vertical_bounds`"
        )
      }
    }


    #--- Check vertical attributes
    if ("units" %in% names(vertical_attributes)) {
      vert_units <- vertical_attributes[["units"]]
      vertical_attributes[["units"]] <- NULL
    } else {
      stop("Need `units` attribute in vertical attribute list")
    }

    if (!("positive" %in% names(vertical_attributes))) {
      stop("Need `positive` attribute in vertical attribute list")
    }

    if ("axis" %in% names(vertical_attributes)) {
      if ("Z" != toupper(vertical_attributes[["axis"]])) {
        stop(
          "`vertical_attributes`: ",
          "if `axis` is included, then its value must be `Z`"
        )
      }
      vertical_attributes[["axis"]] <- NULL
    }

    if ("bounds" %in% names(vertical_attributes)) {
      if ("vertical_bnds" != vertical_attributes[["bounds"]]) {
        stop(
          "`vertical_attributes`: ",
          "if `bounds` is included, then its value must be `vertical_bnds`"
        )
      }
      vertical_attributes[["bounds"]] <- NULL
    }

    ns_att_vert <- names(vertical_attributes)
  }


  #------ Variables ------
  n_vars <- max(1, data_dims["nv"]) # at least one implicit variable

  if (any(lengths(var_attributes) != n_vars)) {
    stop("All variable attributes need a value for each variable.")
  }

  if ("name" %in% names(var_attributes)) {
    var_names <- var_attributes[["name"]]
    var_attributes[["name"]] <- NULL
  } else {
    stop("Need a `name` variable attribute.")
  }

  if (!"long_name" %in% names(var_attributes)) {
    var_attributes[["long_name"]] <- var_names
  }

  if ("units" %in% names(var_attributes)) {
    var_units <- var_attributes[["units"]]
    var_attributes[["units"]] <- NULL
  } else {
    stop("Need unit attribute in variable attribute list")
  }

  if (!("grid_mapping" %in% names(var_attributes))) {
    # This function creates only one grid_mapping and
    # the grid_mapping variable name is hard-coded to be "crs"
    var_attributes[["grid_mapping"]] <- paste(
      "crs:",
      # here, guessing axis order to be 1, 2
      # (that would be correct for OGC:CRS84, but not for EPSG:4326)
      xy_attributes[["name"]][1],
      xy_attributes[["name"]][2]
    )

    if (verbose) {
      message(
        "Adding `grid_mapping = \"", var_attributes[["grid_mapping"]], "\"`",
        " to variable attributes."
      )
    }

  } else {
    if (isTRUE(!grepl("\\<crs", var_attributes[["grid_mapping"]]))) {
      warning(
        "Variable attribute for `grid_mapping` should be 'crs: ...', but is ",
        shQuote(var_attributes[["grid_mapping"]])
      )
    }
  }

  if (!is_gridded) {
    #nolint start
    # http://cfconventions.org/Data/cf-conventions/cf-conventions-1.8/cf-conventions.html#point-data
    #nolint end
    if (!("coordinates" %in% names(var_attributes))) {
      tmp <- paste(xy_attributes[["name"]][2], xy_attributes[["name"]][1])
      if (has_T_timeAxis != "none") tmp <- paste("time", tmp)
      if (has_Z_verticalAxis != "none") tmp <- paste(tmp, "vertical")
      var_attributes[["coordinates"]] <- tmp

      if (verbose) {
        message("Adding `coordinates = \"", tmp, "\"` to variable attributes.")
      }
    }

    if (!("featureType" %in% names(global_attributes))) {
      global_attributes[["featureType"]] <- "point"

      if (verbose) {
        message("Adding `featureType = \"point\"` to global attributes.")
      }
    }
  }

  ns_att_vars <- names(var_attributes)



  #------ 2) Define netCDF dimensions ------------------------------------------
  #--- bounds dimension (without dimensional variable)
  bnddim <- ncdf4::ncdim_def(
    name = "bnds",
    units = "",
    vals = seq_len(2L),
    create_dimvar = FALSE
  )

  # x and y dimension
  if (is_gridded) {
    xdim <- ncdf4::ncdim_def(
      name = xy_attributes[["name"]][1],
      longname = xy_attributes[["long_name"]][1],
      units = xy_attributes[["units"]][1],
      vals = xvals
    )
    ydim <- ncdf4::ncdim_def(
      name = xy_attributes[["name"]][2],
      longname = xy_attributes[["long_name"]][2],
      units = xy_attributes[["units"]][2],
      vals = yvals
    )

    var_dims <- list(xdim, ydim)
    var_chunksizes <- if (has_chunks) c(n_xvals, n_yvals) else NA
    var_start <- c(1, 1)

  } else {
    idim <- ncdf4::ncdim_def(
      name = "site",
      longname = "SOILWAT2 simulation sites",
      units = "1",
      vals = seq_len(n_sites)
    )

    var_dims <- list(idim)
    var_chunksizes <- if (has_chunks) n_sites else NA
    var_start <- 1
  }

  # vertical dimension
  if (has_Z_verticalAxis != "none") {
    zdim <- ncdf4::ncdim_def(
      name = "vertical",
      units = vert_units,
      vals = vertical_values
    )

    var_dims <- c(var_dims, list(zdim))
    if (has_chunks && has_predet_chunks) {
      var_chunksizes <- c(
        var_chunksizes,
        if (nc_chunks == "by_zt") 1 else n_vertical
      )
    }
    var_start <- c(var_start, 1)
  }

  # time dimension
  if (has_T_timeAxis != "none") {
    tdim <- ncdf4::ncdim_def(
      name = "time",
      units = time_units,
      calendar = time_cal,
      unlim = time_unlim,
      vals = time_values
    )

    var_dims <- c(var_dims, list(tdim))
    if (has_chunks && has_predet_chunks) {
      var_chunksizes <- c(
        var_chunksizes,
        if (nc_chunks %in% c("by_zt", "by_t")) 1 else n_time
      )
    }
    var_start <- c(var_start, 1)
  }




  #------ 3) Define netCDF variables -------------------------------------------
  if (has_chunks && !has_predet_chunks) {
    stopifnot(length(nc_chunks) == length(var_dims))
    var_chunksizes <- nc_chunks
  }


  #------ Define data variables ------
  var_defs <- lapply(
    seq_len(n_vars),
    function(k) {
      ncdf4::ncvar_def(
        name = var_names[k],
        units = var_units[k],
        dim = var_dims,
        compression = nc_deflate,
        shuffle = nc_shuffle,
        chunksizes = if (has_chunks) var_chunksizes else NA,
        missval = NAflag,
        prec = data_type
      )
    }
  )


  #------ Define x and y as variables if not gridded ------
  if (!is_gridded) {
    xvar <- ncdf4::ncvar_def(
      name = xy_attributes[["name"]][1],
      longname = xy_attributes[["long_name"]][1],
      units = xy_attributes[["units"]][1],
      dim = list(idim),
      compression = nc_deflate,
      chunksizes = n_sites,
      missval = NAflag,
      prec = "double"
    )

    yvar <- ncdf4::ncvar_def(
      name = xy_attributes[["name"]][2],
      longname = xy_attributes[["long_name"]][2],
      units = xy_attributes[["units"]][2],
      dim = list(idim),
      compression = nc_deflate,
      chunksizes = n_sites,
      missval = NAflag,
      prec = "double"
    )

    var_defs <- c(var_defs, list(yvar, xvar))
  }


  #------ Define CRS ------
  crsdef <- ncdf4::ncvar_def(
    name = "crs",
    units = "",
    dim = list(),
    missval = NULL,
    prec = "integer"
  )


  #------ Define dimension bounds ------
  nc_dimvars <- if (is_gridded) {
    bnds_name <- paste0(xy_attributes[["name"]][1:2], "_bnds")

    if ("bounds" %in% names(xy_attributes)) {
      if (any(bnds_name != xy_attributes[["bounds"]])) {
        stop(
          "`xy_attributes`: ",
          "if `bounds` is included, then its value must be ",
          shQuote(bnds_name[1]), " and ", shQuote(bnds_name[2])
        )
      }
      xy_attributes[["bounds"]] <- NULL
    }

    list(
      ncdf4::ncvar_def(
        name = bnds_name[1],
        units = "",
        dim = list(bnddim, xdim),
        missval = NULL,
        compression = nc_deflate,
        chunksizes = c(2L, n_xvals),
        prec = "double"
      ),

      ncdf4::ncvar_def(
        name = bnds_name[2],
        units = "",
        dim = list(bnddim, ydim),
        missval = NULL,
        compression = nc_deflate,
        chunksizes = c(2L, n_yvals),
        prec = "double"
      )
    )

  } else {
    list()
  }

  if (has_Z_verticalAxis != "none") {
    vertbnddef <- ncdf4::ncvar_def(
      name = "vertical_bnds",
      units = "",
      dim = list(bnddim, zdim),
      missval = NULL,
      compression = nc_deflate,
      chunksizes = c(2L, n_vertical),
      prec = "double"
    )

    nc_dimvars <- c(nc_dimvars, list(vertbnddef))
  }

  if (has_T_timeAxis != "none") {
    tbnddef <- ncdf4::ncvar_def(
      name = varid_timebnds,
      units = "",
      dim = list(bnddim, tdim),
      missval = NULL,
      compression = nc_deflate,
      chunksizes = if (time_unlim) c(2L, 1L) else c(2L, n_time),
      prec = "double"
    )

    nc_dimvars <- c(nc_dimvars, list(tbnddef))
  }



  #------ 4) Write dimensions and attributes to netCDF file --------------------

  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)

  nc <- ncdf4::nc_create(
    filename = filename,
    vars = c(nc_dimvars, list(crsdef), var_defs),
    force_v4 = TRUE
  )

  on.exit(ncdf4::nc_close(nc))


  #------ Write dimensional variable values ------
  if (is_gridded) {
    #--- Write xy-bounds
    ncdf4::ncvar_put(
      nc,
      varid = bnds_name[1],
      vals = x_bounds,
      start = c(1, 1),
      count = c(2L, n_xvals)
    )

    ncdf4::ncvar_put(
      nc,
      varid = bnds_name[2],
      vals = y_bounds,
      start = c(1, 1),
      count = c(2L, n_yvals)
    )

  } else {
    #--- Write xy-coordinates to associated variables
    ncdf4::ncvar_put(
      nc,
      varid = xy_attributes[["name"]][1],
      vals = xvals,
      start = 1,
      count = n_sites
    )

    ncdf4::ncvar_put(
      nc,
      varid = xy_attributes[["name"]][2],
      vals = yvals,
      start = 1,
      count = n_sites
    )
  }

  if (has_Z_verticalAxis != "none") {
    ncdf4::ncvar_put(
      nc,
      varid = "vertical_bnds",
      vals = t(vertical_bounds),
      start = c(1, 1),
      count = c(2L, n_vertical)
    )
  }

  if (has_T_timeAxis != "none") {
    ncdf4::ncvar_put(
      nc,
      varid = varid_timebnds,
      vals = t(time_bounds),
      start = c(1, 1),
      count = c(2, n_time)
    )
  }



  #------ Write attributes ------

  #--- add standard_name attribute of x/y variables
  if ("standard_name" %in% names(xy_attributes)) {
    for (k in seq_len(2)) {
      ncdf4::ncatt_put(
        nc,
        varid = xy_attributes[["name"]][k],
        attname = "standard_name",
        attval = xy_attributes[["standard_name"]][k]
      )
    }
  }

  #--- add dimension attributes
  if (is_gridded) {
    ncdf4::ncatt_put(nc, xy_attributes[["name"]][1], "axis", "X")
    ncdf4::ncatt_put(nc, xy_attributes[["name"]][1], "bounds", bnds_name[1])
    ncdf4::ncatt_put(nc, xy_attributes[["name"]][2], "axis", "Y")
    ncdf4::ncatt_put(nc, xy_attributes[["name"]][2], "bounds", bnds_name[2])
  }

  if (has_Z_verticalAxis != "none") {
    ncdf4::ncatt_put(nc, "vertical", "axis", "Z")
    ncdf4::ncatt_put(nc, "vertical", "bounds", "vertical_bnds")

    for (natt in ns_att_vert) {
      ncdf4::ncatt_put(
        nc,
        varid = "vertical",
        attname = natt,
        attval = vertical_attributes[[natt]]
      )
    }
  }

  if (has_T_timeAxis != "none") {
    ncdf4::ncatt_put(nc, "time", "axis", "T")
    ncdf4::ncatt_put(nc, "time", att_timebnds, varid_timebnds)

    for (natt in ns_att_time) {
      ncdf4::ncatt_put(
        nc,
        varid = "time",
        attname = natt,
        attval = time_attributes[[natt]]
      )
    }
  }


  #--- add variable attributes
  for (k in seq_len(n_vars)) {
    for (natt in ns_att_vars) {
      ncdf4::ncatt_put(
        nc,
        varid = var_names[k],
        attname = natt,
        attval = var_attributes[[natt]][k]
      )
    }
  }

  #--- add coordinate system attributes
  for (natt in ns_att_crs) {
    ncdf4::ncatt_put(
      nc,
      varid = "crs",
      attname = natt,
      attval = crs_attributes[[natt]]
    )
  }

  ncdf4::ncatt_put(nc, "crs", attname = "crs_wkt", attval = crs_wkt$Wkt)


  #--- add global attributes
  ncdf4::ncatt_put(nc, varid = 0, attname = "Conventions", attval = "CF-1.8")

  ncdf4::ncatt_put(
    nc,
    varid = 0,
    attname = "created_by",
    attval = paste0(
      R.version[["version.string"]],
      ", R packages ",
      "ncdf4 v", getNamespaceVersion("ncdf4"),
      ", and ", system2("nc-config", "--version", stdout = TRUE, stderr = TRUE)
    )
  )

  ncdf4::ncatt_put(
    nc,
    varid = 0,
    attname = "creation_date",
    attval = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )

  if (!missing(global_attributes)) {
    ns_att_glob <- names(global_attributes)

    tmp <- c("Conventions", "created_by", "creation_date")
    if (has_T_timeAxis == "none") {
      tmp <- c(tmp, "time_label", "time_title")
    }
    has_replaced_gatts <- tmp[tmp %in% ns_att_glob]
    if (length(has_replaced_gatts) > 0) {
      warning(
        "`global_attributes` contained values for ",
        paste0(shQuote(has_replaced_gatts), collapse = ", "),
        "; they were replaced with an automatically generated value."
      )
      ns_att_glob <- setdiff(ns_att_glob, has_replaced_gatts)
    }

    for (natt in ns_att_glob) {
      ncdf4::ncatt_put(
        nc,
        varid = 0,
        attname = natt,
        attval = global_attributes[[natt]]
      )
    }
  }

  if (has_T_timeAxis == "none") {
    ncdf4::ncatt_put(nc, varid = 0, attname = "time_label", attval = "None")
    ncdf4::ncatt_put(
      nc,
      varid = 0,
      attname = "time_title",
      attval = "No temporal dimensions ... fixed field"
    )
  }


  #------ Add values (if provided) ---------------------------------------------
  if (has_data) {
    .populate_netCDF_nocheck(
      nc,
      data = data,
      var_names = var_names,
      data_str = data_str,
      is_gridded = is_gridded
    )
  }


  #------ The end --------------------------------------------------------------
  if (verbose) {
    message(
      "The netCDF has ", nc$nvars, " variables and ", nc$ndim, " dimensions"
    )
  }

  invisible(TRUE)
}




.populate_netCDF_nocheck <- function(
  x,
  data,
  var_names,
  data_str,
  is_gridded
) {
  stopifnot(requireNamespace("ncdf4"))

  if (!inherits(x, "ncdf4")) {
    x <- ncdf4::nc_open(filename = x, write = TRUE)
    on.exit(ncdf4::nc_close(x))
  }

  n_vars <- length(var_names)

  if ((data_str %in% c("xy", "s")) && n_vars > 1) {
    for (k in seq_len(n_vars)) {
      if (is_gridded) {
        values <- data[, , k]
        var_start <- c(1, 1)
        var_count <- c(-1, -1)

      } else {
        values <- data[, k]
        var_start <- 1
        var_count <- -1
      }

      ncdf4::ncvar_put(
        nc = x,
        varid = var_names[k],
        vals = values,
        start = var_start,
        count = var_count,
      )
    }

  } else {
    ncdf4::ncvar_put(nc = x, varid = var_names[1], vals = data)
  }
}


# TODO: write code for populate_netCDF
#' Write data to an existing \var{netCDF} file
#' @noRd
populate_netCDF <- function(
  filename,
  data,
  var_names,
  data_str = c("xyzt", "xyt", "xyz", "xy", "szt", "st", "sz", "s"),
  nc_name_crs = "crs",
  nc_name_crs_wkt = "crs_wkt",
  xy_names = c("lon", "lat"),
  time_ids = -1,
  vertical_ids = -1,
  ...
) {
  stopifnot(requireNamespace("ncdf4"), file.exists(filename))

  if (!inherits(filename, "ncdf4")) {
    x <- ncdf4::nc_open(filename = filename, write = FALSE, readunlim = FALSE)
    on.exit(ncdf4::nc_close(x))
  }

  stop("unfinished code.")
}


#' Read a \var{netCDF}
#'
#' Read a \var{netCDF} as produced by \code{create_netCDF}
#'
#' @inheritParams rSW2st_netCDF
#' @param x An object identifying a \var{netCDF} file, i.e.,
#'   a character string as file name or an object of class \var{ncdf4} derived
#'   from \code{ncdf4::nc_open}.
#' @param method A character string. Determines how the \var{netCDF} is read
#'   and if a spatial subset (by \code{locations}) is extracted.
#' @param var A character string. The variable name to be read. Passed along as
#'   \var{varname} for \var{rasters} or \var{var} for \var{stars} targets.
#' @param nc_name_crs A character string. The name of the \var{crs} variable
#'   in the \var{netCDF}.
#'   Function \code{\link{create_netCDF}} hard codes \var{"crs"}.
#' @param nc_name_crs_wkt A character string. The name of the attribute
#'   that holds the \var{WKT2} string of the \var{crs} variable
#'   in the \var{netCDF}.
#'   Function \code{\link{create_netCDF}} hard codes \var{"crs_wkt"}.
#' @param ... Additional arguments passed on to the specific functions.
#'
#' @return
#'   A named list including a data \var{array}
#'   (the list contains completed information to re-create the file with a
#'   call to \code{\link{create_netCDF}}),
#'   a \code{\link[raster:RasterLayer-class]{raster::RasterLayer}}, or
#'   a \code{stars::stars} object.
#'
#'
#' @section Notes:
#'   Reading discrete \var{netCDF}s,
#'   i.e., cases \var{"szt"}, \var{"st"}, \var{"sz"}, and \var{"s"},
#'   is mostly supported; see examples.
#'
#' @examples
#' tmp_nc <- create_example_netCDFs(tempdir(), c("xyt", "szt"), "timeseries")
#'
#' ## Read gridded netCDF as array
#' data_xyt <- read_netCDF(
#'   tmp_nc[["xyt"]],
#'   method = "array",
#'   xy_names = c("x", "y")
#' )
#'
#' if (requireNamespace("graphics")) {
#'   graphics::persp(
#'     x = data_xyt[["xyspace"]][["x"]],
#'     y = data_xyt[["xyspace"]][["y"]],
#'     z = data_xyt[["data"]][, , 15],
#'     theta = 30,
#'     phi = 30,
#'     expand = 0.5,
#'     col = "lightblue"
#'   )
#' }
#'
#' ## Read discrete netCDF as array
#' data_szt <- read_netCDF(
#'   tmp_nc[["szt"]],
#'   method = "array",
#'   xy_names = c("x", "y")
#' )
#'
#' if (requireNamespace("graphics")) {
#'   cols <- grDevices::hcl.colors(12, "YlOrRd", rev = TRUE)
#'
#'   plot(
#'     data_szt[["xyspace"]][["x"]],
#'     data_szt[["xyspace"]][["y"]],
#'     col = cols[cut(data_szt[["data"]][, 1, 15], breaks = 12)],
#'     pch = 16
#'   )
#'
#'   graphics::image(
#'     x = data_szt[["site"]],
#'     y = data_szt[["vertical_values"]],
#'     z = data_szt[["data"]][, , 15],
#'     col = cols
#'   )
#' }
#'
#'
#' ## Read netCDF as raster object
#' # This will generate several warnings and messages
#' raster_xyt <- read_netCDF(tmp_nc[["xyt"]], method = "raster", band = 15)
#' raster::plot(raster_xyt)
#'
#' raster_szt <- read_netCDF(tmp_nc[["szt"]], method = "raster", band = 15)
#' raster::plot(raster_szt)
#'
#' ## Read netCDF as stars object
#' stars_xyt <- read_netCDF(tmp_nc[["xyt"]], method = "stars", var = "sine")
#' plot(stars_xyt)
#'
#' stars_szt <- read_netCDF(tmp_nc[["szt"]], method = "stars", var = "sine")
#' plot(stars_szt)
#'
#' ## Read gridded netCDF as array and extract subset
#' datasubset_xyt <- read_netCDF(
#'   tmp_nc[["xyt"]],
#'   method = "xy_subset",
#'   locations = cbind(x = seq(-5, 5), y = 2501316 + seq(-5, 5)),
#'   xy_names = c("x", "y")
#' )
#'
#' ## Read CRS of netCDFs
#' read_crs_from_netCDF(tmp_nc[["xyt"]])
#' read_crs_from_netCDF(tmp_nc[["szt"]])
#'
#' ## Read attributes of netCDFs
#' read_attributes_from_netCDF(tmp_nc[["xyt"]], var = "sine")
#' read_attributes_from_netCDF(
#'   tmp_nc[["xyt"]],
#'   group = "all",
#'   var = "sine",
#'   xy_names = c("x", "y")
#' )
#' read_attributes_from_netCDF(tmp_nc[["szt"]], group = "global")
#'
#' # Clean up
#' unlink(unlist(tmp_nc))
#'
#' @export
read_netCDF <- function(
  x,
  method = c("array", "raster", "stars", "xy_subset"),
  var = NULL,
  nc_name_crs = "crs",
  nc_name_crs_wkt = "crs_wkt",
  locations = NULL,
  ...
) {
  method <- match.arg(method)

  if (method == "xy_subset" && is.null(locations)) {
    stop("`method` = \"xy_subset\" requires `locations` for spatial subsetting")
  }

  dots <- list(...)
  if ("varname" %in% names(dots)) {
    if (is.null(var) || isTRUE(var == dots[["varname"]])) {
      var <- dots[["varname"]]
      dots[["varname"]] <- NULL

    } else {
      stop("Cannot handle both `var` and `varname` and with different values.")
    }
  }

  args <- c(
    list(
      x = x,
      var = var,
      nc_name_crs = nc_name_crs,
      nc_name_crs_wkt = nc_name_crs_wkt
    ),
    dots
  )

  res <- switch(
    EXPR = method,
    raster = do.call(read_netCDF_as_raster, args = args),
    stars = do.call(read_netCDF_as_stars, args = args),
    do.call(read_netCDF_as_array, args = args)
  )

  if (method == "xy_subset") {
    convert_xyspace(
      grid = res[["xyspace"]],
      data = res[["data"]],
      locations = locations,
      locations_crs = if ("locations_crs" %in% names(dots)) {
        dots[["locations_crs"]]
      } else {
        sf::st_crs(locations)
      },
      data_str = res[["data_str"]],
      direction = "collapse"
    )

  } else {
    res
  }
}


#' @rdname read_netCDF
#'
#' @param xy_names A vector with two character strings. The names of the
#'   \var{x} and \var{y} spatial dimensions of the \var{netCDF} file.
#' @param time_ids An integer vector. The index to read a subset of
#'   time steps; a value of \code{-1} means to read all.
#' @param vertical_ids An integer vector. The index to read a subset of
#'   vertical steps; a value of \code{-1} means to read all.
#' @param collapse_degen A logical value. If \code{TRUE}, then degenerate, i.e.,
#'   \code{length-1} vertical and time dimensions in the returned array are
#'   collapsed. A degenerate variable dimension, in the case of a
#'   \code{"xy"} data structure, is always collapsed. The dimension/s of the
#'   \var{xy-space} is/are never collapsed.
#' @param load_values A logical value. If \code{FALSE}, then data values are
#'   not returned
#'   (i.e., element "data" of the returned object will be \code{NULL}).
#'
#' @export
read_netCDF_as_array <- function(
  x,
  var = NULL,
  nc_name_crs = "crs",
  nc_name_crs_wkt = "crs_wkt",
  xy_names = c("lon", "lat"),
  time_ids = -1,
  vertical_ids = -1,
  collapse_degen = TRUE,
  load_values = TRUE,
  ...
) {
  stopifnot(requireNamespace("ncdf4"))

  if (!inherits(x, "ncdf4")) {
    x <- ncdf4::nc_open(filename = x, write = FALSE, readunlim = FALSE)
    on.exit(ncdf4::nc_close(x))
  }


  #--- Variables names
  nc_vars <- nc_vars_all <- names(x[["var"]])
  nc_dims <- names(x[["dim"]])


  #--- Dimensions
  is_gridded <- !("site" %in% nc_dims)

  has_xy <- if (is_gridded) {
    all(xy_names %in% nc_dims)
  } else {
    # Discrete sites/points: x and y are not dimensions but variables
    all(xy_names %in% nc_vars)
  }

  if (!has_xy) {
    stop(
      "Argument `xy_names` (",
      paste0(shQuote(xy_names), collapse = ", "),
      ") does not describe the xy-dimensions of the ",
      if (is_gridded) "gridded " else "discrete ",
      "`netCDF` which has: ",
      paste0(shQuote(nc_dims), collapse = ", ")
    )
  }

  # Clean up variable names
  if (!is_gridded) {
    # Remove x and y variables because they are treated as if dimensions
    nc_vars <- nc_vars[!(nc_vars %in% xy_names)]
  }

  # Exclude variables associated with dimensions and bounds
  tmp <- c(nc_dims, "crs", "climatology_bounds", paste0(nc_dims, "_bnds"))
  nc_vars <- grep(
    paste0("(\\<", tmp, "\\>)", collapse = "|"),
    nc_vars,
    value = TRUE,
    invert = TRUE
  )

  has_vars <- all(var %in% nc_vars)
  if (has_vars) {
    if (length(var) > 0) {
      nc_vars <- nc_vars[nc_vars %in% var]
    }

  } else {
    stop(
      "Argument `var` (",
      paste0(shQuote(var), collapse = ", "),
      ") does not request variables contained in the `netCDF` ",
      "which has: ",
      paste0(shQuote(nc_vars), collapse = ", ")
    )
  }


  #--- Put together the xyspace (and site) object(s)
  tmp <- list(
    # `ncvar_get()` reads xy values whether they are dimensional values
    # (is_gridded) or regular variables (!is_gridded)
    x = ncdf4::ncvar_get(nc = x, varid = xy_names[1]),
    y = ncdf4::ncvar_get(nc = x, varid = xy_names[2])
  )

  # TODO: account for bounds
  # TODO: check/warn if not regular grid
  xyspace <- c(
    tmp,
    list(res = if (is_gridded) sapply(tmp, function(x) unique(diff(x))) else NA)
  )

  sites <- if (!is_gridded) {
    ncdf4::ncvar_get(nc = x, varid = "site")
  }


  #--- Set up time
  has_time <- "time" %in% nc_dims

  if (has_time) {
    nc_time_values <- ncdf4::ncvar_get(nc = x, varid = "time")
    nc_time_N <- length(nc_time_values)

    # time/climatology bounds
    has_clim <- "climatology_bounds" %in% nc_vars_all
    nc_time_bounds <- if (has_clim || "time_bnds" %in% nc_vars_all) {
      t(ncdf4::ncvar_get(
        nc = x,
        varid = if (has_clim) "climatology_bounds" else "time_bnds"
      ))
    }

    nc_type_timeaxis <- if (has_clim) "climatology" else "timeseries"

    # Requested time steps (subset)
    time_ids <- sort(unique(time_ids))
    has_time_subset <- all(time_ids > 0) && length(time_ids) > 0

    if (has_time_subset) {
      if (any(time_ids > nc_time_N)) {
        stop(
          "Not all requested `time_ids` are available; ",
          "available time steps = ", nc_time_N
        )
      }

      has_time_subset <- !isTRUE(identical(time_ids, seq_along(nc_time_values)))
      if (has_time_subset) {
        nc_time_values <- nc_time_values[time_ids]
        nc_time_bounds <- nc_time_bounds[time_ids, , drop = FALSE]
      }
    }

    has_time_collapsed <-
      collapse_degen &&
      (has_time_subset && length(time_ids) == 1 || nc_time_N == 1)

  } else {
    has_time_subset <- FALSE
    has_time_collapsed <- TRUE
    nc_time_values <- nc_time_bounds <- NULL
    nc_type_timeaxis <- NA
  }


  #--- Set up the vertical axis
  has_vertical <- "vertical" %in% nc_dims

  if (has_vertical) {
    nc_vertical_values <- ncdf4::ncvar_get(nc = x, varid = "vertical")
    nc_vertical_N <- length(nc_vertical_values)

    # vertical bounds
    nc_vertical_bounds <- if ("vertical_bnds" %in% nc_vars_all) {
      t(ncdf4::ncvar_get(nc = x, varid = "vertical_bnds"))
    }

    # Requested vertical steps (subset)
    vertical_ids <- sort(unique(vertical_ids))
    has_vertical_subset <- all(vertical_ids > 0) && length(vertical_ids) > 0

    if (has_vertical_subset) {
      if (any(vertical_ids > nc_vertical_N)) {
        stop(
          "Not all requested `vertical_ids` not available; ",
          "available vertical steps = ", nc_vertical_N
        )
      }

      has_vertical_subset <- !isTRUE(
        identical(vertical_ids, seq_along(nc_vertical_values))
      )

      if (has_vertical_subset) {
        nc_vertical_values <- nc_vertical_values[time_ids]
        nc_vertical_bounds <- nc_vertical_bounds[time_ids, , drop = FALSE]
      }
    }

    has_vertical_collapsed <-
      collapse_degen &&
      (
        has_vertical_subset && length(vertical_ids) == 1 ||
        nc_vertical_N == 1
      )

  } else {
    has_vertical_subset <- FALSE
    has_vertical_collapsed <- TRUE
    nc_vertical_values <- nc_vertical_bounds <- NULL
  }



  #--- Data structure
  n_xy <- if (is_gridded) 2 else 1

  data_str <- paste0(
    if (is_gridded) "xy" else "s",
    if (has_vertical && !has_vertical_collapsed) "z",
    if (has_time && !has_time_collapsed) "t"
  )

  nc_data_str <- paste0(
    if (is_gridded) "xy" else "s",
    if (has_vertical) "z",
    if (has_time) "t"
  )


  if (load_values) {
    if (has_vertical_subset) {
      id_vertical_dim <- as.integer(regexpr("z", nc_data_str))
    }

    if (has_time_subset) {
      id_time_dim <- as.integer(regexpr("t", nc_data_str))
    }


    if (has_time_subset || has_vertical_subset) {
      # This requires that all variables have identical xy-space dimensions!
      varid <- nc_vars[1]
      nc_count <- x[["var"]][[varid]][["varsize"]]
      if (has_vertical_subset) nc_count[id_vertical_dim] <- 1
      if (has_time_subset) nc_count[id_time_dim] <- 1

      nc_start <- c(
        rep(1, n_xy),
        if (has_vertical) NA,
        if (has_time) NA
      )
      res_dim <- c(
        nc_count[seq_len(n_xy)],
        if (has_vertical && !has_vertical_collapsed) length(vertical_ids),
        if (has_time && !has_time_collapsed) length(time_ids)
      )
    }


    #--- Read values
    res <- lapply(
      nc_vars,
      function(varid) {
        if (has_time_subset || has_vertical_subset) {
          # Read a subset of values
          tmp <- list()
          tmp_extr <- expand.grid(t = time_ids, v = vertical_ids)

          for (k in seq_len(nrow(tmp_extr))) {
            tmp_start <- nc_start
            if (has_vertical) tmp_start[id_vertical_dim] <- tmp_extr[k, "v"]
            if (has_time) tmp_start[id_time_dim] <- tmp_extr[k, "t"]

            tmp[[k]] <- ncdf4::ncvar_get(
              nc = x,
              varid = varid,
              start = tmp_start,
              count = nc_count,
              collapse_degen = TRUE
            )
          }

          tmp_collapse <-
            has_vertical_collapsed && has_time_collapsed && length(tmp) == 1

          #TODO: check that this works correctly if both time + vertical subset
          tmp_res <- abind::abind(
            tmp,
            along = length(res_dim) + if (tmp_collapse) 0 else 1
          )

        } else {
          # Read all values
          tmp_res <- ncdf4::ncvar_get(
            nc = x,
            varid = varid,
            collapse_degen = FALSE
          )
        }

        # Drop degenerate dimensions, but never xy-space dimensions
        tmp_degen <- which(dim(tmp_res) == 1)
        tmp_degen <- tmp_degen[tmp_degen > n_xy]

        if (collapse_degen && length(tmp_degen) > 0) {
          tmp_res <- abind::adrop(tmp_res, drop = tmp_degen)
        }

        # Check data structure
        if (length(dim(tmp_res)) != nchar(data_str)) {
          warning(
            "Dimensions of data extracted from netCDF (",
            paste0(dim(tmp_res), collapse = ", "),
            ") do not match `data_str` = ", shQuote(data_str)
          )
        }

        tmp_res
      }
    )


    #--- Make sure output structure is as expected/documented
    if (length(nc_vars) > 1) {
      if (data_str == "xy" && collapse_degen) {
        # Combine multiple variables to xy-v or s-v
        res <- abind::abind(res, along = n_xy + 1)

      } else {
        warning(
          "More than one variable and a time and/or vertical dimension: ",
          "returned format of `data` is not standardized!"
        )
      }

    } else {
      res <- res[[1]]
    }

  } else {
    # data values were not requested
    res <- NULL
  }


  #--- Prepare return object
  tmp <- list(
    data = res,
    data_str = data_str,
    type_timeaxis = nc_type_timeaxis,
    crs = read_crs_from_netCDF(
      x,
      nc_name_crs = nc_name_crs,
      nc_name_crs_wkt = nc_name_crs_wkt
    ),
    xyspace = xyspace,
    vertical_values = nc_vertical_values,
    vertical_bounds = nc_vertical_bounds,
    time_values = nc_time_values,
    time_bounds = nc_time_bounds
  )

  tmp <- c(
    tmp,
    read_attributes_from_netCDF(
      x,
      group = "all",
      var = nc_vars,
      xy_names = xy_names,
      nc_name_crs = nc_name_crs
    )
  )

  if (is_gridded) {
    tmp
  } else {
    c(tmp, list(site = sites))
  }
}



#' @rdname read_netCDF
#'
#' @section Details: \code{\link{read_netCDF_as_raster}} is a thin wrapper
#' around \code{\link[raster:raster]{raster::raster}},
#' but makes an extra attempt to correctly set the \var{crs} object.
#'
#' @export
read_netCDF_as_raster <- function(
  x,
  var = NULL,
  nc_name_crs = "crs",
  nc_name_crs_wkt = "crs_wkt",
  ...
) {

  r <- if (is.null(var)) {
    raster::raster(x, ...)
  } else {
    raster::raster(x, varname = var, ...)
  }

  # Check whether projection was read correctly
  r_crs <- raster::crs(r)
  r_has_crs <-
    inherits(r_crs, "CRS") &&
    !is.na(r_crs) &&
    isTRUE(try(
      rgdal::checkCRSArgs_ng(raster::crs(r, asText = TRUE))[[1]]
    ))

  if (!r_has_crs) {
    nc_crs <- read_crs_from_netCDF(
      x,
      nc_name_crs = nc_name_crs,
      nc_name_crs_wkt = nc_name_crs_wkt
    )

    # TODO: update to use WKT2
    # once `raster` internal workflow is updated to use WKT2 instead of PROJ.4
    nc_crs <- raster::crs(nc_crs$Wkt)
    if (
      inherits(nc_crs, "CRS") &&
      !is.na(nc_crs) &&
      isTRUE(try(
        rgdal::checkCRSArgs_ng(raster::crs(nc_crs, asText = TRUE))[[1]]
      ))
    ) {
      raster::crs(r) <- nc_crs
    } else {
      warning("Could not locate a valid crs.")
    }
  }

  r
}


#' @rdname read_netCDF
#'
#' @section Details: \code{\link{read_netCDF_as_stars}} is a thin wrapper
#' around \code{\link[stars:read_ncdf]{stars::read_ncdf}},
#' but makes an extra attempt to correctly set the \var{crs} object.
#'
#' @export
read_netCDF_as_stars <- function(
  x,
  var = NULL,
  nc_name_crs = "crs",
  nc_name_crs_wkt = "crs_wkt",
  ...
) {

  r <- stars::read_ncdf(x, var = var, ...)


  # Check whether projection was read correctly
  r_crs <- try(sf::st_crs(r), silent = TRUE)
  r_has_crs <- inherits(r_crs, "crs") && !is.na(r_crs)

  if (!r_has_crs) {
    sf::st_crs(r) <- read_crs_from_netCDF(
      x,
      nc_name_crs = nc_name_crs,
      nc_name_crs_wkt = nc_name_crs_wkt
    )
  }

  r
}





#' Read \var{crs} projection from a \var{netCDF}
#'
#' @inheritParams read_netCDF
#'
#' @return An object of the \var{crs}-class of package \pkg{sf}.
#'
#' @section Details:
#'   Example code is available in the documentation of
#'   \code{\link{read_netCDF}}.
#'
#' @export
read_crs_from_netCDF <- function(
  x,
  nc_name_crs = "crs",
  nc_name_crs_wkt = "crs_wkt"
) {
  stopifnot(requireNamespace("ncdf4"))

  if (!inherits(x, "ncdf4")) {
    x <- ncdf4::nc_open(filename = x, write = FALSE)
    on.exit(ncdf4::nc_close(x))
  }

  nc_crs <- ncdf4::ncatt_get(
    nc = x,
    varid = nc_name_crs,
    attname = nc_name_crs_wkt
  )[["value"]]

  nc_crs <- try(sf::st_crs(nc_crs), silent = TRUE)
  if (!inherits(nc_crs, "crs") || is.na(nc_crs)) {
    warning("Could not locate a valid crs; returning `NA_crs_`.")
    nc_crs <- sf::NA_crs_
  }

  nc_crs
}




#' Read all attributes of a group from a \var{netCDF}
#'
#' @inheritParams read_netCDF
#' @param group A character string. Specifies which attributes to extract.
#' @param var A character string. The name or names of the variables from
#'   which to extract attributes, used if the "var" \code{group} is requested.
#'
#' @return A named list
#'
#' @section Details:
#'   Example code is available in the documentation of
#'   \code{\link{read_netCDF}}.
#
#' @section Details: \itemize{
#'   \item If attributes of a variable are requested (group is \var{"var"}),
#'         then the variable name must be passed to \code{var}.
#'   \item If \var{xy-space} attributes are requested (group is \var{"xy"}),
#'         then the names of the x and y dimension must be passed to
#'         \code{xy_names}.
#'   \item If \var{crs} attributes are requested (group is \var{"crs"}),
#'         then the name must be passed to \code{nc_name_crs}.
#'   \item To obtain all attributes from each group, pass "all" to \code{group};
#'         \code{var}, \code{xy_names}, \code{nc_name_crs}
#'         must be correctly specified.
#' }
#'
#' @export
read_attributes_from_netCDF <- function(
  x,
  group = c("var", "xy", "crs", "time", "vertical", "global", "all"),
  var = NULL,
  xy_names = c("lon", "lat"),
  nc_name_crs = "crs"
) {
  stopifnot(requireNamespace("ncdf4"))

  group <- match.arg(group)

  if (!inherits(x, "ncdf4")) {
    x <- ncdf4::nc_open(filename = x, write = FALSE)
    on.exit(ncdf4::nc_close(x))
  }

  if (group %in% c("var", "all") && is.null(var)) {
    stop("Variable attributes requested but `var` was not provided.")
  }

  if (group == "all") {
    # Put together attributes from all groups
    tmp_has <- c(names(x[["var"]]), names(x[["dim"]]))
    tmp_req <- c(
      "var",
      "xy",
      if (nc_name_crs %in% tmp_has) nc_name_crs else NA,
      if ("time" %in% tmp_has) "time" else NA,
      if ("vertical" %in% tmp_has) "vertical" else NA,
      "global"
    )
    tmp_nms <- paste0(
      c("var", "xy", "crs", "time", "vertical", "global"),
      "_attributes"
    )

    res <- stats::setNames(
      lapply(
        tmp_req,
        function(att) {
          if (is.na(att)) {
            list()
          } else {
            read_attributes_from_netCDF(
              x,
              group = att,
              var = var,
              xy_names = xy_names,
              nc_name_crs = nc_name_crs
            )
          }
        }
      ),
      tmp_nms
    )

  } else if (group == "xy") {
    # Put together \var{xy-space} attributes
    res <- read_attributes_from_netCDF(x, group = "var", var = xy_names)

  } else if (group == "var" && length(var) > 1) {
    # Put together multiple variable attributes (vectorized)
    tmp_vatts <- lapply(
      var,
      function(var) read_attributes_from_netCDF(x, var = var)
    )

    vatts <- names(tmp_vatts[[1]])
    for (k in seq_along(tmp_vatts)[-1]) {
      vatts <- intersect(vatts, names(tmp_vatts[[k]]))
    }

    res <- stats::setNames(
      lapply(vatts, function(att) sapply(tmp_vatts, function(x) x[[att]])),
      vatts
    )

  } else {
    # Get attributes from any other group
    varid <- switch(
      EXPR = group,
      var = var,
      crs = nc_name_crs,
      global = 0,
      group
    )

    if (
      group != "global" &&
      !(varid %in% c(names(x[["var"]]), names(x[["dim"]])))
    ) {
      stop("Attributes of requested ", shQuote(varid), " cannot be located.")
    }

    res <- ncdf4::ncatt_get(nc = x, varid = varid, attname = NA)

    if (group == "var") {
      res <- c(res, list(name = varid))

    } else if (group == "time") {
      tmp <- x[["unlimdimid"]]

      res <- c(
        res,
        list(unlim = if (tmp > 0) names(x[["dim"]])[tmp] == "time" else FALSE)
      )
    }
  }

  res
}




#' \var{XYZT} or \var{SZT} data dimensions for interacting with \var{netCDFs}
#'
#' @inheritParams rSW2st_netCDF
#' @param dims An integer vector.
#'
#' @return A named list with six elements for
#'   \describe{
#'     \item{ns}{sites, for a spatial representation of data as sites}
#'     \item{nx}{x coordinate, for a gridded spatial representation}
#'     \item{ny}{y coordinate, for a gridded spatial representation}
#'     \item{nz}{vertical levels}
#'     \item{nt}{temporal units}
#'   }
#'
#' @examples
#' x <- array(dim = c(17, 15, 12, 100))
#' get_data_dims("xyzt", dim(x))
#' get_data_dims("xyzt", c(17, 15, 12, 100))
#' get_data_dims("xyt", c(17, 15, 100))
#' get_data_dims("xyz", c(17, 15, 12))
#' get_data_dims("xy", c(17, 15))
#' get_data_dims("xy", c(17, 15, 3))
#'
#' get_data_dims("szt", c(17 * 15, 12, 100))
#' get_data_dims("xy", c(17 * 15))
#' get_data_dims("xy", c(17 * 15, 3))
#'
#' @export
get_data_dims <- function(
  data_str = c("xyzt", "xyt", "xyz", "xy", "szt", "st", "sz", "s"),
  dims = NULL
) {
  data_str <- match.arg(data_str)

  if (is.null(dims)) {
    dims <- rep(0, 4)
  } else {
    dims <- unname(dims)
  }


  switch(
    EXPR = data_str,
    `xyzt` = c(
      ns = 0, nx = dims[1], ny = dims[2], nz = dims[3], nt = dims[4], nv = 0
    ),
    `xyt` = c(ns = 0, nx = dims[1], ny = dims[2], nz = 0, nt = dims[3], nv = 0),
    `xyz` = c(ns = 0, nx = dims[1], ny = dims[2], nz = dims[3], nt = 0, nv = 0),
    `xy` = c(
      ns = 0, nx = dims[1], ny = dims[2], nz = 0, nt = 0,
      nv = if (length(dims) >= 3) dims[3] else 0
    ),

    `szt` = c(
      ns = dims[1], nx = 0, ny = 0, nz = dims[2], nt = dims[3], nv = 0
    ),
    `st` = c(ns = dims[1], nx = 0, ny = 0, nz = 0, nt = dims[2], nv = 0),
    `sz` = c(ns = dims[1], nx = 0, ny = 0, nz = dims[2], nt = 0, nv = 0),
    `s` = c(
      ns = dims[1], nx = 0, ny = 0, nz = 0, nt = 0,
      nv = if (length(dims) >= 2) dims[2] else 0
    ),

    stop("Data structure ", shQuote(data_str), " not implemented.")
  )
}



#' Extract unique \var{x} and \var{y} spanning a gridded \var{xy-space}
#'
#' @inheritParams rSW2st_netCDF
#' @inheritParams rSW2st_crs
#' @param res A numeric vector of length two. The (absolute values of)
#'   cell size/resolution/delta of the \var{x} and \var{y} dimensions
#'   in units of the \var{crs}.
#'
#' @section Notes:
#' The argument \code{crs} is only used if \code{grid} is a \code{data.frame},
#' in which case it is passed to \code{\link{as_points}}.
#' Otherwise, it is ignored and can be missing.
#'
#' @section Notes:
#' The argument \code{res} is only used in two cases: \itemize{
#'   \item if \code{grid} is a list,
#'         such as the one produced by \code{\link{xy_from_grid}}, but does
#'         not contain a named element \var{res}; or
#'   \item if \code{grid} is an object with coordinate values for
#'         all \var{gridcell} centers.
#' }
#' Otherwise, it is ignored and can be missing.
#'
#' @return A list with three elements:
#'  two vectors one each containing all unique values of the
#'  \var{x} coordinate and the \var{y} coordinates of all \var{gricell} centers;
#'  and a vector \var{res} with the (regular) \var{x} and \var{y} resolutions.
#'
#' @examples
#' # grid as raster object
#' r <- raster::raster(
#'   xmn = 0, xmx = 120,
#'   ymn = 0, ymx = 45,
#'   crs = "OGC:CRS84",
#'   resolution = c(1, 1)
#' )
#' xy_from_grid(r)
#'
#' # grid as data frame with coordinate values
#' rdf <- raster::coordinates(r)
#' xy_from_grid(rdf, crs = "OGC:CRS84", res = c(1, 1))
#'
#' # a list with vectors for all x values and all y values (and resolution)
#' rl <- list(
#'   x = sort(unique(rdf[, 1])),
#'   y = sort(unique(rdf[, 2]))
#' )
#' xy_from_grid(c(rl, list(res = c(1, 1))))
#' xy_from_grid(rl, res = c(1, 1))
#'
#' @export
xy_from_grid <- function(grid, crs, res) {
  if (is.character(grid)) {
    # e.g., a filename to a raster
    grid <- try(raster::raster(grid), silent = TRUE)
  }


  if (inherits(grid, "Raster")) {
    xy_grid <- list(
      x = raster::xFromCol(grid, seq_len(raster::ncol(grid))),
      y = raster::yFromRow(grid, rev(seq_len(raster::nrow(grid)))),
      res = raster::res(grid)
    )

  } else if (inherits(grid, "stars")) {
    tmp_res <- abs(unname(
      sapply(stars::st_dimensions(grid), function(x) x[["delta"]])
    ))[1:2]

    if (anyNA(tmp_res)) {
      stop("Can currently only handle regular, rectangular grids.")
    }

    xy_grid <- list(
      x = sort(stars::st_get_dimension_values(grid, which = 1, center = TRUE)),
      y = sort(stars::st_get_dimension_values(grid, which = 2, center = TRUE)),
      res = tmp_res
    )

  } else {
    tmp <- try(
      as_points(grid, to_class = "sf", crs = crs),
      silent = TRUE
    )

    if (!inherits(tmp, "try-error")) {
      tmp <- sf::st_coordinates(tmp)[, 1:2, drop = FALSE]
      xy_grid <- list(
        x = sort(unique(tmp[, 1])),
        y = sort(unique(tmp[, 2])),
        res = res
      )

    } else {
      xy_grid <- list(
        x = sort(unique(grid[[1]])),
        y = sort(unique(grid[[2]])),
        res = if ("res" %in% names(grid)) grid[["res"]] else res
      )
    }
  }

  xy_grid
}




#' Expand/collapse between separate and combined \var{x} and \var{y} dimensions
#'
#' @inheritParams rSW2st_netCDF
#' @param direction A character string. The direction of the operation.
#'
#' @section Details:
#' The expanding direction expects that the first dimension of \code{data}
#' matches the first dimension of \code{locations}.
#' In this case, \code{locations} and \code{locations_crs} will be passed to
#' \code{\link{as_points}}.
#'
#' @section Notes:
#' The use of \code{data_str} currently refers to the "expanded" state
#' unlike how the argument is used by other functions. This may change
#' in future versions of this function.
#'
#' @return A copy of \code{data} with one dimension added/removed.
#'
#' @examples
#' tmp_nc <- create_example_netCDFs(tempdir(), "xyt", "timeseries")
#' data_xyt <- read_netCDF(tmp_nc[["xyt"]], "array", xy_names = c("x", "y"))
#'
#' # Collapse x-y-t into xy-t format
#' res_collapsed <- convert_xyspace(
#'   grid = data_xyt[["xyspace"]],
#'   data = data_xyt[["data"]],
#'   locations = expand.grid(data_xyt[["xyspace"]][c("x", "y")]),
#'   data_str = "xyt",
#'   direction = "collapse"
#' )
#'
#' # Expand xy-t into x-y-t format
#' res_expanded <- convert_xyspace(
#'   grid = data_xyt[["xyspace"]],
#'   data = res_collapsed,
#'   locations = expand.grid(data_xyt[["xyspace"]][c("x", "y")]),
#'   data_str = "xyt",
#'   direction = "expand"
#' )
#'
#' # Round trip (using all grid locations) recovers data (but not names)
#' all.equal(data_xyt[["data"]], res_expanded, check.attributes = FALSE)
#'
#' # Clean up
#' unlink(unlist(tmp_nc))
#'
#' @export
convert_xyspace <- function(
  grid,
  data,
  locations,
  locations_crs = sf::st_crs(locations),
  data_str = c("xyzt", "xyt", "xyz", "xy"),
  direction = c("expand", "collapse")
) {
  data_str <- match.arg(data_str)
  direction <- match.arg(direction)

  if ((is.vector(data) || is.null(dim(data)))) {
    if (data_str == "xy") {
      # Convert vector into matrix
      data <- matrix(data, ncol = 1, dimnames = list(NULL, names(data)))

    } else {
      stop("`data` is a vector or similar object, but `data_str` is not 'xy'.")
    }
  }

  data_dims <- dim(data)


  #--- xy-coordinates of grid
  xy_grid <- xy_from_grid(grid, crs = locations_crs)
  n_cells <- prod(lengths(xy_grid[1:2]))


  #--- xy-coordinates of data
  locations <- as_points(locations, to_class = "sf", crs = locations_crs)
  n_loc <- nrow(locations)
  xy_data <- sf::st_coordinates(locations)[, 1:2, drop = FALSE]

  # Check if locations are outside grid
  ids_outside <-
    xy_data[, 1] < (min(xy_grid[[1]]) - xy_grid[["res"]][1] / 2) |
    xy_data[, 1] > (max(xy_grid[[1]]) + xy_grid[["res"]][1] / 2) |
    xy_data[, 2] < (min(xy_grid[[2]]) - xy_grid[["res"]][2] / 2) |
    xy_data[, 2] > (max(xy_grid[[2]]) + xy_grid[["res"]][2] / 2)


  #--- Map locations (xy_data) to gridcells (xy_grid)
  # i.e, identify the gridcell x-rows/y-columns for each location
  ids_x <- sapply(xy_data[, 1], function(x) which.min(abs(xy_grid[[1]] - x)))
  ids_y <- sapply(xy_data[, 2], function(x) which.min(abs(xy_grid[[2]] - x)))

  if (any(ids_outside)) {
    warning(
      "`locations` fall outside the `grid`: n = ", sum(ids_outside),
      "; they will return NA."
    )

    ids_x[ids_outside] <- NA
    ids_y[ids_outside] <- NA
  }

  if (anyDuplicated(cbind(ids_x, ids_y)) > 0) {
    warning(
      "`locations` identify non-unique cells on the `grid`."
    )
  }


  if (direction == "expand") {
    #------ Expand one xy-dimension into separate x- and y-dimensions
    if (data_dims[1] > n_cells) {
      stop(
        "`nrow(data)` must be smaller or equal to ",
        "the number of cells in `grid`."
      )
    }

    if (data_dims[1] != n_loc) {
      stop(
        "The number of locations must match `nrow(data)`."
      )
    }


    #--- Expand data
    tmp_dn <- strsplit(data_str, split = "")[[1]]
    if (length(tmp_dn) < length(data_dims) && data_str == "xy") {
      tmp_dn <- c(tmp_dn, "v")
    }

    res <- array(
      dim = unname(c(lengths(xy_grid)[1:2], data_dims[-1])),
      dimnames = sapply(tmp_dn, function(x) NULL)
    )

    if (data_str %in% c("xyt", "xyz", "xy")) {
      ids_tzv <- rep(seq_len(data_dims[2]), each = n_loc)
      res[cbind(ids_x, ids_y, ids_tzv)] <- as.matrix(data)

    } else if (data_str %in% "xyzt") {
      ids_t <- rep(seq_len(data_dims[2]), each = n_loc)
      ids_z <- rep(seq_len(data_dims[3]), each = prod(data_dims[1:2]))
      res[cbind(ids_x, ids_y, ids_t, ids_z)] <- data

    } else {
      stop(
        "No implementation for `data` to expand space; ",
        "`data` has dimensions: ", paste(data_dims, collapse = ", "),
        " and structure ", shQuote(data_str)
      )
    }


  } else if (direction == "collapse") {
    #------ Collapse/extract x- and y-dimensions into one xy-dimension
    res <- NULL

    if (length(data_dims) == 2) {
      if (data_str == "xy") {
        res <- data[cbind(ids_x, ids_y)]
        attr(res, "dim") <- c(n_loc, 1L)
      }

    } else if (length(data_dims) == 3) {
      if (data_str %in% c("xyt", "xyz", "xy")) {
        ids_tzv <- rep(seq_len(data_dims[3]), each = n_loc)
        res <- data[cbind(ids_x, ids_y, ids_tzv)]
        attr(res, "dim") <- c(n_loc, data_dims[3])
      }

    } else if (length(data_dims) == 4) {
      if (data_str %in% "xyzt") {
        ids_t <- rep(seq_len(data_dims[3]), each = n_loc)
        ids_z <- rep(seq_len(data_dims[4]), each = n_loc * data_dims[3])
        res <- data[cbind(ids_x, ids_y, ids_t, ids_z)]
        attr(res, "dim") <- c(n_loc, data_dims[3:4])
      }
    }

    if (is.null(res)) {
      stop(
        "No implementation for `data` to collapse space; ",
        "`data` has dimensions: ", paste(data_dims, collapse = ", "),
        " and structure ", shQuote(data_str)
      )
    }

  }

  res
}




#' Create example \var{netCDFs} for use by package
#'
#' This function creates different \var{netCDFs} for all requested conditions:
#' \itemize{
#'   \item \code{data_str}
#'   \item \code{type_timeaxis}
#' }
#'
#' @param path A character string. The path to where the files will be
#'   written to disk.
#' @inheritParams rSW2st_netCDF
#' @inheritParams create_netCDF
#'
#' @return A named list, invisibly, of full paths to the created files on disk.
#'
#' @examples
#' tmp_nc <- create_example_netCDFs(tempdir(), "xyt", "timeseries")
#' data_xyt <- read_netCDF(
#'   tmp_nc[["xyt"]],
#'   method = "array",
#'   xy_names = c("x", "y")
#' )
#'
#' if (requireNamespace("graphics")) {
#'   graphics::persp(
#'     x = data_xyt[["xyspace"]][["x"]],
#'     y = data_xyt[["xyspace"]][["y"]],
#'     z = data_xyt[["data"]][, , 15],
#'     theta = 30,
#'     phi = 30,
#'     expand = 0.5,
#'     col = "lightblue"
#'   )
#' }
#'
#' unlink(unlist(tmp_nc))
#'
#' @export
create_example_netCDFs <- function(
  path,
  data_str = c("xyzt", "xyt", "xyz", "xy", "szt", "st", "sz", "s"),
  type_timeaxis = c("timeseries", "climatology"),
  overwrite = FALSE
) {

  # TODO:
  # nv = c(0/1, many if data_str in c("xy", "s"))
  # nz, nt = c(1, many)
  # if nz, nt == 1, then {
  #   i) non-dropped dim in data
  #   ii) dropped or non-dropped dim (with one value) in netCDF
  # }


  #--- Determine requested netCDF conditions
  req_data_str <- match.arg(data_str, several.ok = TRUE)

  req_spatial <- unlist(lapply(
    c("xy", "s"),
    function(x) if (any(grepl(paste0("\\<", x), req_data_str))) x
  ))

  req_nonspatial <- unlist(lapply(
    c("zt", "t", "z"),
    function(x) if (any(grepl(paste0(x, "\\>"), req_data_str))) x
  ))

  if (any(c("xy", "s") %in% req_data_str)) {
    req_nonspatial <- c(req_nonspatial, "v")
  }

  req_type_timeaxis <- match.arg(type_timeaxis, several.ok = TRUE)


  #--- Attributes
  nc_att_global <- list(
    title = "Example netCDF of package rSW2st",
    version = paste0("v", format(Sys.Date(), "%Y%m%d")),
    source_id = "SOILWAT2",
    further_info_url = "https://github.com/DrylandEcology/",
    source_type = "LAND",
    realm = "land",
    product = "model-output",
    grid = "native Alberts projection grid with NAD83 datum",
    grid_label = "gn",
    nominal_resolution = "1 m"
  )


  #nolint start
  # USA Contiguous Albers Equal Area Conic USGS version
  # +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs
  # http://cfconventions.org/Data/cf-conventions/cf-conventions-1.8/cf-conventions.html#_albers_equal_area
  #nolint end
  nc_att_crs <- list(
    crs_wkt = sf::st_crs("EPSG:6350")$Wkt,
    grid_mapping_name = "albers_conical_equal_area",
    standard_parallel = c(29.5, 45.5),
    longitude_of_central_meridian = -96.0,
    latitude_of_projection_origin = 23.0,
    false_easting = 0.0,
    false_northing = 0.0,
    # GRS 1980 ellipsoid
    longitude_of_prime_meridian = 0,
    semi_major_axis = 6378137.0,
    inverse_flattening = 298.257222101
  )

  nc_att_xy <- list(
    name = c("x", "y"),
    standard_name = c("projection_x_coordinate", "projection_y_coordinate"),
    long_name = c("x coordinate of projection", "y coordinate of projection"),
    units = c("m", "m")
  )



  #--- Create raster
  orig <- sf::st_coordinates(
    sf::st_transform(
      rSW2st::as_points(
        c(
          nc_att_crs[["longitude_of_central_meridian"]],
          nc_att_crs[["standard_parallel"]][2]
        ),
        crs = "OGC:CRS84",
        to_class = "sf"
      ),
      nc_att_crs[["crs_wkt"]]
    )
  )[1, ]

  dxy <- c(20, 30)
  nxy <- 2 * dxy + 1

  x <- seq(-dxy[1], dxy[1], length = nxy[1])
  y <- seq(-dxy[2], dxy[2], length = nxy[2])



  raster_xy <- suppressWarnings(raster::raster(
    xmn = orig[1] + min(x) - 0.5,
    xmx = orig[1] + max(x) + 0.5,
    ymn = orig[2] + min(y) - 0.5,
    ymx = orig[2] + max(y) + 0.5,
    crs = nc_att_crs[["crs_wkt"]],
    resolution = c(1, 1)
  ))


  #--- Create sites
  sites_ids <- cbind(x = seq_along(x), y = diff(dxy) + seq_along(x))
  sites_xy <- cbind(x = x[sites_ids[, "x"]], y = y[sites_ids[, "y"]])
  ns <- nrow(sites_ids)


  #--- Create time
  nt <- 20
  time_values <- seq_len(nt)
  time_bounds <- cbind(time_values - 1, time_values)
  time_attributes <- list(
    units = "days since 1900-01-01",
    calendar = "standard",
    unlim = FALSE
  )


  #--- Create depth
  nz <- 5
  vertical_values <- seq_len(nz) - 1
  vertical_bounds <- cbind(vertical_values, vertical_values + 1)
  vertical_attributes <- list(
    units = "m",
    positive = "down"
  )




  #--- Example data ------
  # Code based off code example of `graphics::persp()`
  data_xyzt <- array(dim = c(nxy, nz, nt))

  var_attributes <- list(
    name = "sine",
    long_name = "Sine Wave",
    units = "1",
    grid_mapping = "crs: x y"
  )

  var_comment <- list(
    zt = paste0(
      "Value represents {",
      "r <- sqrt(x ^ 2 + y ^ 2 + z ^ 3); sin(r * t) / r",
      "}"
    ),
    t = paste0(
      "Value represents {",
      "r <- sqrt(x ^ 2 + y ^ 2); sin(r * t) / r",
      "}"
    ),
    z = paste0(
      "Value represents {",
      "r <- sqrt(x ^ 2 + y ^ 2 + z ^ 3); sin(r) / r",
      "}"
    ),
    v = paste0(
      "Value represents {",
      "r <- sqrt(x ^ 2 + y ^ 2); sin(r) / r",
      "}"
    )
  )


  f <- function(x, y, z, t = 1) {
    r <- sqrt(x ^ 2 + y ^ 2 + z ^ 3)
    sin(r * t) / r
  }

  for (k1 in seq_len(nt)) {
    for (k2 in seq_len(nz))
      data_xyzt[, , k2, k1] <- outer(
        x, y,
        FUN = f,
        z = vertical_values[k2],
        t = k1 / (0.75 * nt)
      )
  }


  if (FALSE) {
    graphics::persp(
      x, y,
      data_xyzt[, , 1, 15],
      theta = 30,
      phi = 30,
      expand = 0.5,
      col = "lightblue"
    )
  }


  #------ Loop over gridded/sites ------
  list_fname_nc <- list()

  for (k1_spatial in req_spatial) {

    xyspace <- switch(EXPR = k1_spatial, xy = raster_xy, s = sites_xy)

    #------ Loop over vertical/time axes ------
    for (k2_datastr in req_nonspatial) {

      tmp_data_str <- paste0(
        k1_spatial,
        switch(EXPR = k2_datastr, v = "", k2_datastr)
      )

      if (!(tmp_data_str %in% req_data_str)) next

      #------ Loop over timeaxis type ------
      used_req_type_timeaxis <- if (k2_datastr %in% c("zt", "t")) {
        req_type_timeaxis
      } else {
        "fx"
      }

      for (k3_Taxis in used_req_type_timeaxis) {

        tag <- paste0(
          tmp_data_str,
          if (k3_Taxis == "climatology") "-clim"
        )

        list_fname_nc[[tag]] <- file.path(
          path,
          paste0("nc_", tag, ".nc")
        )

        if (!file.exists(list_fname_nc[[tag]]) || overwrite) {

          tmp_data <- if (k1_spatial == "xy") {
            switch(
              EXPR = k2_datastr,
              zt = data_xyzt,
              t = data_xyzt[, , 1, ],
              z = data_xyzt[, , , 15],
              v = data_xyzt[, , 1, 15]
            )

          } else if (k1_spatial == "s") {
            ids <- switch(
              EXPR = k2_datastr,
              zt = cbind(
                rep(sites_ids[, "x"], times = nz * nt),
                rep(sites_ids[, "y"], times = nz * nt),
                z = rep(rep(seq_len(nz), each = ns), times = nz),
                t = rep(seq_len(nt), each = ns * nz)
              ),
              t = cbind(
                rep(sites_ids[, "x"], times = nt),
                rep(sites_ids[, "y"], times = nt),
                z = 1,
                t = rep(seq_len(nt), each = ns)
              ),
              z = cbind(
                rep(sites_ids[, "x"], times = nz),
                rep(sites_ids[, "y"], times = nz),
                z = rep(seq_len(nz), each = ns),
                t = 15
              ),
              v = cbind(sites_ids, z = 1, t = 15)
            )

            data_dims <- switch(
              EXPR = k2_datastr,
              zt = c(ns, nz, nt),
              t = c(ns, nt),
              z = c(ns, nz),
              v = ns
            )

            array(data_xyzt[ids], dim = data_dims)
          }

          create_netCDF(
            filename = list_fname_nc[[tag]],
            xyspace = xyspace,
            data = tmp_data,
            data_str = tmp_data_str,
            data_type = "float",
            var_attributes = c(
              var_attributes,
              comment = var_comment[[k2_datastr]]
            ),

            xy_attributes = nc_att_xy,
            crs_attributes = nc_att_crs,
            check_crs = FALSE,

            time_values = if (k2_datastr %in% c("zt", "t")) time_values,
            type_timeaxis = if (k3_Taxis == "fx") "timeseries" else k3_Taxis,
            time_attributes = time_attributes,
            time_bounds = time_bounds,

            vertical_values = if (k2_datastr %in% c("zt", "z")) vertical_values,
            vertical_attributes = vertical_attributes,
            vertical_bounds = vertical_bounds,

            global_attributes = nc_att_global,
            nc_compression = TRUE,
            nc_deflate = 5,
            overwrite = overwrite
          )
        }
      }
    }
  }

  invisible(list_fname_nc)
}
