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
#'    \item a list, such as the one produced by \code{\link{get_xyspace}}.
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
#' @param xy_names A vector with two character strings. The names of the
#'   \var{x} and \var{y} spatial dimensions of a \var{netCDF} file.
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
#'   If data are gridded, then passed to \code{\link{get_xyspace}};
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
# nolint start: line_length_linter.
#'     \href{http://cfconventions.org/Data/cf-conventions/cf-conventions-1.8/cf-conventions.html#point-data}{CF point-data}.
# nolint end
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
# nolint start: line_length_linter.
#' \href{https://github.com/PCMDI/cmip6-cmor-tables/tree/master/Tables}{CMIP6-cmor-tables}
# nolint end
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
# nolint start: line_length_linter.
#' @references \href{https://cfconventions.org/Data/cf-conventions/cf-conventions-1.8/cf-conventions.html}{CF conventions}
# nolint end
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
#' tmp_nc[["xyt2"]] <- sub(".nc$", "2.nc", tmp_nc[["xyt"]])
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
#' tmp_nc[["szt2"]] <- sub(".nc$", "2.nc", tmp_nc[["szt"]])
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
    long_name = "WGS84",
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
  time_bounds = matrix(NA, nrow = length(time_values), ncol = 2L),
  vertical_values = NULL,
  vertical_attributes = list(
    units = "",
    positive = "down"
  ),
  vertical_bounds = matrix(NA, nrow = length(vertical_values), ncol = 2L),
  global_attributes = list(title = "Title"),
  overwrite = FALSE,
  nc_compression = FALSE,
  nc_shuffle = TRUE,
  nc_deflate = 5L,
  nc_chunks = "by_zt",
  verbose = FALSE
) {
  #------ 1) Checks/preparations -----------------------------------------------
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
    if (!overwrite) {
      warning(
        "File ", shQuote(basename(filename)),
        " exists and 'overwrite' is FALSE; returning early.",
        call. = FALSE
      )
      return(invisible(FALSE))
    }

    unlink(filename)
  }


  #------ data characterization ------
  has_data <- !missing(data) && !is.null(data)

  data_type <- match.arg(data_type)

  # Three data structure situations:
  #   i) one variable and vertical axis and time ("xyzt", "szt")
  #   ii) one variable and time OR vertical axis ("xyt", "xyz", "st", "sz")
  #   iii) one/multiple variables and no time/vertical axis ("xy", "s")
  data_str <- match.arg(data_str)
  is_gridded <- startsWith(data_str, "xy")


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
    if (
      has_data &&
        !isTRUE(all.equal(
          data_dims_from_data,
          data_dims[names(data_dims_from_data)]
        ))
    ) {
      stop(
        "Disagreement in dimensions between `data_dims` and `data`.",
        call. = FALSE
      )
    }
  }


  # Check argument data dimensions
  stopifnot(
    c("ns", "nx", "ny", "nt", "nz", "nv") %in% names(data_dims),
    !anyNA(data_dims),
    data_dims[["ns"]] > 0L || data_dims[["nx"]] > 0L && data_dims[["ny"]] > 0L
  )

  # nolint start: consecutive_assertion_linter.
  # Check that data structure is possible given data dimensions
  tmp <- switch(
    EXPR = data_str,

    xyzt = stopifnot(
      data_dims[["nt"]] > 0L,
      data_dims[["nz"]] > 0L,
      data_dims[["nv"]] == 0L
    ),

    xyt = stopifnot(
      data_dims[["nt"]] > 0L,
      data_dims[["nz"]] == 0L,
      data_dims[["nv"]] == 0L
    ),

    xyz = stopifnot(
      data_dims[["nt"]] == 0L,
      data_dims[["nz"]] > 0L,
      data_dims[["nv"]] == 0L
    ),

    xy = stopifnot(
      data_dims[["nt"]] == 0L,
      data_dims[["nz"]] == 0L,
      data_dims[["nv"]] >= 0L
    )
  )
  # nolint end



  #------ xy-space ------
  if (is.null(xyspace) || missing(xyspace)) {
    stop("Must provide `xyspace` as argument.", call. = FALSE)
  }


  #--- xy attributes
  if (any(lengths(xy_attributes) != 2L)) {
    stop("All `xy_attributes` must be of lenght two (xy-space).", call. = FALSE)
  }

  tmp_xy_atts <- c("name", "standard_name", "long_name", "units")
  if (!all(tmp_xy_atts %in% names(xy_attributes))) {
    stop(
      "`xy_attributes` must include ",
      toString(shQuote(tmp_xy_atts)),
      call. = FALSE
    )
  }

  if ("axis" %in% names(xy_attributes)) {
    if (any(c("X", "Y") != toupper(xy_attributes[["axis"]]))) {
      stop(
        "`xy_attributes`: if `axis` is included, then its value must be ",
        "`X` and `Y`.",
        call. = FALSE
      )
    }
    xy_attributes[["axis"]] <- NULL
  }

  #--- Discrete sites
  nameSites <- "site"

  #--- Spatial bounds
  nameBndsDim <- "bnds"
  nameBndVarsXY <- paste0(xy_attributes[["name"]][1L:2L], "_", nameBndsDim)

  if ("bounds" %in% names(xy_attributes)) {
    if (any(nameBndVarsXY != xy_attributes[["bounds"]])) {
      stop(
        "`xy_attributes`: ",
        "if `bounds` is included, then its value must be ",
        shQuote(nameBndVarsXY[[1L]]), " and ", shQuote(nameBndVarsXY[[2L]]),
        call. = FALSE
      )
    }
    xy_attributes[["bounds"]] <- NULL
  }


  #--- crs attributes setup & info
  nameCRS <- "crs"

  if ("crs_wkt" %in% names(crs_attributes)) {
    crs_wkt_user <- crs_attributes[["crs_wkt"]]
    crs_attributes[["crs_wkt"]] <- NULL

    # check that CRS is valid
    crs_used <- try(sf::st_crs(crs_wkt_user), silent = TRUE)
    if (!inherits(crs_used, "crs") || crs_used == sf::NA_crs_) {
      stop(
        "`crs_attributes[[\"crs_wkt\"]]` does not represent a valid CRS.",
        call. = FALSE
      )
    }

  } else {
    stop("Need `crs_attributes[[\"crs_wkt\"]]`", call. = FALSE)
  }

  if (!("grid_mapping_name" %in% names(crs_attributes))) {
    stop("Need `crs_attributes[[\"grid_mapping_name\"]]`", call. = FALSE)
  }


  # `xyspace` is allowed to be an object without a crs
  crs_xyspace <- try(suppressWarnings(sf::st_crs(xyspace)), silent = TRUE)
  if (!inherits(crs_xyspace, "crs") || crs_xyspace == sf::NA_crs_) {
    crs_xyspace <- crs_used

    if (verbose) {
      message(
        "No `crs` could be extracted from `xyspace`; assuming that it is ",
        "`crs_attributes[[\"crs_wkt\"]]`."
      )
    }
  }


  # check that CRS definition matches CRS of xyspace (grid or locations)
  if (crs_xyspace != crs_used) {
    msg <- paste0(
      "The CRS given in `crs_attributes[[\"crs_wkt\"]]` needs to ",
      "match the CRS of the `xyspace` object. Currently, ",
      "`crs_attributes[[\"crs_wkt\"]]` is ", shQuote(crs_used$Wkt),
      " and the CRS of `xyspace` is ", shQuote(crs_xyspace$Wkt)
    )

    if (check_crs) stop(msg, call. = FALSE)

    if (verbose) warning(msg, call. = FALSE)
  }


  if (is_gridded) {
    # Note: xvals are organized from west to east, yvals from south to north
    xy_grid <- try(
      get_xyspace(xyspace, crs = crs_xyspace),
      silent = TRUE
    )

    if (inherits(xy_grid, "try-error")) {
      stop(
        "Argument `xyspace` could not be interpreted as grid.",
        call. = FALSE
      )
    }

    # xy values
    xvals <- xy_grid[[1L]]
    yvals <- xy_grid[[2L]]
    n_xvals <- length(xvals)
    n_yvals <- length(yvals)

    if (data_dims[["nx"]] > n_xvals || data_dims[["ny"]] > n_yvals) {
      stop(
        "For gridded data, `data_dims[\"nx\"]` and `data_dims[\"ny\"]` ",
        "must be smaller or equal to the number of unique x or, respectively, ",
        "y coordinate values in `xyspace` for gridded data.",
        call. = FALSE
      )
    }

    # Grid resolution/bounds
    grid_res <- lapply(xy_grid, function(x) unique(diff(x)))
    check_res <- vapply(
      grid_res,
      function(x) diff(range(x)),
      FUN.VALUE = NA_real_
    )

    if (any(check_res > sqrt(.Machine[["double.eps"]]))) {
      stop(
        "Coordinate intervals of `xyspace` are not constant ",
        "as is required for a grid.",
        call. = FALSE
      )
    }

    # Calculate x and y bounds
    grid_halfres <- c(grid_res[[1L]][[1L]], grid_res[[2L]][[1L]]) / 2
    x_bounds <- rbind(xvals - grid_halfres[[1L]], xvals + grid_halfres[[1L]])
    y_bounds <- rbind(yvals - grid_halfres[[2L]], yvals + grid_halfres[[2L]])

  } else {
    locs <- try(
      as_points(xyspace, to_class = "sf", crs = crs_used),
      silent = TRUE
    )

    if (inherits(locs, "try-error")) {
      stop(
        "Argument `xyspace` could not be interpreted as location.",
        call. = FALSE
      )
    }

    n_sites <- nrow(locs)
    tmp_coord <- sf::st_coordinates(locs) # coordinates of point locations
    xvals <- tmp_coord[, 1L, drop = TRUE]
    yvals <- tmp_coord[, 2L, drop = TRUE]

    if (data_dims[["ns"]] > n_sites) {
      stop(
        "`data_dims[\"ns\"]` must be smaller or equal to ",
        "the number of sites in `xyspace` for discrete data sites or points.",
        call. = FALSE
      )
    }
  }



  #------ time axis ------
  nameTime <- "time"

  type_timeaxis <- match.arg(type_timeaxis)

  n_time <- length(time_values)

  has_T_timeAxis <- if (data_dims[["nt"]] > 0L) {
    "explicit"
  } else if (n_time > 0L) {
    "implicit"
  } else {
    "none"
  }

  if (has_T_timeAxis %in% c("explicit", "implicit")) {
    #--- Check time values/dimension match
    if (has_T_timeAxis == "explicit" && data_dims[["nt"]] != n_time) {
      stop(
        "`data_dims[\"nt\"]` must match ",
        "the number of elements in `time_values`.",
        call. = FALSE
      )
    }

    if (has_T_timeAxis == "implicit" && n_time != 1) {
      stop(
        "If `data_dims[\"nt\"]` is zero, ",
        "then `time_values` can only have one value.",
        call. = FALSE
      )
    }

    #--- Check and conform time_bounds
    if (all(is.na(time_bounds))) {
      time_bounds <- NULL
    }

    if (!is.null(time_bounds)) {
      if (length(dim(time_bounds)) == 0L) {
        if (n_time * 2L != length(time_bounds)) {
          stop(
            "Start and end required for each `time_values` ",
            "to define `time_bounds`",
            call. = FALSE
          )
        }

        time_bounds <- matrix(
          time_bounds, nrow = n_time, ncol = 2L, byrow = TRUE
        )

      } else if (!identical(dim(time_bounds), c(as.integer(n_time), 2L))) {
        stop(
          "Start and end required for each `time_values` ",
          "to define `time_bounds`",
          call. = FALSE
        )
      }
    }

    #--- Identify type of time axis
    if (type_timeaxis == "timeseries") {
      varid_timebnds <- "time_bnds"
      att_timebnds <- "bounds"

    } else if (type_timeaxis == "climatology") {
      # nolint start: line_length_linter.
      # http://cfconventions.org/Data/cf-conventions/cf-conventions-1.8/cf-conventions.html#climatological-statistics
      # nolint end
      varid_timebnds <- "climatology_bounds" # not climatology_bnds!
      att_timebnds <- "climatology"
    }

    #--- Check time attributes
    if (!"units" %in% names(time_attributes)) {
      stop("Need units attribute in time attribute list", call. = FALSE)
    }
    time_units <- time_attributes[["units"]]
    time_attributes[["units"]] <- NULL


    if (!"calendar" %in% names(time_attributes)) {
      stop("Need calendar attribute in time attribute list", call. = FALSE)
    }
    time_cal <- time_attributes[["calendar"]]
    time_attributes[["calendar"]] <- NULL


    if (!"unlim" %in% names(time_attributes)) {
      stop("Need unlim attribute in time attribute list", call. = FALSE)
    }
    time_unlim <- as.logical(time_attributes[["unlim"]])
    time_attributes[["unlim"]] <- NULL

    if ("axis" %in% names(time_attributes)) {
      if ("T" != toupper(time_attributes[["axis"]])) {
        stop(
          "`time_attributes`: ",
          "if `axis` is included, then its value must be `T`",
          call. = FALSE
        )
      }
      time_attributes[["axis"]] <- NULL
    }

    if (att_timebnds %in% names(time_attributes)) {
      if (varid_timebnds != time_attributes[[att_timebnds]]) {
        stop(
          "`time_attributes`: ",
          "if ", shQuote(att_timebnds), " is included, then its value must be ",
          shQuote(varid_timebnds),
          call. = FALSE
        )
      }
      time_attributes[[att_timebnds]] <- NULL
    }

    not_att_timebnds <- switch(
      EXPR = type_timeaxis,
      timeseries = "climatology",
      climatology = "bounds"
    )

    if (not_att_timebnds %in% names(time_attributes)) {
      warning(
        "`time_attributes`: ",
        "the attribute ", shQuote(not_att_timebnds), " is ignored ",
        "because time represents a ", shQuote(type_timeaxis),
        "; instead, the automatically generated attribute ",
        shQuote(att_timebnds),
        " encodes the bounds of the time axis.",
        call. = FALSE
      )
      time_attributes[[not_att_timebnds]] <- NULL
    }
  }


  #------ vertical axis ------
  nameVertical <- "vertical"

  n_vertical <- length(vertical_values)

  has_Z_verticalAxis <- if (data_dims[["nz"]] > 0L) {
    "explicit"
  } else if (n_vertical > 0L) {
    "implicit"
  } else {
    "none"
  }

  if (has_Z_verticalAxis %in% c("explicit", "implicit")) {

    #--- Check time values/dimension match
    if (has_Z_verticalAxis == "explicit" && data_dims[["nz"]] != n_vertical) {
      stop(
        "`data_dims[\"nz\"]` must match ",
        "the number of elements in `vertical_values`.",
        call. = FALSE
      )
    }

    if (has_Z_verticalAxis == "implicit" && n_vertical != 1L) {
      stop(
        "If `data_dims[\"nz\"]` is zero, ",
        "then `vertical_values` can only have one value.",
        call. = FALSE
      )
    }

    #--- Check and conform vertical_bounds
    if (all(is.na(vertical_bounds))) {
      vertical_bounds <- NULL
    }

    if (!is.null(vertical_bounds)) {
      if (length(dim(vertical_bounds)) == 0L) {
        if (n_vertical * 2L != length(vertical_bounds)) {
          stop(
            "Start and end values required for each `vertical_values` ",
            "to define `vertical_bounds`",
            call. = FALSE
          )
        }

        vertical_bounds <- matrix(
          vertical_bounds,
          nrow = n_vertical,
          ncol = 2L,
          byrow = TRUE
        )

      } else if (
        !identical(dim(vertical_bounds), c(as.integer(n_vertical), 2L))
      ) {
        stop(
          "Start and end values required for each `vertical_values` ",
          "to define `vertical_bounds`",
          call. = FALSE
        )
      }
    }


    #--- Check vertical attributes
    if (!"units" %in% names(vertical_attributes)) {
      stop("Need `units` attribute in vertical attribute list", call. = FALSE)
    }
    vert_units <- vertical_attributes[["units"]]
    vertical_attributes[["units"]] <- NULL

    if (!("positive" %in% names(vertical_attributes))) {
      stop(
        "Need `positive` attribute in vertical attribute list",
        call. = FALSE
      )
    }

    if ("axis" %in% names(vertical_attributes)) {
      if ("Z" != toupper(vertical_attributes[["axis"]])) {
        stop(
          "`vertical_attributes`: ",
          "if `axis` is included, then its value must be `Z`",
          call. = FALSE
        )
      }
      vertical_attributes[["axis"]] <- NULL
    }

    if ("bounds" %in% names(vertical_attributes)) {
      if ("vertical_bnds" != vertical_attributes[["bounds"]]) {
        stop(
          "`vertical_attributes`: ",
          "if `bounds` is included, then its value must be `vertical_bnds`",
          call. = FALSE
        )
      }
      vertical_attributes[["bounds"]] <- NULL
    }
  }


  #------ Variables ------
  n_vars <- max(1L, data_dims[["nv"]]) # at least one implicit variable

  if (any(lengths(var_attributes) != n_vars)) {
    stop(
      "All variable attributes need a value for each variable.",
      call. = FALSE
    )
  }

  if (!"name" %in% names(var_attributes)) {
    stop("Need a `name` variable attribute.", call. = FALSE)
  }
  var_names <- var_attributes[["name"]]
  var_attributes[["name"]] <- NULL

  if (!"long_name" %in% names(var_attributes)) {
    var_attributes[["long_name"]] <- var_names
  }

  if (!"units" %in% names(var_attributes)) {
    stop("Need unit attribute in variable attribute list", call. = FALSE)
  }
  var_units <- var_attributes[["units"]]
  var_attributes[["units"]] <- NULL

  if ("grid_mapping" %in% names(var_attributes)) {
    if (!startsWith(var_attributes[["grid_mapping"]], nameCRS)) {
      warning(
        "Variable attribute for `grid_mapping` should be 'crs: ...', but is ",
        shQuote(var_attributes[["grid_mapping"]]),
        call. = FALSE
      )
    }

  } else {
    # This function creates only one grid_mapping and
    # the grid_mapping variable name is hard-coded to be "crs"
    var_attributes[["grid_mapping"]] <- paste(
      paste0(nameCRS, ":"),
      # here, guessing axis order to be 1, 2
      # (that would be correct for OGC:CRS84, but not for EPSG:4326)
      xy_attributes[["name"]][[1L]],
      xy_attributes[["name"]][[2L]]
    )

    if (verbose) {
      message(
        "Adding `grid_mapping = \"", var_attributes[["grid_mapping"]], "\"`",
        " to variable attributes."
      )
    }
  }

  if (!is_gridded) {
    # nolint start: line_length_linter.
    # http://cfconventions.org/Data/cf-conventions/cf-conventions-1.8/cf-conventions.html#point-data
    # nolint end
    if (!("coordinates" %in% names(var_attributes))) {
      tmp <- paste(xy_attributes[["name"]][[2L]], xy_attributes[["name"]][[1L]])
      if (has_T_timeAxis != "none") tmp <- paste(tmp, nameTime)
      if (has_Z_verticalAxis != "none") tmp <- paste(tmp, nameVertical)
      var_attributes[["coordinates"]] <- tmp

      if (verbose) {
        message("Adding `coordinates = \"", tmp, "\"` to variable attributes.")
      }
    }

    if (!("featureType" %in% names(global_attributes))) {
      global_attributes[["featureType"]] <- if (has_T_timeAxis == "none") {
        "point"
      } else {
        "timeSeries"
      }

      if (verbose) {
        message("Adding `featureType` to global attributes.")
      }
    }
  }

  if ("_FillValue" %in% names(var_attributes)) {
    if (verbose) {
      warning(
        "`_FillValue` variable attribute is automatically generated.",
        call. = FALSE
      )
    }
    var_attributes[["_FillValue"]] <- NULL
  }

  if ("missing_value" %in% names(var_attributes)) {
    if (verbose) {
      warning(
        "`missing_value` variable attribute is replaced by ",
        "automatically generated `_FillValue`.",
        call. = FALSE
      )
    }
    var_attributes[["missing_value"]] <- NULL
  }


  #--- Create netCDF file ------
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)

  xnc <- RNetCDF::create.nc(filename = filename, format = "netcdf4")
  on.exit(RNetCDF::close.nc(xnc))


  #--- ..* Global attributes ------

  if (!is.null(global_attributes)) {
    ns_att_glob <- names(global_attributes)

    tmp <- c("Conventions", "creation_date")
    if (has_T_timeAxis == "none") {
      tmp <- c(tmp, "time_label", "time_title")
    }
    has_replaced_gatts <- tmp[tmp %in% ns_att_glob]
    if (length(has_replaced_gatts) > 0) {
      warning(
        "`global_attributes` contained values for ",
        toString(shQuote(has_replaced_gatts)),
        "; they were replaced with an automatically generated value.",
        call. = FALSE
      )
      ns_att_glob <- setdiff(ns_att_glob, has_replaced_gatts)
      global_attributes <- global_attributes[ns_att_glob]
    }
  }


  setGlobalAttributesNCSW(
    xnc,
    attributes = c(
      Conventions = "CF-1.8",
      creation_date = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
      global_attributes
    )
  )


  #------ 2) netCDF axes ------------------------------------------

  #--- ..* spatial dimension ------
  if (is_gridded) {
    setAxisNCSW(
      xnc,
      nameAxis = xy_attributes[["name"]][[1L]],
      dataType = "NC_DOUBLE",
      values = xvals,
      axis = "X",
      long_name = xy_attributes[["long_name"]][[1L]],
      units = xy_attributes[["units"]][[1L]],
      attributes = c(standard_name = xy_attributes[["standard_name"]][[1L]])
    )

    setAxisNCSW(
      xnc,
      nameAxis = xy_attributes[["name"]][[2L]],
      dataType = "NC_DOUBLE",
      values = yvals,
      axis = "Y",
      long_name = xy_attributes[["long_name"]][[2L]],
      units = xy_attributes[["units"]][[2L]],
      attributes = c(standard_name = xy_attributes[["standard_name"]][[2L]])
    )

    setSpatialBoundsNCSW(
      xnc,
      nameBndsDim = nameBndsDim,
      nameBndsX = nameBndVarsXY[[1L]],
      nameBndsY = nameBndVarsXY[[2L]],
      nameDimX = xy_attributes[["name"]][[1L]],
      nameDimY = xy_attributes[["name"]][[2L]],
      valuesBndsX = x_bounds,
      valuesBndsY = y_bounds
    )

    var_dims <- c(xy_attributes[["name"]][[1L]], xy_attributes[["name"]][[2L]])
    var_chunksizes <- if (has_chunks) c(n_xvals, n_yvals) else NA
    var_start <- c(1L, 1L)

  } else {
    setAxisSiteNCSW(
      xnc,
      siteValues = seq_len(n_sites),
      nameAxis = nameSites,
      units = "1",
      dataType = "NC_INT"
    )

    var_dims <- nameSites
    var_chunksizes <- if (has_chunks) n_sites else NA
    var_start <- 1L
  }

  #--- ..* vertical dimension ------
  if (has_Z_verticalAxis != "none") {
    verticalType <- if (identical(vert_units, "1")) "layers" else "values"

    setAxisVerticalNCSW(
      xnc,
      verticalValues = vertical_values,
      verticalUpperBound = vertical_bounds[, 1L, drop = TRUE],
      verticalLowerBound = vertical_bounds[, 2L, drop = TRUE],
      verticalType = verticalType,
      nameAxis = nameVertical,
      units = vert_units,
      dataType = "NC_INT"
    )

    var_dims <- c(var_dims, nameVertical)

    if (has_chunks && has_predet_chunks) {
      var_chunksizes <- c(
        var_chunksizes,
        if (nc_chunks == "by_zt") 1L else n_vertical
      )
    }
    var_start <- c(var_start, 1L)
  }

  #--- ..* time dimension ------
  if (has_T_timeAxis != "none") {
    setAxisTimeNCSW(
      xnc,
      timeUnits = time_units,
      timeValues = time_values,
      nameAxis = nameTime,
      calendar = time_cal,
      dataType = "NC_DOUBLE",
      isUnlimitedDim = time_unlim
    )

    if (!is.null(time_bounds)) {
      setAxisBoundsNCSW(
        xnc,
        nameBndsVar = varid_timebnds,
        nameDim = nameTime,
        valuesBnds = t(time_bounds),
        nameBndsDim = nameBndsDim
      )

      RNetCDF::att.put.nc(
        xnc,
        variable = nameTime,
        name = att_timebnds,
        type = "NC_CHAR",
        value = varid_timebnds
      )
    }

    var_dims <- c(var_dims, nameTime)

    if (has_chunks && has_predet_chunks) {
      var_chunksizes <- c(
        var_chunksizes,
        if (nc_chunks %in% c("by_zt", "by_t")) 1L else n_time
      )
    }
    var_start <- c(var_start, 1L)
  }


  #------ 3) netCDF variables -------------------------------------------
  if (has_chunks && !has_predet_chunks) {
    stopifnot(length(nc_chunks) == length(var_dims))
    var_chunksizes <- nc_chunks
  }


  #--- ..* data variables ------
  for (k in seq_len(n_vars)) {
    setVariableNCSW(
      xnc,
      varName = var_names[[k]],
      values = NULL,
      var_chunksizes_xyzt = if (has_chunks) var_chunksizes else NA,
      dataType = data_type,
      dimensions = var_dims,
      deflate = nc_deflate,
      long_name = var_attributes[["long_name"]],
      units = var_units[[k]],
      cell_method = NULL,
      coordinates = var_attributes[["coordinates"]],
      grid_mapping = var_attributes[["grid_mapping"]],
      attributes = lapply(
        var_attributes,
        function(att) if (length(att) == 1L) att else att[[k]]
      ),
      addFillValue = TRUE
    )
  }


  #--- ..* x and y as variables if not gridded ------
  if (!is_gridded) {
    setVariableNCSW(
      xnc,
      varName = xy_attributes[["name"]][[1L]],
      values = xvals,
      dataType = "NC_DOUBLE",
      dimensions = nameSites,
      deflate = nc_deflate,
      long_name = xy_attributes[["long_name"]][[1L]],
      units = xy_attributes[["units"]][[1L]],
      cell_method = NULL,
      coordinates = NULL,
      grid_mapping = NULL,
      attributes = c(
        standard_name = xy_attributes[["standard_name"]][[1L]],
        axis = "X"
      ),
      addFillValue = FALSE
    )

    setVariableNCSW(
      xnc,
      varName = xy_attributes[["name"]][[2L]],
      values = yvals,
      dataType = "NC_DOUBLE",
      dimensions = nameSites,
      deflate = nc_deflate,
      long_name = xy_attributes[["long_name"]][[2L]],
      units = xy_attributes[["units"]][[2L]],
      cell_method = NULL,
      coordinates = NULL,
      grid_mapping = NULL,
      attributes = c(
        standard_name = xy_attributes[["standard_name"]][[2L]],
        axis = "Y"
      ),
      addFillValue = FALSE
    )
  }


  #--- ..* CRS ------
  setCRSNCSW(
    xnc,
    nameCRS = nameCRS,
    grid_mapping_name = crs_attributes[["grid_mapping_name"]],
    crs_wkt = crs_wkt_user,
    attributes = crs_attributes
  )



  #------ 4) Add values (if provided) ------------------------------------------
  if (has_data) {
    .populate_netCDF_nocheck(
      xnc,
      data = data,
      var_names = var_names,
      data_str = data_str,
      is_gridded = is_gridded
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
  ox <- openRnetCDF(x, write = TRUE)
  xnc <- ox[["con"]]
  if (ox[["closeOnExit"]]) on.exit(RNetCDF::close.nc(xnc))

  n_vars <- length(var_names)

  if ((data_str %in% c("xy", "s")) && n_vars > 1L) {
    for (k in seq_len(n_vars)) {
      if (is_gridded) {
        values <- data[, , k]
        var_start <- c(1L, 1L)
        var_count <- c(-1L, -1L)

      } else {
        values <- data[, k]
        var_start <- 1L
        var_count <- -1L
      }

      RNetCDF::var.put.nc(
        xnc,
        variable = var_names[[k]],
        data = values,
        start = var_start,
        count = var_count
      )
    }

  } else {
    RNetCDF::var.put.nc(xnc, variable = var_names[[1L]], data = data)
  }
}


#' Add data to an existing \var{netCDF} file
#'
#' @inheritParams create_netCDF
#' @inheritParams read_netCDF_as_array
#' @param var_names A character vector of strings. The \var{netCDF} variable
#'   names.
#'
#' @section Notes: This function is not yet implemented.
#'
#' @export
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
  populate_netCDF_dev(
    filename = filename,
    data = data,
    var_names = var_names,
    data_str = data_str,
    nc_name_crs = nc_name_crs,
    nc_name_crs_wkt = nc_name_crs_wkt,
    xy_names = xy_names,
    time_ids = time_ids,
    vertical_ids = vertical_ids,
    ...
  )
}


# TODO: write code for populate_netCDF
populate_netCDF_dev <- function(
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
  ox <- openRnetCDF(filename, write = TRUE)
  xnc <- ox[["con"]]
  if (ox[["closeOnExit"]]) on.exit(RNetCDF::close.nc(xnc))


  if (length(var_names) == 1L && data_str == "xyt") {
    # check time_ids and match with data
    RNetCDF::var.put.nc(
      xnc,
      variable = var_names[[1L]],
      data = data,
      start = c(1L, 1L, time_ids),
      count = c(-1L, -1L, 1L)
    )
  }

  warning("unfinished code.", call. = FALSE)
}


#' Read a \var{netCDF}
#'
#' Read a \var{netCDF} as produced by \code{create_netCDF}
#'
#' @inheritParams rSW2st_netCDF
#' @param x An object identifying a \var{netCDF} file, i.e.,
#'   a character string as file name, an object of class \var{"NetCDF"},
#'   or an object of class \var{"ncdf4"}.
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
#' @param verbose_read A logical value. If \code{FALSE}, then an attempt is made
#'   to silence communication generated from reading the \var{netCDF}.
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
#' if (requireNamespace("raster")) {
#'   # This will generate several warnings and messages
#'   raster_xyt <- read_netCDF(
#'     tmp_nc[["xyt"]],
#'     method = "raster",
#'     band = 15,
#'     verbose_read = FALSE
#'   )
#'   raster::plot(raster_xyt)
#'
#'   raster_szt <- read_netCDF(
#'     tmp_nc[["szt"]],
#'     method = "raster",
#'     band = 15,
#'     verbose_read = FALSE
#'   )
#'   raster::plot(raster_szt)
#' }
#'
#' ## Read netCDF as stars object
#' stars_xyt <- read_netCDF(
#'   tmp_nc[["xyt"]],
#'   method = "stars",
#'   var = "sine",
#'   verbose_read = FALSE
#' )
#' plot(stars_xyt)
#'
#' # Hack that stars::read_ncdf() is not treating it as time series (see notes)
#' setGlobalAttributesNCSW(
#'   tmp_nc[["szt"]],
#'   attributes = c(featureType = "featureType", frequency = "timeseries-wog")
#' )
#' stars_szt <- read_netCDF(
#'   tmp_nc[["szt"]],
#'   method = "stars",
#'   var = "sine",
#'   verbose_read = FALSE
#' )
#' plot(stars_szt)
#'
#' ## Read netCDF as terra object
#' terra_xyt <- read_netCDF(
#'   tmp_nc[["xyt"]],
#'   method = "terra",
#'   var = "sine",
#'   verbose_read = FALSE
#' )
#' terra::plot(terra_xyt)
#'
#' terra_szt <- read_netCDF(
#'   tmp_nc[["szt"]],
#'   method = "terra",
#'   var = "sine",
#'   verbose_read = FALSE
#' )
#' terra::plot(terra_szt)
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
#' unlink("Rplots.pdf")
#'
#' @export
read_netCDF <- function(
  x,
  method = c("array", "raster", "stars", "terra", "xy_subset"),
  var = NULL,
  nc_name_crs = "crs",
  nc_name_crs_wkt = "crs_wkt",
  locations = NULL,
  verbose_read = TRUE,
  ...
) {
  method <- match.arg(method)

  if (method == "xy_subset" && is.null(locations)) {
    stop(
      "`method` = \"xy_subset\" requires `locations` for spatial subsetting",
      call. = FALSE
    )
  }

  dots <- list(...)
  if ("varname" %in% names(dots)) {
    if (!is.null(var) && !isTRUE(var == dots[["varname"]])) {
      stop(
        "Cannot handle both `var` and `varname` and with different values.",
        call. = FALSE
      )
    }

    var <- dots[["varname"]]
    dots[["varname"]] <- NULL
  }

  listArgs <- c(
    list(
      x = x,
      var = var,
      nc_name_crs = nc_name_crs,
      nc_name_crs_wkt = nc_name_crs_wkt,
      verbose_read = verbose_read
    ),
    dots
  )

  res <- switch(
    EXPR = method,
    raster = do.call(read_netCDF_as_raster, args = listArgs),
    stars = do.call(read_netCDF_as_stars, args = listArgs),
    terra = do.call(read_netCDF_as_terra, args = listArgs),
    do.call(read_netCDF_as_array, args = listArgs)
  )

  if (method == "xy_subset") {
    convert_xyspace(
      grid = res[["xyspace"]],
      data = res[["data"]],
      locations = locations,
      locations_crs = if ("locations_crs" %in% names(dots)) {
        dots[["locations_crs"]]
      } else {
        tmp_crs <- try(sf::st_crs(locations), silent = TRUE)
        if (inherits(tmp_crs, "crs")) {
          tmp_crs
        } else {
          res[["crs"]]
        }
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
#' @param time_name A character string. The dimension name corresponding
#'   to the time axis.
#' @param time_ids An integer vector. The index to read a subset of
#'   time steps; a value of \code{-1} means to read all.
#' @param vertical_name A character string. The dimension name corresponding
#'   to the vertical axis.
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
#' @param meta A named list. Description of the content of the \var{netCDF}
#' file. For internal purposes.
#'
#' @export
read_netCDF_as_array <- function(
  x,
  var = NULL,
  nc_name_crs = "crs",
  nc_name_crs_wkt = "crs_wkt",
  xy_names = c("lon", "lat"),
  time_name = "time",
  time_ids = -1,
  vertical_name = "vertical",
  vertical_ids = -1,
  collapse_degen = TRUE,
  load_values = TRUE,
  meta = NULL,
  ...
) {
  ox <- openRnetCDF(x, write = FALSE)
  xnc <- ox[["con"]]
  if (ox[["closeOnExit"]]) on.exit(RNetCDF::close.nc(xnc))

  if (is.null(meta)) {
    meta <- get_meta_netCDF(xnc)
  }

  #--- Variables names
  nc_vars <- nc_vars_all <- setdiff(meta[["vars"]], meta[["dims"]])
  nc_dims <- meta[["dims"]]


  #--- Dimensions
  is_gridded <- is_netCDF_gridded(xnc, xy_names = xy_names)

  has_xy <- if (is_gridded) {
    all(xy_names %in% nc_dims)
  } else {
    # Discrete sites/points: x and y are not dimensions but variables
    all(xy_names %in% nc_vars)
  }

  if (!has_xy) {
    stop(
      "Argument `xy_names` (",
      toString(shQuote(xy_names)),
      ") does not describe the xy-dimensions of the ",
      if (is_gridded) "gridded " else "discrete ",
      "`netCDF` which has: ",
      toString(shQuote(nc_dims)),
      call. = FALSE
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
      toString(shQuote(var)),
      ") does not request variables contained in the `netCDF` ",
      "which has: ",
      toString(shQuote(nc_vars)),
      call. = FALSE
    )
  }


  #--- Put together the xyspace (and site) object(s)
  xyspace <- get_xyspace(xnc, xy_names = xy_names)

  sites <- if (!is_gridded) {
    RNetCDF::var.get.nc(xnc, variable = "site", unpack = TRUE)
  }


  #--- Set up time
  has_time <- time_name %in% nc_dims

  if (has_time) {
    nc_time_values <- RNetCDF::var.get.nc(
      xnc, variable = time_name, unpack = TRUE
    )
    nc_time_N <- length(nc_time_values)

    # time/climatology bounds
    has_clim <- "climatology_bounds" %in% nc_vars_all
    time_bnds_name <- paste0(time_name, "_bnds")
    nc_time_bounds <- if (has_clim || time_bnds_name %in% nc_vars_all) {
      t(
        RNetCDF::var.get.nc(
          xnc,
          variable = if (has_clim) "climatology_bounds" else time_bnds_name,
          unpack = TRUE
        )
      )
    }

    nc_type_timeaxis <- if (has_clim) "climatology" else "timeseries"

    # Requested time steps (subset)
    time_ids <- sort(unique(time_ids))
    has_time_subset <- all(time_ids > 0) && length(time_ids) > 0

    if (has_time_subset) {
      if (any(time_ids > nc_time_N)) {
        stop(
          "Not all requested `time_ids` are available; ",
          "available time steps = ", nc_time_N,
          call. = FALSE
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
  has_vertical <- vertical_name %in% nc_dims

  if (has_vertical) {
    nc_vertical_values <- RNetCDF::var.get.nc(
      xnc, variable = vertical_name, unpack = TRUE
    )
    nc_vertical_N <- length(nc_vertical_values)

    # vertical bounds
    vertical_bnds_name <- paste0(vertical_name, "_bnds")
    nc_vertical_bounds <- if (vertical_bnds_name %in% nc_vars_all) {
      t(RNetCDF::var.get.nc(xnc, variable = vertical_bnds_name, unpack = TRUE))
    }

    # Requested vertical steps (subset)
    vertical_ids <- sort(unique(vertical_ids))
    has_vertical_subset <- all(vertical_ids > 0) && length(vertical_ids) > 0

    if (has_vertical_subset) {
      if (any(vertical_ids > nc_vertical_N)) {
        stop(
          "Not all requested `vertical_ids` not available; ",
          "available vertical steps = ", nc_vertical_N,
          call. = FALSE
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
  n_xy <- if (is_gridded) 2L else 1L

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
      id_vertical_dim <- as.integer(regexpr("z", nc_data_str, fixed = TRUE))
    }

    if (has_time_subset) {
      id_time_dim <- as.integer(regexpr("t", nc_data_str, fixed = TRUE))
    }


    if (has_time_subset || has_vertical_subset) {
      # This requires that all variables have identical xy-space dimensions!
      varid <- nc_vars[[1L]]
      nc_count <- x[["var"]][[varid]][["varsize"]]
      if (has_vertical_subset) nc_count[id_vertical_dim] <- 1
      if (has_time_subset) nc_count[id_time_dim] <- 1

      nc_start <- c(
        rep(1L, n_xy),
        if (has_vertical) NA_integer_,
        if (has_time) NA_integer_
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

            tmp[[k]] <- RNetCDF::var.get.nc(
              xnc,
              variable = varid,
              start = tmp_start,
              count = nc_count,
              unpack = TRUE,
              collapse = TRUE
            )
          }

          tmp_collapse <-
            has_vertical_collapsed && has_time_collapsed && length(tmp) == 1L

          # TODO: check that this works correctly if both time + vertical subset
          tmp_res <- abind::abind(
            tmp,
            along = length(res_dim) + if (tmp_collapse) 0L else 1L
          )

        } else {
          # Read all values
          tmp_res <- RNetCDF::var.get.nc(
            xnc, variable = varid, unpack = TRUE, collapse = FALSE
          )
        }

        # Drop degenerate dimensions, but never xy-space dimensions
        tmp_degen <- which(dim(tmp_res) == 1L)
        tmp_degen <- tmp_degen[tmp_degen > n_xy]

        if (collapse_degen && length(tmp_degen) > 0) {
          tmp_res <- abind::adrop(tmp_res, drop = tmp_degen)
        }

        # Check data structure
        if (length(dim(tmp_res)) != nchar(data_str)) {
          warning(
            "Dimensions of data extracted from netCDF (",
            toString(dim(tmp_res)),
            ") do not match `data_str` = ", shQuote(data_str),
            call. = FALSE
          )
        }

        tmp_res
      }
    )


    #--- Make sure output structure is as expected/documented
    if (length(nc_vars) > 1) {
      if (data_str == "xy" && collapse_degen) {
        # Combine multiple variables to xy-v or s-v
        res <- abind::abind(res, along = n_xy + 1L)

      } else {
        warning(
          "More than one variable and a time and/or vertical dimension: ",
          "returned format of `data` is not standardized!",
          call. = FALSE
        )
      }

    } else {
      res <- res[[1L]]
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
      nc_name_crs = nc_name_crs,
      meta = meta
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
  verbose_read = TRUE,
  ...
) {
  stopifnot(
    requireNamespace("raster"),
    # `raster::raster()` uses "ncdf4" but it is a suggested package
    requireNamespace("ncdf4", quietly = TRUE)
  )

  e <- expression(
    if (is.null(var)) {
      raster::raster(x, ...)
    } else {
      raster::raster(x, varname = var, ...)
    }
  )

  r <- if (verbose_read) {
    eval(e)
  } else {
    # silence `print()`, see issue #9
    utils::capture.output(
      res <- suppressMessages( # nolint: implicit_assignment_linter.
        suppressWarnings(
          eval(e)
        )
      )
    )
    res
  }


  # Check whether projection was read correctly
  r_crs <- raster::crs(r)
  r_has_crs <-
    inherits(r_crs, "CRS") &&
    !is.na(r_crs) &&
    isTRUE(try(inherits(sf::st_crs(r_crs)), "crs"))

  if (!r_has_crs) {
    nc_crs <- read_crs_from_netCDF(
      x,
      nc_name_crs = nc_name_crs,
      nc_name_crs_wkt = nc_name_crs_wkt
    )

    # TODO: update to use WKT2
    # once `raster` internal workflow is updated to use WKT2 instead of PROJ.4
    nc_crs <- raster::crs(nc_crs$Wkt)
    tmp_crs <- sf::st_crs(nc_crs)
    if (
      !is.na(tmp_crs) &&
        isTRUE(try(inherits(tmp_crs, "crs"), silent = TRUE))
    ) {
      raster::crs(r) <- nc_crs
    } else {
      warning("Could not locate a valid crs: ", nc_crs, call. = FALSE)
    }
  }

  r
}


#' @rdname read_netCDF
#'
#' @section Details: [read_netCDF_as_stars()] is a thin wrapper
#' around [stars::read_ncdf()], but makes an extra attempt to correctly set
#' the `crs` object.
#'
#' @section Notes: [read_netCDF_as_stars()] via [stars::read_ncdf()] uses
#' [ncdfgeom::read_timeseries_dsg()] to read data
#' if there is a global attribute `featureType = "timeseries"`; however, this
#' fails if there are no `"geometry"` data.
#' See examples for a work-around.
#'
#' @md
#' @export
read_netCDF_as_stars <- function(
  x,
  var = NULL,
  nc_name_crs = "crs",
  nc_name_crs_wkt = "crs_wkt",
  verbose_read = TRUE,
  ...
) {

  stopifnot(
    # `stars::read_ncdf()` uses "ncmeta" but it is a suggested package
    requireNamespace("ncmeta", quietly = TRUE),
    # `stars::read_ncdf()` uses "ncdfgeom" (if `featureType = "timeseries"`),
    # but it is a suggested package
    requireNamespace("ncdfgeom", quietly = TRUE)
  )

  e <- expression(
    stars::read_ncdf(x, var = var, ...)
  )

  r <- if (verbose_read) {
    eval(e)
  } else {
    suppressMessages(
      suppressWarnings(
        eval(e)
      )
    )
  }


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



#' @rdname read_netCDF
#'
#' @section Details: \code{\link{read_netCDF_as_terra}} is a thin wrapper
#' around \code{\link[terra:rast]{terra::rast}},
#' but makes an extra attempt to correctly set the \var{crs} object.
#'
#' @export
read_netCDF_as_terra <- function(
  x,
  var = NULL,
  nc_name_crs = "crs",
  nc_name_crs_wkt = "crs_wkt",
  verbose_read = TRUE,
  ...
) {

  e <- expression(
    terra::rast(x, drivers = "NETCDF")
  )

  r <- if (verbose_read) {
    eval(e)
  } else {
    suppressMessages(
      suppressWarnings(
        eval(e)
      )
    )
  }


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
  ox <- openRnetCDF(x, write = FALSE)
  xnc <- ox[["con"]]
  if (ox[["closeOnExit"]]) on.exit(RNetCDF::close.nc(xnc))

  hasVarCRS <- try(
    RNetCDF::var.inq.nc(xnc, variable = nc_name_crs),
    silent = TRUE
  )

  nc_crs <- if (!inherits(hasVarCRS, "try-error")) {
    res <- try(
      RNetCDF::att.get.nc(
        xnc, variable = nc_name_crs, attribute = nc_name_crs_wkt
      ),
      silent = TRUE
    )

    if (!inherits(res, "try-error")) res
  }

  nc_crs <- try(sf::st_crs(nc_crs), silent = TRUE)
  if (!inherits(nc_crs, "crs") || is.na(nc_crs)) {
    warning("Could not locate a valid crs; returning `NA_crs_`.", call. = FALSE)
    nc_crs <- sf::NA_crs_
  }

  nc_crs
}


get_meta_netCDF <- function(x) {
  ox <- openRnetCDF(x, write = FALSE)
  xnc <- ox[["con"]]
  if (ox[["closeOnExit"]]) on.exit(RNetCDF::close.nc(xnc))

  meta <- RNetCDF::file.inq.nc(xnc)

  meta[["vars"]] <- vapply(
    seq_len(meta[["nvars"]]),
    function(k) RNetCDF::var.inq.nc(xnc, variable = k - 1L)[["name"]],
    FUN.VALUE = NA_character_
  )

  meta[["dims"]] <- vapply(
    seq_len(meta[["ndims"]]),
    function(k) RNetCDF::dim.inq.nc(xnc, dimension = k - 1L)[["name"]],
    FUN.VALUE = NA_character_
  )

  meta
}


#' Read all attributes of a group from a \var{netCDF}
#'
#' @inheritParams read_netCDF
#' @inheritParams read_netCDF_as_array
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
  group = c("var", "xy", "crs", time_name, vertical_name, "global", "all"),
  var = NULL,
  xy_names = c("lon", "lat"),
  time_name = "time",
  vertical_name = "vertical",
  nc_name_crs = "crs",
  meta = NULL
) {
  group <- match.arg(group)

  ox <- openRnetCDF(x, write = FALSE)
  xnc <- ox[["con"]]
  if (ox[["closeOnExit"]]) on.exit(RNetCDF::close.nc(xnc))


  if (group %in% c("var", "all") && is.null(var)) {
    stop(
      "Variable attributes requested but `var` was not provided.",
      call. = FALSE
    )
  }

  if (is.null(meta)) {
    meta <- get_meta_netCDF(xnc)
  }

  if (group == "all") {
    # Put together attributes from all groups
    tmp_has <- unique(c(meta[["vars"]], meta[["dims"]]))
    tmp_req <- c(
      "var",
      "xy",
      if (nc_name_crs %in% tmp_has) nc_name_crs else NA,
      if (time_name %in% tmp_has) time_name else NA,
      if (vertical_name %in% tmp_has) vertical_name else NA,
      "global"
    )
    tmp_nms <- paste0(
      c("var", "xy", "crs", time_name, vertical_name, "global"),
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
              xnc,
              group = att,
              var = var,
              xy_names = xy_names,
              nc_name_crs = nc_name_crs,
              meta = meta
            )
          }
        }
      ),
      tmp_nms
    )

  } else if (group == "xy") {
    # Put together \var{xy-space} attributes
    res <- read_attributes_from_netCDF(
      xnc, group = "var", var = xy_names, meta = meta
    )

  } else if (group == "var" && length(var) > 1) {
    # Put together multiple variable attributes (vectorized)
    tmp_vatts <- lapply(
      var,
      function(var) read_attributes_from_netCDF(xnc, var = var, meta = meta)
    )

    vatts <- names(tmp_vatts[[1L]])
    for (k in seq_along(tmp_vatts)[-1]) {
      vatts <- intersect(vatts, names(tmp_vatts[[k]]))
    }

    res <- stats::setNames(
      lapply(
        vatts,
        function(att) unlist(lapply(tmp_vatts, function(x) x[[att]]))
      ),
      vatts
    )

  } else {
    # Get attributes from any other group
    varid <- switch(
      EXPR = group,
      var = var,
      crs = nc_name_crs,
      global = "NC_GLOBAL",
      group
    )

    if (
      group != "global" && !(varid %in% c(meta[["vars"]], meta[["dims"]]))
    ) {
      stop(
        "Attributes of requested ", shQuote(varid), " cannot be located.",
        call. = FALSE
      )
    }

    tmp <- if (identical(varid, "NC_GLOBAL")) {
      lapply(
        seq_len(meta[["ngatts"]]),
        function(k) {
          list(
            RNetCDF::att.inq.nc(
              xnc, variable = "NC_GLOBAL", attribute = k - 1L
            )[["name"]],
            RNetCDF::att.get.nc(xnc, variable = "NC_GLOBAL", attribute = k - 1L)
          )
        }
      )
    } else {
      natts <- RNetCDF::var.inq.nc(xnc, variable = varid)[["natts"]]
      lapply(
        seq_len(natts),
        function(k) {
          list(
            RNetCDF::att.inq.nc(
              xnc, variable = varid, attribute = k - 1L
            )[["name"]],
            RNetCDF::att.get.nc(xnc, variable = varid, attribute = k - 1L)
          )
        }
      )
    }

    res <- stats::setNames(
      lapply(tmp, FUN = function(x) x[[2L]]),
      nm = vapply(tmp, FUN = function(x) x[[1L]], FUN.VALUE = NA_character_)
    )

    if (group == "var") {
      res <- c(res, list(name = varid))

    } else if (group == time_name) {
      tmp <- meta[["unlimdimid"]]

      res <- c(
        res,
        list(
          unlim = if (isTRUE(is.na(tmp))) {
            FALSE
          } else {
            meta[["dims"]][[tmp]] == time_name
          }
        )
      )
    }
  }

  res
}


#' Determine if a \var{netCDF} has a gridded or discrete \var{xy-space}
#'
#' @inheritParams read_netCDF
#'
#' @return A logical value
#'
#' @export
is_netCDF_gridded <- function(
  x,
  xy_names = c("lon", "lat")
) {
  ox <- openRnetCDF(x, write = FALSE)
  xnc <- ox[["con"]]
  if (ox[["closeOnExit"]]) on.exit(RNetCDF::close.nc(xnc))

  hasDimSite <- try(RNetCDF::dim.inq.nc(xnc, "site"), silent = TRUE)
  hasDimX <- try(RNetCDF::dim.inq.nc(xnc, xy_names[[1L]]), silent = TRUE)
  hasDimY <- try(RNetCDF::dim.inq.nc(xnc, xy_names[[2L]]), silent = TRUE)
  hasVarX <- try(RNetCDF::var.inq.nc(xnc, xy_names[[1L]]), silent = TRUE)
  hasVarY <- try(RNetCDF::var.inq.nc(xnc, xy_names[[2L]]), silent = TRUE)

  is_gridded <- c(
    isTRUE(inherits(hasDimSite, "try-error")),
    isTRUE(!inherits(hasDimX, "try-error")),
    isTRUE(!inherits(hasDimY, "try-error")),
    isTRUE(!inherits(hasVarX, "try-error")),
    isTRUE(!inherits(hasVarY, "try-error"))
  )

  all(is_gridded)
}


#' \var{XYZT} or \var{SZT} data dimensions for interacting with \var{netCDFs}
#'
#' @inheritParams rSW2st_netCDF
#' @param dims An integer vector.
#'
#' @return A named integer vector with six elements for
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

  dims <- if (is.null(dims)) {
    rep(0L, 4L)
  } else {
    as.integer(dims) # strips names
  }

  # Fix missing elements to NA
  nd <- length(dims)
  nstr <- nchar(data_str)
  if (nstr > 1L && nd < nstr) {
    dims[(nd + 1L):nstr] <- NA_integer_
  }


  # Compose result
  switch(
    EXPR = data_str,
    xyzt = c(
      ns = 0L,
      nx = dims[[1L]],
      ny = dims[[2L]],
      nz = dims[[3L]],
      nt = dims[[4L]],
      nv = 0L
    ),

    xyt = c(
      ns = 0L,
      nx = dims[[1L]],
      ny = dims[[2L]],
      nz = 0L,
      nt = dims[[3L]],
      nv = 0L
    ),

    xyz = c(
      ns = 0L,
      nx = dims[[1L]],
      ny = dims[[2L]],
      nz = dims[[3L]],
      nt = 0L,
      nv = 0L
    ),

    xy = c(
      ns = 0L,
      nx = dims[[1L]],
      ny = dims[[2L]],
      nz = 0L,
      nt = 0L,
      nv = if (nd >= 3L) dims[[3L]] else 0L
    ),

    szt = c(
      ns = dims[[1L]],
      nx = 0L,
      ny = 0L,
      nz = dims[[2L]],
      nt = dims[[3L]],
      nv = 0L
    ),

    st = c(
      ns = dims[[1L]],
      nx = 0L,
      ny = 0L,
      nz = 0L,
      nt = dims[[2L]],
      nv = 0L
    ),

    sz = c(
      ns = dims[[1L]],
      nx = 0L,
      ny = 0L,
      nz = dims[[2L]],
      nt = 0L,
      nv = 0L
    ),

    s = c(
      ns = dims[[1L]],
      nx = 0L,
      ny = 0L,
      nz = 0L,
      nt = 0L,
      nv = if (nd >= 2L) dims[[2L]] else 0L
    ),

    stop(
      "Data structure ", shQuote(data_str), " not implemented.",
      call. = FALSE
    )
  )
}


#' Guess `netCDF` data type of an object
#'
#' @param x An object
#'
#' @return A text string of supported `netCDF` data types. Supported
#' types are:
#' `"NC_INT"`, `"NC_DOUBLE"`, `"NC_CHAR"`
#'
#' @section Details:
#' The data type for a character object is `"NC_CHAR"`
#' (and not `"NC_STRING"`) because `"NC_STRING"` is not fully supported.
#'
#'
#' @seealso [RNetCDF::var.def.nc()]
#'
#' @examples
#' get_nc_type("test") ## "NC_CHAR"
#' get_nc_type(c(1L, 5L)) ## "NC_INT"
#' get_nc_type(1) ## "NC_DOUBLE"
#' \dontrun{get_nc_type(TRUE)} ## error
#'
#' @export
get_nc_type <- function(x) {
  switch(
    EXPR = storage.mode(x),
    integer = "NC_INT",
    double = "NC_DOUBLE",
    character = "NC_CHAR",
    stop(
      shQuote(storage.mode(x)), " is not implemented.",
      call. = FALSE
    )
  )
}



#' Extract \var{xy-space} values
#'
#' @param x An object that describes a gridded or discrete \var{xy-space}.
#'  Regular, rectangular grids are the only currently supported grid.
#'  This can be \itemize{
#'    \item an object identifying a \var{netCDF} file, i.e.,
#'          a character string as file name,
#'          an object of class \var{NetCDF}, or
#'          an object of class \var{ncdf4}.
#'    \item a \code{\link[raster:RasterLayer-class]{raster::RasterLayer}}
#'          object,
#'    \item a \code{\link[terra:SpatRaster-class]{terra::SpatRaster}}
#'          object,
#'    \item a \code{stars::stars} object,
#'    \item a list, such as the one produced by \code{\link{get_xyspace}}.
#'    \item an object with coordinate values for all \var{gridcell} centers
#'          that can be passed to \code{\link{as_points}};
#' }
#' @inheritParams rSW2st_netCDF
#' @inheritParams rSW2st_crs
#' @param res A numeric vector of length two. The (absolute values of)
#'   cell size/resolution/delta of the \var{x} and \var{y} dimensions
#'   in units of the \var{crs}.
#' @param tol A numeric value. The tolerance applied to determine if a grid
#'   is regular and to calculate grid resolution.
#'
#' @section Notes:
#' The argument \code{crs} is only used if \code{grid} is a \code{data.frame},
#' in which case it is passed to \code{\link{as_points}}.
#' Otherwise, it is ignored and can be missing.
#'
#' @section Notes:
#' The argument \code{res} is only used in two cases: \itemize{
#'   \item if \code{grid} is a list,
#'         such as the one produced by \code{\link{get_xyspace}}, but does
#'         not contain a named element \var{res}; or
#'   \item if \code{grid} is an object with coordinate values for
#'         all \var{gridcell} centers.
#' }
#' Otherwise, it is ignored and can be missing.
#'
#' @return A list with three elements:
#'  two vectors one each containing all unique values of the
#'  \var{x} coordinate and the \var{y} coordinates of
#'  (i) all \var{gricell} centers, if gridded, or
#'  (ii) all \var{sites}, if discrete;
#'  and a vector \var{res} with the (regular) \var{x} and \var{y} resolutions,
#'  if gridded (NA, if discrete).
#'
#' @examples
#' # grid as terra object
#' r <- terra::rast(
#'   xmin = 0, xmax = 120,
#'   ymin = 0, ymax = 45,
#'   crs = "OGC:CRS84",
#'   resolution = c(1, 1)
#' )
#' get_xyspace(r)
#'
#' # grid as data frame with coordinate values
#' rdf <- terra::crds(r)
#' get_xyspace(rdf, crs = "OGC:CRS84", res = c(1, 1))
#'
#' # a list with vectors for all x values and all y values (and resolution)
#' rl <- list(
#'   x = sort(unique(rdf[, 1])),
#'   y = sort(unique(rdf[, 2]))
#' )
#' get_xyspace(c(rl, list(res = c(1, 1))))
#' get_xyspace(rl, res = c(1, 1))
#'
#' @export
get_xyspace <- function(
  x,
  crs,
  res,
  xy_names = c("lon", "lat"),
  tol = sqrt(.Machine[["double.eps"]])
) {
  ox <- openRnetCDF(x, write = FALSE, stopOnError = FALSE)
  x <- ox[["con"]]
  if (ox[["closeOnExit"]]) on.exit(RNetCDF::close.nc(x))


  # TODO: convert function into S3 methods?
  # TODO: account for bounds
  # TODO: check/warn if gridded, but not regular
  if (inherits(x, "NetCDF")) {
    is_gridded <- is_netCDF_gridded(x, xy_names = xy_names)

    tmp_xy <- list(
      x = as.vector(
        RNetCDF::var.get.nc(x, variable = xy_names[[1L]], unpack = TRUE)
      ),
      y = as.vector(
        RNetCDF::var.get.nc(x, variable = xy_names[[2L]], unpack = TRUE)
      )
    )

    tmp_res <- if (is_gridded) {
      vapply(
        tmp_xy,
        function(x) {
          tmp <- unique(diff(x))
          if (length(tmp) > 1) {
            if (any(abs(diff(tmp)) > tol)) {
              stop(
                "Object is gridded, but likely not regular: ",
                "retrieved coordinate resolutions (a.k.a. deltas) are ",
                toString(tmp),
                call. = FALSE
              )
            }

            tmp[[1L]]

          } else {
            tmp
          }
        },
        FUN.VALUE = NA_real_
      )

    } else {
      rep(NA, 2)
    }

    xyspace <- c(
      tmp_xy,
      list(res = tmp_res)
    )

  } else if (inherits(x, "Raster")) {
    stopifnot(requireNamespace("raster"))

    xyspace <- list(
      x = raster::xFromCol(x, seq_len(raster::ncol(x))),
      y = raster::yFromRow(x, rev(seq_len(raster::nrow(x)))),
      res = raster::res(x)
    )

  } else if (inherits(x, "SpatRaster")) {
    xyspace <- list(
      x = terra::xFromCol(x, seq_len(terra::ncol(x))),
      y = terra::yFromRow(x, rev(seq_len(terra::nrow(x)))),
      res = terra::res(x)
    )

  } else if (inherits(x, "stars")) {
    tmp_res <- abs(unname(
      vapply(
        stars::st_dimensions(x),
        function(x) x[["delta"]],
        FUN.VALUE = NA_real_
      )
    ))[1:2]

    if (anyNA(tmp_res)) {
      stop(
        "Can currently only handle regular, rectangular grids.",
        call. = FALSE
      )
    }

    xyspace <- list(
      x = sort(stars::st_get_dimension_values(x, which = 1, center = TRUE)),
      y = sort(stars::st_get_dimension_values(x, which = 2, center = TRUE)),
      res = tmp_res
    )

  } else {

    is_not_points <-
      identical(names(x), c("x", "y", "res")) ||
      all(length(x) >= 2L, length(x[[1L]]) != length(x[[2L]]))

    if (!is_not_points) {
      tmp <- try(
        as_points(x, to_class = "sf", crs = crs),
        silent = TRUE
      )

      is_not_points <- inherits(tmp, "try-error")
    }

    xyspace <- if (is_not_points) {
      list(
        x = sort(unique(x[[1L]])),
        y = sort(unique(x[[2L]])),
        res = if ("res" %in% names(x)) x[["res"]][1:2] else res[1:2]
      )

    } else {
      tmp <- sf::st_coordinates(tmp)[, 1:2, drop = FALSE]
      list(
        x = sort(unique(tmp[, 1])),
        y = sort(unique(tmp[, 2])),
        res = res[1:2]
      )
    }
  }

  tmp_res <- unname(xyspace[["res"]])
  c(
    xyspace[c("x", "y")],
    res = list(c(x = tmp_res[[1L]], y = tmp_res[[2L]]))
  )
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
    if (data_str != "xy") {
      stop(
        "`data` is a vector or similar object, but `data_str` is not 'xy'.",
        call. = FALSE
      )
    }

    # Convert vector into matrix
    data <- matrix(data, ncol = 1, dimnames = list(NULL, names(data)))
  }

  data_dims <- dim(data)


  #--- check crs
  stopifnot(
    inherits(try(sf::st_crs(locations_crs), silent = TRUE), "crs")
  )


  #--- xy-coordinates of grid
  xy_grid <- get_xyspace(grid, crs = locations_crs)
  n_cells <- prod(lengths(xy_grid[1:2]))


  #--- xy-coordinates of data
  locations <- as_points(locations, to_class = "sf", crs = locations_crs)
  n_loc <- nrow(locations)
  xy_data <- sf::st_coordinates(locations)[, 1:2, drop = FALSE]

  # Check if locations are outside grid
  ids_outside <-
    xy_data[, 1] < (min(xy_grid[[1L]]) - xy_grid[["res"]][[1L]] / 2) |
    xy_data[, 1] > (max(xy_grid[[1L]]) + xy_grid[["res"]][[1L]] / 2) |
    xy_data[, 2] < (min(xy_grid[[2L]]) - xy_grid[["res"]][[2L]] / 2) |
    xy_data[, 2] > (max(xy_grid[[2L]]) + xy_grid[["res"]][[2L]] / 2)


  #--- Map locations (xy_data) to gridcells (xy_grid)
  # i.e, identify the gridcell x-rows/y-columns for each location
  ids_x <- vapply(
    xy_data[, 1],
    function(x) which.min(abs(xy_grid[[1L]] - x)),
    FUN.VALUE = NA_integer_
  )
  ids_y <- vapply(
    xy_data[, 2],
    function(x) which.min(abs(xy_grid[[2L]] - x)),
    FUN.VALUE = NA_integer_
  )

  if (any(ids_outside)) {
    warning(
      "`locations` fall outside the `grid`: n = ", sum(ids_outside),
      "; they will return NA.",
      call. = FALSE
    )

    ids_x[ids_outside] <- NA_integer_
    ids_y[ids_outside] <- NA_integer_
  }

  if (anyDuplicated(cbind(ids_x, ids_y)) > 0) {
    warning(
      "`locations` identify non-unique cells on the `grid`.",
      call. = FALSE
    )
  }


  if (direction == "expand") {
    #------ Expand one xy-dimension into separate x- and y-dimensions
    if (data_dims[[1L]] > n_cells) {
      stop(
        "`nrow(data)` must be smaller or equal to ",
        "the number of cells in `grid`.",
        call. = FALSE
      )
    }

    if (data_dims[[1L]] != n_loc) {
      stop(
        "The number of locations must match `nrow(data)`.",
        call. = FALSE
      )
    }


    #--- Expand data
    tmp_dn <- strsplit(data_str, split = "", fixed = TRUE)[[1L]]
    if (length(tmp_dn) < length(data_dims) && data_str == "xy") {
      tmp_dn <- c(tmp_dn, "v")
    }

    res <- array(
      dim = unname(c(lengths(xy_grid)[1:2], data_dims[-1])),
      dimnames = lapply(tmp_dn, function(x) NULL)
    )

    if (data_str %in% c("xyt", "xyz", "xy")) {
      ids_tzv <- rep(seq_len(data_dims[[2L]]), each = n_loc)
      res[cbind(ids_x, ids_y, ids_tzv)] <- as.matrix(data)

    } else if (data_str == "xyzt") {
      ids_t <- rep(seq_len(data_dims[[2L]]), each = n_loc)
      ids_z <- rep(seq_len(data_dims[[3L]]), each = prod(data_dims[1:2]))
      res[cbind(ids_x, ids_y, ids_t, ids_z)] <- data

    } else {
      stop(
        "No implementation for `data` to expand space; ",
        "`data` has dimensions: ", toString(data_dims),
        " and structure ", shQuote(data_str),
        call. = FALSE
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
        ids_tzv <- rep(seq_len(data_dims[[3L]]), each = n_loc)
        res <- data[cbind(ids_x, ids_y, ids_tzv)]
        attr(res, "dim") <- c(n_loc, data_dims[[3L]])
      }

    } else if (length(data_dims) == 4L && data_str == "xyzt") {
      ids_t <- rep(seq_len(data_dims[[3L]]), each = n_loc)
      ids_z <- rep(seq_len(data_dims[[4L]]), each = n_loc * data_dims[[3L]])
      res <- data[cbind(ids_x, ids_y, ids_t, ids_z)]
      attr(res, "dim") <- c(n_loc, data_dims[3:4])
    }

    if (is.null(res)) {
      stop(
        "No implementation for `data` to collapse space; ",
        "`data` has dimensions: ", toString(data_dims),
        " and structure ", shQuote(data_str),
        call. = FALSE
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
#' unlink("Rplots.pdf")
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


  # nolint start: line_length_linter.
  # USA Contiguous Albers Equal Area Conic USGS version
  # proj4string was +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs
  # http://cfconventions.org/Data/cf-conventions/cf-conventions-1.8/cf-conventions.html#_albers_equal_area
  # nolint end
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
          nc_att_crs[["standard_parallel"]][[2L]]
        ),
        crs = "OGC:CRS84",
        to_class = "sf"
      ),
      nc_att_crs[["crs_wkt"]]
    )
  )[1, ]

  dxy <- c(20, 30)
  nxy <- 2 * dxy + 1

  x <- seq(-dxy[[1L]], dxy[[1L]], length = nxy[[1L]])
  y <- seq(-dxy[[2L]], dxy[[2L]], length = nxy[[2L]])



  raster_xy <- suppressWarnings(
    terra::rast(
      xmin = orig[[1L]] + min(x) - 0.5,
      xmax = orig[[1L]] + max(x) + 0.5,
      ymin = orig[[2L]] + min(y) - 0.5,
      ymax = orig[[2L]] + max(y) + 0.5,
      crs = nc_att_crs[["crs_wkt"]],
      resolution = c(1, 1)
    )
  )


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
    r <- sqrt(x^2 + y^2 + z^3)
    sin(r * t) / r
  }

  for (k1 in seq_len(nt)) {
    for (k2 in seq_len(nz)) {
      data_xyzt[, , k2, k1] <- outer(
        x, y,
        FUN = f,
        z = vertical_values[k2],
        t = k1 / (0.75 * nt)
      )
    }
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
