
openRnetCDF <- function(x, write = FALSE, stopOnError = TRUE) {
  closeOnExit <- FALSE

  if (inherits(x, "ncdf4")) {
    x <- x[["filename"]]
  }

  if (inherits(x, "NetCDF")) {
    xnc <- x
  } else if (inherits(x, "character")) {
    xnc <- RNetCDF::open.nc(x, write = write)
    closeOnExit <- TRUE
  } else if (isTRUE(stopOnError)) {
    stop(
      "Class ", class(x), " not implemented for argument 'x'.",
      call. = FALSE
    )
  } else {
    xnc <- x
  }

  list(con = xnc, closeOnExit = closeOnExit)
}


#' `netCDF` functionality suitable for `SOILWAT2`
#'
#' @param x A character string (file name that will be opened and closed),
#' an object of class `"NetCDF"` from the `RNetCDF` package (kept open), or
#' an object of class `"ncdf4"` from the `ncdf4` package (kept open).
#' @param long_name A character string. The `"long_name"` attribute.
#' @param units A character string.  The `"units"` attribute.
#' @param cell_method A character string. The `"cell_method"` attribute.
#' @param coordinates A character string. The `"coordinates"` attribute.
#' @param grid_mapping A character string. The `"grid_mapping"` attribute.
#' @param attributes A named vector or named list of character strings.
#' @param dataType A character string. A `netCDF` data type, see [ncDataType()].
#' @param nameDimX A character string. The name of the `X`-axis dimension and
#' coordinate variable.
#' @param nameDimY A character string. The name of the `Y`-axis dimension and
#' coordinate variable.
#' @param nameCRS A character string. The name of the `CRS` variable.
#' @param nameAxis A character string. The name of the axis dimension and
#' associated coordinate variable.
#' @param values A numeric object. The values to write a variable if not `NULL`.
#' @param siteValues A numeric object. The values of the discrete `site` axis
#' variable.
#' @param timeValues A numeric vector. The values of the time axis variable.
#' @param verticalValues A numeric vector. The values of the vertical axis
#' variable.
#' @param pftValues A vector of character strings. The value of the variable
#' representing plant functional types.
#' @param var_chunksizes_xyzt A numeric vector. The chunk sizes if not `NA`,
#' see [`RNetCDF::var.def.nc()`] for more detail.
#' @param isUnlimitedDim A logical value. Define the new dimension as unlimited?
#'
#'
#'
#' @name ncsw
NULL
#--- ncsw ------

#' Translate to `netCDF` data types
#'
#' @inheritParams ncsw
#' @param stopOnError A logical value.
#'
#' @return A standardized `netCDF` library data type.
#' If not found, then return value depends on `stopOnError`:
#'    - if `TRUE`, then signal an error
#'    - if `FALSE`, then return unmodified value of `dataType`
#'
#' @examples
#' ncDataType("integer")
#' ncDataType("noType", stopOnError = FALSE)
#'
#' @export
ncDataType <- function(dataType, stopOnError = TRUE) {
  switch(
    EXPR = toupper(dataType),

    BYTE = ,
    NC_BYTE = "NC_BYTE",

    UBYTE = ,
    NC_UBYTE = "NC_UBYTE",

    CHAR = ,
    NC_CHAR = "NC_CHAR",

    SHORT = ,
    NC_SHORT = "NC_SHORT",

    USHORT = ,
    NC_USHORT = "NC_USHORT",

    INT = ,
    INTEGER = ,
    NC_INT = "NC_INT",

    UINT = ,
    NC_UINT = "NC_UINT",

    INT64 = ,
    NC_INT64 = "NC_INT64",

    UINT64 = ,
    NC_UINT64 = "NC_UINT64",

    FLOAT = ,
    NC_FLOAT = "NC_FLOAT",

    DOUBLE = ,
    NC_DOUBLE = "NC_DOUBLE",

    STRING = ,
    NC_STRING = "NC_STRING",

    # nolint start: unnecessary_nesting_linter.
    if (isTRUE(stopOnError)) {
      stop(shQuote(dataType), " is not implemented.", call. = FALSE)
    } else {
      dataType
    }
    # nolint end: unnecessary_nesting_linter.
  )
}

#' Identify the fill value corresponding to a data type
#'
#' @inheritParams ncsw
#'
#' @examples
#' fillValue("integer")
#'
#' @export
fillValue <- function(dataType) {
  switch(
    EXPR = toupper(ncDataType(dataType, stopOnError = FALSE)),

    NC_BYTE = ,
    NC_FILL_BYTE = -127L,

    NC_UBYTE = ,
    NC_FILL_UBYTE = 255L,

    NC_CHAR = ,
    NC_FILL_CHAR = 0,

    NC_SHORT = ,
    NC_FILL_SHORT = -32768L,

    NC_USHORT = ,
    NC_FILL_USHORT = 65535L,

    NC_INT = ,
    NC_FILL_INT = -2147483647L,

    NC_UINT = ,
    NC_FILL_UINT = 4294967295,

    NC_INT64 = ,
    NC_FILL_INT64 = -9223372036854775806,

    NC_UINT64 = ,
    NC_FILL_UINT64 = 18446744073709551614,

    NC_FLOAT = ,
    NC_FILL_FLOAT = ,

    NC_DOUBLE = ,
    NC_FILL_DOUBLE = 9.9692099683868690e+36,

    NC_STRING = ,
    NC_FILL_STRING = "",

    stop(shQuote(dataType), " is not implemented.", call. = FALSE)
  )
}


#' Unique differences between adjacent values
#'
#' Check if values are regularly spaced.
#'
#' @param x A numeric vector.
#' @param uniqueDifferences A logical value. Find unique differences between
#' adjacent values in `x`.
#' @param allowMultiple A logical value. Allow more than one unique difference.
#' @param tolerance A numeric value. Tolerance to identify unique differences.
#'
#' @examples
#' x <- seq(0, 1, by = 1 / 24)
#' uniqueDifferences(x)
#'
#' x[[2L]] <- x[[2L]] + sqrt(.Machine[["double.eps"]])
#' try(uniqueDifferences(x))
#' uniqueDifferences(x, tolerance = 1e-7)
#' uniqueDifferences(x, allowMultiple = TRUE)
#'
#' @export
uniqueDifferences <- function(
  x,
  uniqueDifferences = TRUE,
  allowMultiple = FALSE,
  tolerance = sqrt(.Machine[["double.eps"]])
) {
  res <- diff(x)

  if (isTRUE(uniqueDifferences)) {
    res <- unique(sort(res))

    if (length(res) > 1L && !is.null(tolerance) && tolerance > 0) {
      notUnique <- duplicated(round(res / tolerance, 0L))
      if (sum(notUnique) > 0L) {
        res <- res[!notUnique]
      }
    }
  }

  if (isTRUE(!allowMultiple) && length(res) > 1L) {
    stop("Values are irregular: ", toString(res), call. = FALSE)
  }

  res
}


#' Write spatial objects to a `netCDF` file suitable for `SOILWAT2`
#'
#' @section Details:
#' `writeTerraToNCSW()` writes a `terra` raster object to a `netCDF` file
#' suitable for `SOILWAT2` simulations.
#'
#' @inheritParams ncsw
#' @param filename A character string. The file name of the `netCDF` file
#' to be created.
#' @param increasingLat A logical value. Sort `Y`-axis values increasingly.
#' @param addSpatialBounds A logical value. Add spatial bounds.
#' @param nameAxisVertical A character string. The name of the `Z`-axis
#' dimension and coordinate variable.
#' @param nameAxisTime A character string. The name of the `T`-axis
#' dimension and coordinate variable.
#' @param deleteGlobalAttributes A vector of character strings.
#' Global attributes to delete.
#'
#' @export
writeTerraToNCSW <- function(
  x,
  filename,
  var_chunksizes_xyzt = NA,
  dataType = c("double", "float", "integer", "short", "byte", "char"),
  increasingLat = TRUE,
  nameDimX = "longitude",
  nameDimY = "latitude",
  addSpatialBounds = TRUE,
  nameAxisVertical = "vertical",
  verticalValues = NULL,
  nameAxisTime = "time",
  timeValues = NULL,
  deleteGlobalAttributes = c("created_by", "created_date", "date")
) {

  # `terra::writeCDF()` uses "ncdf4" but it is a suggested package
  stopifnot(requireNamespace("ncdf4", quietly = TRUE))

  dataType <- match.arg(dataType) # terra dataType

  if (increasingLat) {
    x <- terra::flip(x, direction = "vertical")
  }

  listArgsWriteCDF <- list(
    filename = filename,
    varname = terra::varnames(x),
    longname =  terra::longnames(x),
    unit = terra::units(x),
    gridmap = NULL, # writes only text
    prec = dataType,
    compression = 5L,
    shuffle = dataType %in% c("integer", "short", "byte", "char"),
    chunksizes = var_chunksizes_xyzt,
    missval = fillValue(dataType)
  )

  hasVertical <- !is.null(nameAxisVertical) && !is.null(verticalValues)
  hasTime <- !is.null(nameAxisTime) && !is.null(timeValues)

  if (getNamespaceVersion("terra") >= numeric_version("1.8-42")) {
    if (hasVertical) {
      terra::depthName(x) <- nameAxisVertical
      terra::depth(x) <- verticalValues
    } else {
      terra::depthName(x) <- ""
      terra::depth(x) <- NULL
    }

    if (hasTime) {
      terra::time(x) <- timeValues
    } else {
      terra::time(x) <- NULL
    }

  } else {
    # terra v1.8-42 introduced terra::depth() and terra::time()
    if (hasVertical && hasTime) {
      stop("Upgrade 'terra' to support xyzt data.", call. = FALSE)
    }

    listArgsWriteCDF[["zname"]] <- if (hasVertical) {
      nameAxisVertical
    } else if (hasTime) {
      nameAxisTime
    }
  }

  listArgsWriteCDF[["x"]] <- x

  do.call(terra::writeCDF, args = listArgsWriteCDF)


  #--- Open netCDF for post-processing
  xnc <- RNetCDF::open.nc(filename, write = TRUE)
  on.exit(RNetCDF::close.nc(xnc))


  #--- Delete unwanted global attributes created by terra
  deleteGlobalAttributesNCSW(xnc, deleteGlobalAttributes)


  #--- Flip latitude values
  # [terra::writeCDF] (v1.7.78) forces latitude to be decreasing, see
  # nolint start.
  # [terra:::.write_cdf] `ydim <- ncdf4::ncdim_def( yname, yunit, yFromRow(y, 1:nrow(y)) )`
  # nolint end.
  xlat <- RNetCDF::var.get.nc(xnc, variable = nameDimY)
  RNetCDF::var.put.nc(xnc, variable = nameDimY, data = rev(xlat))


  #--- longitude/latitude attributes
  RNetCDF::att.put.nc(xnc, nameDimX, "axis", "NC_CHAR", value = "X")
  RNetCDF::att.put.nc(
    xnc, nameDimX, "standard_name", "NC_CHAR", value = nameDimX
  )

  RNetCDF::att.put.nc(xnc, nameDimY, "axis", "NC_CHAR", value = "Y")
  RNetCDF::att.put.nc(
    xnc, nameDimY, "standard_name", "NC_CHAR", value = nameDimY
  )


  #--- Spatial bounds
  if (isTRUE(addSpatialBounds)) {
    setSpatialBoundsNCSW(xnc, nameDimX = nameDimX, nameDimY = nameDimY)
  }

  #--- Set CRS to WGS84
  if (isTRUE(sf::st_crs(x) == sf::st_crs("WGS84"))) {
    setCRSWGS84NCSW(xnc, nameCRS = "crs")
  }
}


#' Create a `CRS` variable
#'
#' @inheritParams ncsw
#' @param grid_mapping_name A character string. The `"grid_mapping_name"`
#' attribute.
#' @param crs_wkt A character string. The `"crs_wkt"` attribute.
#'
#' @return [setCRSNCSW()]: creates a `CRS` variable with custom attributes
#'
#' @export
setCRSNCSW <- function(
  x,
  nameCRS = "crs",
  grid_mapping_name = NULL,
  crs_wkt = NULL,
  attributes = NULL
) {
  ox <- openRnetCDF(x, write = TRUE)
  xnc <- ox[["con"]]
  if (ox[["closeOnExit"]]) on.exit(RNetCDF::close.nc(xnc))

  res <- try(RNetCDF::var.inq.nc(xnc, nameCRS), silent = TRUE)
  if (inherits(res, "try-error")) {
    RNetCDF::var.def.nc(
      xnc, varname = nameCRS, vartype = "NC_BYTE", dimensions = NA
    )
  }

  #--- Attributes
  tmp <- c(
    if (!is.null(grid_mapping_name)) {
      list(grid_mapping_name = grid_mapping_name)
    },
    if (!is.null(crs_wkt)) list(crs_wkt = crs_wkt),
    if (!is.null(attributes)) as.list(attributes)
  )
  tmp <- tmp[unique(names(tmp))]
  tmp <- tmp[lengths(tmp) > 0L]

  for (k in seq_along(tmp)) {
    RNetCDF::att.put.nc(
      xnc,
      variable = nameCRS,
      name = names(tmp)[[k]],
      type = get_nc_type(tmp[[k]]),
      value = tmp[[k]]
    )
  }
}

#' @rdname setCRSNCSW
#'
#' @inheritParams ncsw
#'
#' @return [setCRSWGS84NCSW()]: creates a `CRS` variable with `WGS84` attributes
#'
#' @examples
#' fn <- tempfile(fileext = ".nc")
#' nc <- RNetCDF::create.nc(fn, format = "netcdf4")
#' setCRSWGS84NCSW(nc)
#'
#' RNetCDF::print.nc(nc)
#' RNetCDF::close.nc(nc)
#' unlink(fn)
#'
#' @export
setCRSWGS84NCSW <- function(x, nameCRS = "crs") {
  setCRSNCSW(
    x,
    nameCRS = nameCRS,
    grid_mapping_name = "latitude_longitude",
    crs_wkt = paste0(
      'GEOGCS["WGS 84",DATUM["WGS_1984",',
      'SPHEROID["WGS 84",6378137,298.257223563,',
      'AUTHORITY["EPSG","7030"]],AUTHORITY["EPSG","6326"]],',
      'PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],',
      'UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],',
      'AUTHORITY["EPSG","4326"]]'
    ),
    attributes = list(
      long_name = "WGS84",
      longitude_of_prime_meridian = 0.0,
      semi_major_axis = 6378137.0,
      inverse_flattening = 298.257223563
    )
  )
}


#' Check spatial structure of a `netCDF`
#'
#' @inheritParams ncsw
#' @inheritParams uniqueDifferences
#' @param expectedSpatialDims A vector of length 2. Function will check
#' spatial dimensions of `x` against `expectedSpatialDims` if not `NA`.
#' @param expectedSpatialExtent A named vector of length 4. Function will check
#' spatial extent of `x` against `expectedSpatialExtent` if not `NA`.
#'
#' @export
checkSpatialNCSW <- function(
  x,
  nameDimX = "longitude",
  nameDimY = "latitude",
  expectedSpatialDims = c(NA, NA),
  expectedSpatialExtent = c(xmin = NA, xmax = NA, ymin = NA, ymax = NA),
  tolerance = sqrt(.Machine[["double.eps"]])
) {

  ox <- openRnetCDF(x, write = FALSE)
  xnc <- ox[["con"]]
  if (ox[["closeOnExit"]]) on.exit(RNetCDF::close.nc(xnc))


  #--- Check spatial dimensions
  if (!anyNA(expectedSpatialDims)) {
    domain_dim <- c(
      RNetCDF::dim.inq.nc(xnc, nameDimX)[["length"]],
      RNetCDF::dim.inq.nc(xnc, nameDimY)[["length"]]
    )

    stopifnot(
      domain_dim[[1L]] == expectedSpatialDims[[1L]],
      domain_dim[[2L]] == expectedSpatialDims[[2L]]
    )
  }

  if (!anyNA(expectedSpatialExtent)) {
    valuesAxisX <- RNetCDF::var.get.nc(xnc, nameDimX)
    resX <- abs(uniqueDifferences(valuesAxisX))[[1L]]
    valuesAxisY <- RNetCDF::var.get.nc(xnc, nameDimY)
    resY <- abs(uniqueDifferences(valuesAxisY))[[1L]]

    ext <- c(
      xmin = min(valuesAxisX) - resX / 2,
      xmax = max(valuesAxisX) + resX / 2,
      ymin = min(valuesAxisY) - resY / 2,
      ymax = max(valuesAxisY) + resY / 2
    )

    stopifnot(
      isTRUE(all.equal(ext[["xmin"]], expectedSpatialExtent[["xmin"]])),
      isTRUE(all.equal(ext[["xmax"]], expectedSpatialExtent[["xmax"]])),
      isTRUE(all.equal(ext[["ymin"]], expectedSpatialExtent[["ymin"]])),
      isTRUE(all.equal(ext[["ymax"]], expectedSpatialExtent[["ymax"]]))
    )
  }
}


#' Rename spatial dimensions and variables
#'
#' @inheritParams ncsw
#'
#' @export
renameSpatialNCSW <- function(
  x,
  nameDimX = c(orig = "longitude", to = "lon"),
  nameDimY = c(orig = "latitude", to = "lat"),
  nameCRS = c(orig = "crs", to = "crs_geogsc")
) {

  ox <- openRnetCDF(x, write = TRUE)
  xnc <- ox[["con"]]
  if (ox[["closeOnExit"]]) on.exit(RNetCDF::close.nc(xnc))


  if (!is.null(nameDimX) && !identical(nameDimX[["orig"]], nameDimX[["to"]])) {
    RNetCDF::dim.rename.nc(xnc, nameDimX[["orig"]], newname = nameDimX[["to"]])
    RNetCDF::var.rename.nc(xnc, nameDimX[["orig"]], newname = nameDimX[["to"]])
  }

  if (!is.null(nameDimY) && !identical(nameDimY[["orig"]], nameDimY[["to"]])) {
    RNetCDF::dim.rename.nc(xnc, nameDimY[["orig"]], newname = nameDimY[["to"]])
    RNetCDF::var.rename.nc(xnc, nameDimY[["orig"]], newname = nameDimY[["to"]])
  }

  if (!is.null(nameCRS) && !identical(nameCRS[["orig"]], nameCRS[["to"]])) {
    RNetCDF::var.rename.nc(xnc, nameCRS[["orig"]], newname = nameCRS[["to"]])
  }
}


#' Set bounds for a coordinate variable
#'
#' @inheritParams ncsw
#' @param nameBndsDim A character string. The name of the bounds dimension.
#' @param nameDim A character string. The name of the associated dimensions.
#' @param nameBndsVar A character string. The name of the selected axis bounds
#' variable.
#' @param nameBndsX A character string. The name of the `X`-axis bounds
#' variable.
#' @param nameBndsY A character string. The name of the `Y`-axis bounds
#' variable.
#' @param valuesBnds A numeric vector. The values of the selected axis bounds
#' variable.
#' @param valuesBndsX A numeric vector. The values of the `X`-axis bounds
#' variable. Values will derived from `X`-axis variable assuming
#' symmetric bounds if `NULL`.
#' @param valuesBndsY A numeric vector. The values of the `Y`-axis bounds
#' symmetric bounds if `NULL`.
#' @param calculateValuesBndsIfMissing A logical value. The values of
#' the selected axis bounds will derived from the associated axis variable
#' assuming symmetric bounds.
#' @param boundsAttributeName A character vector. The name of the bounds
#' attribute that is added to the `nameDim` variable. In most cases, this
#' attribute's name is `"bounds"` or `"climatology"`.
#'
#' @return [setAxisBoundsNCSW()]: set bounds on a selected axis.
#'
#' @export
setAxisBoundsNCSW <- function(
  x,
  nameBndsVar,
  nameDim,
  valuesBnds = NULL,
  calculateValuesBndsIfMissing = FALSE,
  boundsAttributeName = "bounds",
  nameBndsDim = "bnds"
) {
  ox <- openRnetCDF(x, write = TRUE)
  xnc <- ox[["con"]]
  if (ox[["closeOnExit"]]) on.exit(RNetCDF::close.nc(xnc))


  #--- Create dimension
  res <- try(RNetCDF::dim.inq.nc(xnc, nameBndsDim), silent = TRUE)
  if (inherits(res, "try-error")) {
    RNetCDF::dim.def.nc(xnc, nameBndsDim, dimlength = 2L)
  }

  #--- Create bound variable
  res <- try(RNetCDF::var.inq.nc(xnc, nameBndsVar), silent = TRUE)
  if (inherits(res, "try-error")) {
    RNetCDF::var.def.nc(
      xnc,
      varname = nameBndsVar,
      vartype = "NC_DOUBLE",
      dimensions = c(nameBndsDim, nameDim),
      deflate = 5L,
      shuffle = TRUE
    )

    if (is.null(valuesBnds) && isTRUE(calculateValuesBndsIfMissing)) {
      valuesAxis <- RNetCDF::var.get.nc(xnc, nameDim)
      res <- abs(uniqueDifferences(valuesAxis)[[1L]])

      valuesBnds <- apply(
        rbind(valuesAxis - res / 2, valuesAxis + res / 2),
        MARGIN = 2L,
        sort
      )
    }

    RNetCDF::var.put.nc(xnc, nameBndsVar, data = valuesBnds)
    RNetCDF::att.put.nc(
      xnc, nameDim, boundsAttributeName, "NC_CHAR", value = nameBndsVar
    )
  }
}

#' @rdname setAxisBoundsNCSW
#'
#' @return [setSpatialBoundsNCSW()]: set spatial bounds on x-axis and y-axis.
#'
#' @export
setSpatialBoundsNCSW <- function(
  x,
  nameBndsDim = "bnds",
  nameBndsX = "lon_bnds",
  nameBndsY = "lat_bnds",
  nameDimX = "longitude",
  nameDimY = "latitude",
  valuesBndsX = NULL,
  valuesBndsY = NULL
) {
  setAxisBoundsNCSW(
    x,
    nameBndsVar = nameBndsX,
    nameDim = nameDimX,
    valuesBnds = valuesBndsX,
    calculateValuesBndsIfMissing = TRUE,
    nameBndsDim = nameBndsDim
  )

  setAxisBoundsNCSW(
    x,
    nameBndsVar = nameBndsY,
    nameDim = nameDimY,
    valuesBnds = valuesBndsY,
    calculateValuesBndsIfMissing = TRUE,
    nameBndsDim = nameBndsDim
  )
}


#' Add a dimension and associated (coordinate) variable
#'
#' @inheritParams ncsw
#' @param axis A character string. The `"axis"` attribute.
#'
#' @export
setAxisNCSW <- function(
  x,
  nameAxis,
  dataType,
  values = NULL,
  isUnlimitedDim = FALSE,
  axis = NULL,
  long_name = NULL,
  units = NULL,
  attributes = NULL
) {
  dataType <- ncDataType(dataType[[1L]])

  if (!is.null(axis)) stopifnot(axis %in% c("X", "Y", "Z", "T"))

  ox <- openRnetCDF(x, write = TRUE)
  xnc <- ox[["con"]]
  if (ox[["closeOnExit"]]) on.exit(RNetCDF::close.nc(xnc))


  #--- Create dimension
  res <- try(RNetCDF::dim.inq.nc(xnc, nameAxis), silent = TRUE)
  if (inherits(res, "try-error")) {
    RNetCDF::dim.def.nc(
      xnc,
      dimname = nameAxis,
      dimlength = length(values),
      unlim = isUnlimitedDim
    )
  }

  #--- Create variable
  res <- try(RNetCDF::var.inq.nc(xnc, nameAxis), silent = TRUE)
  if (inherits(res, "try-error")) {
    RNetCDF::var.def.nc(
      xnc, varname = nameAxis, vartype = dataType, dimensions = nameAxis
    )
  }

  if (!is.null(values)) {
    RNetCDF::var.put.nc(xnc, variable = nameAxis, data = values)
  }


  #--- Variable attributes
  tmp <- c(
    if (!is.null(axis)) list(axis = axis),
    if (!is.null(long_name)) list(long_name = long_name),
    if (!is.null(units)) list(units = units),
    if (!is.null(attributes)) as.list(attributes)
  )
  tmp <- tmp[unique(names(tmp))]
  tmp <- tmp[lengths(tmp) > 0L]

  for (k in seq_along(tmp)) {
    RNetCDF::att.put.nc(
      xnc,
      variable = nameAxis,
      name = names(tmp)[[k]],
      type = "NC_CHAR",
      value = as.character(tmp[[k]])
    )
  }
}


#' @rdname setAxisNCSW
#'
#' @return [setAxisSiteNCSW()] adds a station/site dimension and associated
#' coordinate variable.
#'
#' @export
setAxisSiteNCSW <- function(
  x,
  siteValues,
  nameAxis = "site",
  units = "1",
  dataType = "NC_INT"
) {
  setAxisNCSW(
    x,
    nameAxis = nameAxis,
    dataType = dataType,
    values = siteValues,
    axis = NULL,
    long_name = "simulation site",
    units = units,
    attributes = c(cf_role = "timeseries_id")
  )
}


#' @rdname setAxisNCSW
#'
#' @return [setAxisVerticalNCSW()] adds a vertical dimension and associated
#' coordinate variable (`Z`-axis).
#'
#' @param verticalUpperBound A numerical vector. The upper (top) soil depths
#' (bounds) of each soil layer (as defined by `verticalValues`).
#' @param verticalLowerBound A numerical vector. The lower (bottom) soil depths
#' (bounds) of each soil layer (as defined by `verticalValues`).
#' @param verticalType A character string. The type of the vertical axis.
#' @param units A character string. The length units of `verticalType`.
#'
#' @export
setAxisVerticalNCSW <- function(
  x,
  verticalValues,
  verticalUpperBound = NULL,
  verticalLowerBound = NULL,
  verticalType = c("values", "layers"),
  nameAxis = "vertical",
  units = "cm",
  dataType = "NC_INT"
) {
  verticalType <- match.arg(verticalType)

  ox <- openRnetCDF(x, write = TRUE)
  xnc <- ox[["con"]]
  if (ox[["closeOnExit"]]) on.exit(RNetCDF::close.nc(xnc))

  setAxisNCSW(
    xnc,
    nameAxis = nameAxis,
    dataType = dataType,
    values = switch(
      EXPR = verticalType,
      values = verticalValues,
      layers = seq_along(verticalValues)
    ),
    axis = "Z",
    long_name = switch(
      EXPR = verticalType,
      values = "soil depth",
      layers = "soil layer"
    ),
    units = if (identical(verticalType, "values")) units,
    attributes = c(
      standard_name = "depth",
      positive = "down",
      comment = if (identical(verticalType, "layers")) {
        "dimensionless vertical coordinate"
      }
    )
  )

  #--- Vertical bounds
  if (
    identical(verticalType, "values") &&
      !is.null(verticalUpperBound) && !is.null(verticalLowerBound)
  ) {
    vertical_bnds <- apply(
      rbind(verticalUpperBound, verticalLowerBound),
      MARGIN = 2L,
      sort
    )

    setAxisBoundsNCSW(
      xnc,
      nameBndsVar = paste0(nameAxis, "_bnds"),
      nameDim = nameAxis,
      valuesBnds = vertical_bnds
    )
  }
}


#' @rdname setAxisNCSW
#'
#' @return [setAxisPFTsNCSW()] adds a plant functional type dimension and
#' associated coordinate variable.
#'
#' @export
setAxisPFTsNCSW <- function(
  x,
  pftValues = c(
    Tree = "Trees", Shrub = "Shrubs", Forb = "Forbs", Grass = "Grasses"
  ),
  nameAxis = "pft",
  dataType = c("NC_STRING", "NC_BYTE")
) {
  dataType <- ncDataType(dataType[[1L]])

  ox <- openRnetCDF(x, write = TRUE)
  xnc <- ox[["con"]]
  if (ox[["closeOnExit"]]) on.exit(RNetCDF::close.nc(xnc))

  setAxisNCSW(
    xnc,
    nameAxis = nameAxis,
    dataType = dataType,
    values = if (identical(dataType, "NC_STRING")) {
      pftValues
    } else if (identical(dataType, "NC_BYTE")) {
      seq_along(pftValues)
    },
    axis = NULL,
    long_name = NULL,
    units = NULL,
    attributes = c(
      standard_name = "biological_taxon_name",
      flag_meanings = if (identical(dataType, "NC_BYTE")) {
        paste(pftValues, collapse = " ")
      }
    )
  )


  if (identical(dataType, "NC_BYTE")) {
    RNetCDF::att.put.nc(
      xnc, nameAxis, "flag_values", "NC_BYTE", value = seq_along(pftValues)
    )
  }
}


#' @rdname setAxisNCSW
#'
#' @return [setAxisTimeNCSW()] adds a time dimension and associated
#' coordinate variable (`T`-axis).
#'
#' @inheritParams ncsw
#' @param startYear An integer value. The calendar year used as reference
#' in the time units `"days since ...`. `timeUnits` takes precedence if both
#' are provided.
#' @param timeUnits A character string. The time units,
#' e.g., `"days since ...`. `timeUnits` takes precedence over `startYear`
#' if both are provided.
#' @param calendar A character string representing the calendar type.
#'
#' @export
setAxisTimeNCSW <- function(
  x,
  timeValues,
  startYear = NULL,
  timeUnits = NULL,
  nameAxis = "time",
  calendar = c("standard", "365_day", "366_day"),
  dataType = "NC_DOUBLE",
  isUnlimitedDim = FALSE
) {
  stopifnot(!is.null(startYear) || !is.null(timeUnits))

  if (is.null(timeUnits)) {
    timeUnits <- paste0("days since ", startYear, "-01-01 00:00:00")
  }

  if (!isTRUE(is.numeric(timeValues))) {
    timeValues <- RNetCDF::utinvcal.nc(
      timeUnits,
      value = as.POSIXct(timeValues)
    )
  }

  setAxisNCSW(
    x,
    nameAxis = nameAxis,
    dataType = dataType,
    values = timeValues,
    isUnlimitedDim = isUnlimitedDim,
    axis = "T",
    long_name = nameAxis,
    units = timeUnits,
    attributes = c(
      standard_name = nameAxis,
      calendar = as.character(calendar[[1L]])
    )
  )
}


#' @rdname setAxisNCSW
#'
#' @return [setAxisMonthClimatologyNCSW()] adds a monthly climatology dimension
#' and associated coordinate variable.
#'
#' @param startYear An integer value. The first calendar year of the period
#' which the climatology represents.
#' @param endYear An integer value. The last calendar year of the period
#' which the climatology represents.
#'
#' @export
setAxisMonthClimatologyNCSW <- function(
  x,
  startYear,
  endYear,
  nameAxis = "time",
  dataType = "NC_DOUBLE"
) {
  dataType <- ncDataType(dataType[[1L]])

  ox <- openRnetCDF(x, write = TRUE)
  xnc <- ox[["con"]]
  if (ox[["closeOnExit"]]) on.exit(RNetCDF::close.nc(xnc))


  time_unit <- paste0("days since ", startYear, "-01-01 00:00:00")

  # mid-month
  tv <- RNetCDF::utinvcal.nc(
    time_unit,
    value = as.POSIXct(
      seq(
        from = as.Date(paste0(startYear, "-01-16")),
        to = as.Date(paste0(startYear, "-12-16")),
        by = "month"
      )
    )
  )

  #--- Create time
  setAxisTimeNCSW(
    xnc,
    startYear = startYear,
    timeValues = tv,
    nameAxis = nameAxis,
    dataType = dataType
  )


  #--- Create bounds
  cbnds <- rbind(
    start = RNetCDF::utinvcal.nc(
      time_unit,
      value = as.POSIXct(
        as.Date(paste0(startYear, "-", seq_len(12L), "-01"))
      )
    ),
    end = RNetCDF::utinvcal.nc(
      time_unit,
      value = as.POSIXct(
        as.Date(
          c(
            paste0(endYear, "-", seq_len(12L)[-1L], "-01"),
            paste0(endYear + 1L, "-01-01")
          )
        ) - 1L
      )
    )
  )

  setAxisBoundsNCSW(
    xnc,
    nameBndsVar = "climatology_bounds",
    nameDim = nameAxis,
    valuesBnds = cbnds,
    boundsAttributeName = "climatology"
  )
}


#' Add a variable
#'
#' @inheritParams ncsw
#' @param varName A character string. The name of the variable.
#' see [`RNetCDF::var.put.nc()`] for more detail.
#' @param start A numeric vector. The start indices at which to write values,
#' see [`RNetCDF::var.put.nc()`] for more detail.
#' @param count A numeric vector. The counts of writing values,
#' see [`RNetCDF::var.put.nc()`] for more detail.
#' @param dimensions A vector, see [`RNetCDF::var.def.nc()`] for more detail.
#' @param deflate A numeric value or `NA`,
#' see [`RNetCDF::var.def.nc()`] for more detail.
#' @param addFillValue A logical value. Add a `"_FillValue"` attribute? The
#' value is determined by `dataType` and [fillValue()].
#'
#' @section Details:
#'   1. Create variable of `dataType` and `dimensions` if not present,
#'      see [`RNetCDF::var.def.nc()`].
#'   2. Write values if `values` are not `NULL`
#'      (using `count` and `start` if provided), see [`RNetCDF::var.put.nc()`].
#'   3. Add attributes `"long_name"`, `"units"`, `"cell_method"`,
#'      `"coordinates"` and `"grid_mapping"` if provided.
#'      If `"long_name"` is not provided and there is no attribute with that
#'      name, then `varName` is used as value for `"long_name"`.
#'   4. Add additional attributes if provided.
#'
#' @export
setVariableNCSW <- function(
  x,
  varName,
  values = NULL,
  start = NA,
  count = NA,
  var_chunksizes_xyzt = NA,
  dataType = "NC_DOUBLE",
  dimensions = NULL,
  deflate = 5L,
  long_name = NULL,
  units = NULL,
  cell_method = NULL,
  coordinates = paste("latitude", "longitude"),
  grid_mapping = "crs",
  attributes = NULL,
  addFillValue = TRUE
) {
  if (length(varName) != 1L) {
    stop(
      "Process one variable at a time, currently n = ", length(varName),
      call. = FALSE
    )
  }
  varName <- as.character(varName[[1L]])
  dataType <- ncDataType(dataType[[1L]])

  ox <- openRnetCDF(x, write = TRUE)
  xnc <- ox[["con"]]
  if (ox[["closeOnExit"]]) on.exit(RNetCDF::close.nc(xnc))


  #--- Create variable
  res <- try(RNetCDF::var.inq.nc(xnc, varName), silent = TRUE)
  if (inherits(res, "try-error")) {
    doChunk <- !anyNA(var_chunksizes_xyzt)

    RNetCDF::var.def.nc(
      xnc,
      varname = varName,
      vartype = dataType,
      dimensions = dimensions,
      chunking = if (doChunk) TRUE else NA,
      chunksizes = if (doChunk) var_chunksizes_xyzt,
      deflate = deflate,
      shuffle = !anyNA(deflate)
    )

    if (isTRUE(addFillValue)) {
      RNetCDF::att.put.nc(
        xnc,
        variable = varName,
        name = "_FillValue",
        type = dataType,
        value = fillValue(dataType)
      )
    }
  }

  #--- Write values
  if (!is.null(values)) {
    RNetCDF::var.put.nc(
      xnc, variable = varName, data = values, start = start, count = count
    )
  }

  #--- Attributes
  tmp <- c(
    if (!is.null(long_name)) list(long_name = long_name),
    if (!is.null(units)) list(units = units),
    if (!is.null(coordinates)) list(coordinates = coordinates),
    if (!is.null(grid_mapping)) list(grid_mapping = grid_mapping),
    if (!is.null(cell_method)) list(cell_method = cell_method),
    if (!is.null(attributes)) as.list(attributes)
  )
  tmp <- tmp[unique(names(tmp))]
  tmp <- tmp[lengths(tmp) > 0L]

  for (k in seq_along(tmp)) {
    RNetCDF::att.put.nc(
      xnc,
      variable = varName, name = names(tmp)[[k]], type = "NC_CHAR",
      value = as.character(tmp[[k]])
    )
  }

  #--- long_name
  res <- try(RNetCDF::att.inq.nc(xnc, varName, "long_name"), silent = TRUE)
  if (inherits(res, "try-error")) {
    RNetCDF::att.put.nc(
      xnc,
      variable = varName, name = "long_name", type = "NC_CHAR",
      value = varName
    )
  }
}


#' Set global attributes of type character
#'
#' @inheritParams ncsw
#'
#' @examples
#' fn <- tempfile(fileext = ".nc")
#' nc <- RNetCDF::create.nc(fn)
#' setGlobalAttributesNCSW(nc, c(featureType = "timeSeries", frequency = "day"))
#' setGlobalAttributesNCSW(nc, c(source = "SOILWAT2v8.2.0"))
#'
#' RNetCDF::print.nc(nc)
#' RNetCDF::close.nc(nc)
#' unlink(fn)
#'
#' @export
setGlobalAttributesNCSW <- function(x, attributes) {

  ox <- openRnetCDF(x, write = TRUE)
  xnc <- ox[["con"]]
  if (ox[["closeOnExit"]]) on.exit(RNetCDF::close.nc(xnc))


  for (k in seq_along(attributes)) {
    RNetCDF::att.put.nc(
      xnc,
      variable = "NC_GLOBAL",
      name = names(attributes)[[k]],
      type = "NC_CHAR",
      value = attributes[[k]]
    )
  }
}

#' Remove global attributes
#'
#' @inheritParams ncsw
#'
#' @examples
#' fn <- tempfile(fileext = ".nc")
#' nc <- RNetCDF::create.nc(fn)
#' setGlobalAttributesNCSW(nc, c(featureType = "timeSeries", frequency = "day"))
#' deleteGlobalAttributesNCSW(nc, "featureType")
#'
#' RNetCDF::print.nc(nc)
#' RNetCDF::close.nc(nc)
#' unlink(fn)
#'
#' @export
deleteGlobalAttributesNCSW <- function(x, attributes) {

  ox <- openRnetCDF(x, write = TRUE)
  xnc <- ox[["con"]]
  if (ox[["closeOnExit"]]) on.exit(RNetCDF::close.nc(xnc))


  for (k in seq_along(attributes)) {
    try(
      RNetCDF::att.delete.nc(
        xnc,
        variable = "NC_GLOBAL",
        attribute = attributes[[k]]
      ),
      silent = TRUE
    )
  }
}
