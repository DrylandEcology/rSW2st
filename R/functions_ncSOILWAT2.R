

#' `netCDF` functionality suitable for `SOILWAT2`
#'
#' @param x A character string (file name that will be opened and closed) or
#' an object of class `"NetCDF"` from the `RNetCDF` package
#' (an open connection to a `NetCDF` dataset that will be kept open).
#'
#' @param attributes A named vector or named list of character strings.
#'
#' @param dataType A character string. A `netCDF` data type.
#'
#' @name ncsw
NULL


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
    EXPR = toupper(dataType),

    SHORT = ,
    NC_SHORT = ,
    NC_FILL_SHORT = -32768,

    INT = ,
    INTEGER = ,
    NC_INT = ,
    NC_FILL_INT = -2147483647,

    FLOAT = ,
    NC_FLOAT = ,
    NC_FILL_FLOAT = ,
    DOUBLE = ,
    NC_DOUBLE = ,
    NC_FILL_DOUBLE = 9.9692099683868690e+36,

    BYTE = ,
    NC_BYTE = ,
    NC_FILL_BYTE = -127,

    stop(shQuote(dataType), " is not implemented.", call. = FALSE)
  )
}


#' Unique differences
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


#' Write a `terra` raster object to a `netCDF` file suitable for `SOILWAT2`
#'
#' @inheritParams ncsw
#'
#' @export
writeTerraToNCSW <- function(
  x,
  filename,
  var_chunksizes_xyzt = NA,
  dataType = c("double", "float", "integer", "short", "byte", "char"),
  increasingLat = TRUE,
  nameLon = "longitude",
  nameLat = "latitude",
  addSpatialBounds = TRUE,
  nameAxisVertical = "vertical",
  verticalValues = NULL,
  nameAxisTime = "time",
  timeValues = NULL,
  deleteGlobalAttributes = c("created_by", "created_date", "date")
) {

  dataType <- match.arg(dataType)

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

  if (getNamespaceVersion("terra") >= "1.8-42") {
    if (!is.null(nameAxisVertical) && !is.null(verticalValues)) {
      terra::depthName(x) <- nameAxisVertical
      terra::depth(x) <- verticalValues
    } else {
      terra::depthName(x) <- ""
      terra::depth(x) <- NULL
    }

    if (!is.null(nameAxisTime) && !is.null(timeValues)) {
      terra::time(x) <- timeValues
    } else {
      terra::time(x) <- NULL
    }

  } else {
    listArgsWriteCDF[["zname"]] <- if (is.null(nameAxisVertical)) {
      nameAxisTime
    } else {
      nameAxisVertical
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
  xlat <- RNetCDF::var.get.nc(xnc, variable = nameLat)
  RNetCDF::var.put.nc(xnc, variable = nameLat, data = rev(xlat))


  #--- longitude/latitude attributes
  RNetCDF::att.put.nc(xnc, nameLon, "axis", "NC_CHAR", value = "X")
  RNetCDF::att.put.nc(xnc, nameLon, "standard_name", "NC_CHAR", value = nameLon)

  RNetCDF::att.put.nc(xnc, nameLat, "axis", "NC_CHAR", value = "Y")
  RNetCDF::att.put.nc(xnc, nameLat, "standard_name", "NC_CHAR", value = nameLat)


  #--- Spatial bounds
  if (isTRUE(addSpatialBounds)) {
    setSpatialBoundsNCSW(xnc, nameDimX = nameLon, nameDimY = nameLat)
  }

  #--- Set CRS to WGS84
  if (isTRUE(sf::st_crs(x) == sf::st_crs("WGS84"))) {
    setCRSWGS84NCSW(xnc, nameCRS = "crs")
  }
}


#' Create a `CRS` variable with `WGS84` attributes
#'
#' @inheritParams ncsw
#'
#' @examples
#' fn <- tempfile(fileext = ".nc")
#' nc <- RNetCDF::create.nc(fn, format = "netcdf4")
#' setCRSWGS84NCSW(nc, nameCRS = "crs")
#'
#' RNetCDF::print.nc(nc)
#' RNetCDF::close.nc(nc)
#' unlink(fn)
#'
#' @export
setCRSWGS84NCSW <- function(x, nameCRS) {

  if (inherits(x, "NetCDF")) {
    xnc <- x
  } else if (inherits(x, "character")) {
    xnc <- RNetCDF::open.nc(x, write = TRUE)
    on.exit(RNetCDF::close.nc(xnc))
  } else {
    stop(
      "Class ", class(x), " not implemented for argument 'x'.",
      call. = FALSE
    )
  }

  #--- Create variable
  res <- try(RNetCDF::var.inq.nc(xnc, nameCRS), silent = TRUE)
  if (inherits(res, "try-error")) {
    RNetCDF::var.def.nc(
      xnc, varname = nameCRS, vartype = "NC_BYTE", dimensions = NA
    )
  }

  #--- Attributes
  RNetCDF::att.put.nc(
    xnc, nameCRS, "grid_mapping_name", "NC_CHAR",
    value = "latitude_longitude"
  )

  RNetCDF::att.put.nc(
    xnc, nameCRS, "long_name", "NC_CHAR",
    value = "WGS84"
  )

  RNetCDF::att.put.nc(
    xnc, nameCRS, "crs_wkt", "NC_CHAR",
    value = paste0(
      'GEOGCS["WGS 84",DATUM["WGS_1984",',
      'SPHEROID["WGS 84",6378137,298.257223563,',
      'AUTHORITY["EPSG","7030"]],AUTHORITY["EPSG","6326"]],',
      'PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],',
      'UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],',
      'AUTHORITY["EPSG","4326"]]'
    )
  )

  RNetCDF::att.put.nc(
    xnc, nameCRS, "longitude_of_prime_meridian", "NC_DOUBLE",
    value = 0.0
  )

  RNetCDF::att.put.nc(
    xnc, nameCRS, "semi_major_axis", "NC_DOUBLE",
    value = 6378137.0
  )

  RNetCDF::att.put.nc(
    xnc, nameCRS, "inverse_flattening", "NC_DOUBLE",
    value = 298.257223563
  )
}


#' Check spatial structure of a `netCDF`
#'
#' @inheritParams ncsw
#'
#' @export
checkSpatialNCSW <- function(
  x,
  nameDimX = "longitude",
  nameDimY = "latitude",
  nameCRS = "crs",
  expectedSpatialDims = c(NA, NA),
  expectedSpatialExtent = c(NA, NA, NA, NA),
  tolerance = sqrt(.Machine[["double.eps"]])
) {
  if (inherits(x, "NetCDF")) {
    xnc <- x
  } else if (inherits(x, "character")) {
    xnc <- RNetCDF::open.nc(x, write = TRUE)
    on.exit(RNetCDF::close.nc(xnc))
  } else {
    stop(
      "Class ", class(x), " not implemented for argument 'x'.",
      call. = FALSE
    )
  }

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
    resX <- abs(uniqueDifferences(valuesAxisX))
    valuesAxisY <- RNetCDF::var.get.nc(xnc, nameDimY)
    resY <- abs(uniqueDifferences(valuesAxisY))

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

  if (inherits(x, "NetCDF")) {
    xnc <- x
  } else if (inherits(x, "character")) {
    xnc <- RNetCDF::open.nc(x, write = TRUE)
    on.exit(RNetCDF::close.nc(xnc))
  } else {
    stop(
      "Class ", class(x), " not implemented for argument 'x'.",
      call. = FALSE
    )
  }

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


#' Set spatial bounds on x-axis and y-axis
#'
#' @inheritParams ncsw
#'
#' @export
setSpatialBoundsNCSW <- function(
  x,
  nameBnds = "bnds",
  nameBndsX = "lon_bnds",
  nameBndsY = "lat_bnds",
  nameDimX = "longitude",
  nameDimY = "latitude",
  valuesBndsX = NULL,
  valuesBndsY = NULL
) {

  if (inherits(x, "NetCDF")) {
    xnc <- x
  } else if (inherits(x, "character")) {
    xnc <- RNetCDF::open.nc(x, write = TRUE)
    on.exit(RNetCDF::close.nc(xnc))
  } else {
    stop(
      "Class ", class(x), " not implemented for argument 'x'.",
      call. = FALSE
    )
  }

  #--- Create dimension
  res <- try(RNetCDF::dim.inq.nc(xnc, nameBnds), silent = TRUE)
  if (inherits(res, "try-error")) {
    RNetCDF::dim.def.nc(xnc, nameBnds, dimlength = 2L)
  }

  #--- Create x-axis bounds
  res <- try(RNetCDF::var.inq.nc(xnc, nameBndsX), silent = TRUE)
  if (inherits(res, "try-error")) {
    RNetCDF::var.def.nc(
      xnc, nameBndsX, "NC_DOUBLE",
      dimensions = c(nameBnds, nameDimX),
      deflate = 5,
      shuffle = TRUE
    )

    if (is.null(valuesBndsX)) {
      valuesAxisX <- RNetCDF::var.get.nc(xnc, nameDimX)
      resX <- abs(uniqueDifferences(valuesAxisX))

      valuesBndsX <- apply(
        rbind(valuesAxisX - resX / 2, valuesAxisX + resX / 2),
        MARGIN = 2L,
        sort
      )
    }

    RNetCDF::var.put.nc(xnc, nameBndsX, data = valuesBndsX)
    RNetCDF::att.put.nc(xnc, nameDimX, "bounds", "NC_CHAR", value = nameBndsX)
  }


  #--- Create y-axis bounds
  res <- try(RNetCDF::var.inq.nc(xnc, nameBndsY), silent = TRUE)
  if (inherits(res, "try-error")) {
    RNetCDF::var.def.nc(
      xnc, nameBndsY, "NC_DOUBLE",
      dimensions = c(nameBnds, nameDimY),
      deflate = 5,
      shuffle = TRUE
    )

    if (is.null(valuesBndsY)) {
      valuesAxisY <- RNetCDF::var.get.nc(xnc, nameDimY)
      resY <- abs(uniqueDifferences(valuesAxisY))

      valuesBndsY <- apply(
        rbind(valuesAxisY + resY / 2, valuesAxisY - resY / 2),
        MARGIN = 2L,
        sort
      )
    }

    RNetCDF::var.put.nc(xnc, nameBndsY, data = valuesBndsY)
    RNetCDF::att.put.nc(xnc, nameDimY, "bounds", "NC_CHAR", value = nameBndsY)
  }
}


#' Add a vertical dimension and coordinate variable
#'
#' @inheritParams ncsw
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

  if (inherits(x, "NetCDF")) {
    xnc <- x
  } else if (inherits(x, "character")) {
    xnc <- RNetCDF::open.nc(x, write = TRUE)
    on.exit(RNetCDF::close.nc(xnc))
  } else {
    stop(
      "Class ", class(x), " not implemented for argument 'x'.",
      call. = FALSE
    )
  }

  #--- Create dimension
  res <- try(RNetCDF::dim.inq.nc(xnc, nameAxis), silent = TRUE)
  if (inherits(res, "try-error")) {
    RNetCDF::dim.def.nc(
      xnc, dimname = nameAxis, dimlength = length(verticalValues)
    )
  }

  #--- Create variable
  res <- try(RNetCDF::var.inq.nc(xnc, nameAxis), silent = TRUE)
  if (inherits(res, "try-error")) {
    RNetCDF::var.def.nc(
      xnc,
      varname = nameAxis, vartype = dataType, dimensions = nameAxis
    )
  }

  RNetCDF::var.put.nc(
    xnc,
    variable = nameAxis,
    data = switch(
      EXPR = verticalType,
      values = verticalValues,
      layers = seq_along(verticalValues)
    )
  )


  #--- Variable attributes
  RNetCDF::att.put.nc(xnc, nameAxis, "axis", "NC_CHAR", value = "Z")

  RNetCDF::att.put.nc(
    xnc, nameAxis, "standard_name", "NC_CHAR", value = "depth"
  )

  RNetCDF::att.put.nc(
    xnc,
    variable = nameAxis,
    name = "long_name",
    type = "NC_CHAR",
    value = switch(
      EXPR = verticalType,
      values = "soil depth",
      layers = "soil layer"
    )
  )

  RNetCDF::att.put.nc(xnc, nameAxis, "positive", "NC_CHAR", value = "down")

  if (!is.null(units) && identical(verticalType, "values")) {
    RNetCDF::att.put.nc(xnc, nameAxis, "units", "NC_CHAR", value = units)
  }

  if (identical(verticalType, "layers")) {
    RNetCDF::att.put.nc(
      xnc,
      variable = "vertical", name = "comment", type = "NC_CHAR",
      value = "dimensionless vertical coordinate"
    )
  }


  #--- Vertical bounds
  if (
    identical(verticalType, "values") &&
      !is.null(verticalUpperBound) && !is.null(verticalLowerBound)
  ) {
    RNetCDF::att.put.nc(
      xnc, nameAxis, "bounds", "NC_CHAR", value = "vertical_bnds"
    )

    res <- try(RNetCDF::var.inq.nc(xnc, "vertical_bnds"), silent = TRUE)
    if (inherits(res, "try-error")) {
      RNetCDF::var.def.nc(
        xnc, "vertical_bnds", "NC_DOUBLE",
        dimensions = c("bnds", nameAxis),
        deflate = 5,
        shuffle = TRUE
      )
    }

    vertical_bnds <- apply(
      rbind(verticalUpperBound, verticalLowerBound),
      MARGIN = 2L,
      sort
    )

    RNetCDF::var.put.nc(xnc, "vertical_bnds", data = vertical_bnds)
  }
}


#' Add a plant functional type dimension and coordinate variable
#'
#' @inheritParams ncsw
#'
#' @export
setAxisPFTsNCSW <- function(
  x,
  pfts = c(Tree = "Trees", Shrub = "Shrubs", Forb = "Forbs", Grass = "Grasses"),
  dimName = "pft",
  dataType = c("NC_STRING", "NC_BYTE")
) {
  dataType <- match.arg(dataType)

  if (inherits(x, "NetCDF")) {
    xnc <- x
  } else if (inherits(x, "character")) {
    xnc <- RNetCDF::open.nc(x, write = TRUE)
    on.exit(RNetCDF::close.nc(xnc))
  } else {
    stop(
      "Class ", class(x), " not implemented for argument 'x'.",
      call. = FALSE
    )
  }

  #--- Create dimension
  res <- try(RNetCDF::dim.inq.nc(xnc, dimName), silent = TRUE)
  if (inherits(res, "try-error")) {
    RNetCDF::dim.def.nc(
      xnc, dimname = dimName, dimlength = length(pfts)
    )
  }

  #--- Create variable
  res <- try(RNetCDF::var.inq.nc(xnc, dimName), silent = TRUE)
  if (inherits(res, "try-error")) {
    RNetCDF::var.def.nc(
      xnc,
      varname = dimName, vartype = dataType, dimensions = dimName
    )
  }

  if (identical(dataType, "NC_STRING")) {
    RNetCDF::var.put.nc(xnc, dimName, data = pfts)
  } else if (identical(dataType, "NC_BYTE")) {
    RNetCDF::var.put.nc(xnc, dimName, data = seq_along(pfts))
  }


  #--- Variable attributes
  RNetCDF::att.put.nc(
    xnc, dimName, "standard_name", "NC_CHAR", value = "biological_taxon_name"
  )

  if (identical(dataType, "NC_BYTE")) {
    RNetCDF::att.put.nc(
      xnc,
      variable = dimName,
      name = "flag_meanings",
      type = "NC_CHAR",
      value = paste(pfts, collapse = " ")
    )

    RNetCDF::att.put.nc(
      xnc, dimName, "flag_values", "NC_BYTE", value = seq_along(pfts)
    )
  }
}


#' Add a time dimension and coordinate variable
#'
#' @inheritParams ncsw
#'
#' @export
setAxisTimeNCSW <- function(
  x,
  startYear,
  timeValues,
  nameAxis = "time",
  calendar = c("standard", "365_day", "366_day"),
  dataType = "NC_DOUBLE"
) {
  calendar <- as.character(calendar[[1L]])
  dataType <- match.arg(dataType)

  if (inherits(x, "NetCDF")) {
    xnc <- x
  } else if (inherits(x, "character")) {
    xnc <- RNetCDF::open.nc(x, write = TRUE)
    on.exit(RNetCDF::close.nc(xnc))
  } else {
    stop(
      "Class ", class(x), " not implemented for argument 'x'.",
      call. = FALSE
    )
  }


  #--- Create dimension
  res <- try(RNetCDF::dim.inq.nc(xnc, nameAxis), silent = TRUE)
  if (inherits(res, "try-error")) {
    RNetCDF::dim.def.nc(
      xnc, dimname = nameAxis, dimlength = length(timeValues)
    )
  }

  #--- Create variable
  res <- try(RNetCDF::var.inq.nc(xnc, nameAxis), silent = TRUE)
  if (inherits(res, "try-error")) {
    RNetCDF::var.def.nc(
      xnc,
      varname = nameAxis, vartype = dataType, dimensions = nameAxis
    )
  }

  RNetCDF::var.put.nc(xnc, variable = nameAxis, data = timeValues)


  #--- Variable attributes
  RNetCDF::att.put.nc(
    xnc,
    variable = nameAxis, name = "units", type = "NC_CHAR",
    value = paste0("days since ", startYear, "-01-01 00:00:00")
  )
  RNetCDF::att.put.nc(
    xnc,
    variable = nameAxis, name = "long_name", type = "NC_CHAR",
    value = nameAxis
  )
  RNetCDF::att.put.nc(
    xnc,
    variable = nameAxis, name = "axis", type = "NC_CHAR",
    value = "T"
  )
  RNetCDF::att.put.nc(
    xnc,
    variable = nameAxis, name = "standard_name", type = "NC_CHAR",
    value = nameAxis
  )
  RNetCDF::att.put.nc(
    xnc,
    variable = nameAxis, name = "calendar", type = "NC_CHAR",
    value = calendar
  )
}


#' Add a monthly climatology dimension and coordinate variable
#'
#' @inheritParams ncsw
#'
#' @export
setAxisMonthClimatologyNCSW <- function(
  x,
  startYear,
  endYear,
  nameAxis = "time",
  dataType = "NC_DOUBLE"
) {

  if (inherits(x, "NetCDF")) {
    xnc <- x
  } else if (inherits(x, "character")) {
    xnc <- RNetCDF::open.nc(x, write = TRUE)
    on.exit(RNetCDF::close.nc(xnc))
  } else {
    stop(
      "Class ", class(x), " not implemented for argument 'x'.",
      call. = FALSE
    )
  }

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

  varName <- "climatology_bounds"
  RNetCDF::att.put.nc(
    xnc,
    variable = nameAxis, name = "climatology", type = "NC_CHAR",
    value = varName
  )

  #--- Create bounds dimension
  res <- try(RNetCDF::dim.inq.nc(xnc, "bnds"), silent = TRUE)
  if (
    inherits(res, "try-error") && grepl("Invalid dimension", res, fixed = TRUE)
  ) {
    RNetCDF::dim.def.nc(xnc, dimname = "bnds", dimlength = 2L)
  }

  #--- Create bounds variable
  res <- try(RNetCDF::var.inq.nc(xnc, varName), silent = TRUE)
  if (inherits(res, "try-error")) {
    RNetCDF::var.def.nc(
      xnc,
      varname = varName,
      vartype = dataType,
      dimensions = c("bnds", nameAxis)
    )
  }

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

  RNetCDF::var.put.nc(xnc, variable = varName, data = cbnds, count = c(2L, 12L))
}


#' Add a variable
#'
#' @inheritParams ncsw
#'
#' @section Details:
#'   1. Create variable of `dataType` and `dimensions` if not present.
#'   2. Write values if `values`, `count`, and optionally `start` are provided.
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
  start = NULL,
  count = NULL,
  var_chunksizes_xyzt = NA,
  dataType = c("NC_DOUBLE", "NC_FLOAT"),
  dimensions = NULL,
  deflate = 5L,
  long_name = NULL,
  units = NULL,
  cell_method = NULL,
  coordinates = paste("latitude", "longitude"),
  grid_mapping = "crs",
  attributes = NULL
) {
  varName <- as.character(varName[[1L]])

  if (inherits(x, "NetCDF")) {
    xnc <- x
  } else if (inherits(x, "character")) {
    xnc <- RNetCDF::open.nc(x, write = TRUE)
    on.exit(RNetCDF::close.nc(xnc))
  } else {
    stop(
      "Class ", class(x), " not implemented for argument 'x'.",
      call. = FALSE
    )
  }

  #--- Create variable
  res <- try(RNetCDF::var.inq.nc(xnc, varName), silent = TRUE)
  if (inherits(res, "try-error")) {
    dataType <- match.arg(dataType)
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

    RNetCDF::att.put.nc(
      xnc,
      variable = varName, name = "_FillValue", type = dataType,
      value = fillValue(dataType)
    )
  }

  #--- Write values
  if (!is.null(values) && !is.null(count)) {
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

  if (inherits(x, "NetCDF")) {
    xnc <- x
  } else if (inherits(x, "character")) {
    xnc <- RNetCDF::open.nc(x, write = TRUE)
    on.exit(RNetCDF::close.nc(xnc))
  } else {
    stop(
      "Class ", class(x), " not implemented for argument 'x'.",
      call. = FALSE
    )
  }

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

  if (inherits(x, "NetCDF")) {
    xnc <- x
  } else if (inherits(x, "character")) {
    xnc <- RNetCDF::open.nc(x, write = TRUE)
    on.exit(RNetCDF::close.nc(xnc))
  } else {
    stop(
      "Class ", class(x), " not implemented for argument 'x'.",
      call. = FALSE
    )
  }

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
