#------ Tests for `ncDataType()` ------
test_that("ncDataType", {
  types <- c(
    "SHORT",
    "NC_SHORT",
    "INT",
    "INTEGER",
    "NC_INT",
    "FLOAT",
    "NC_FLOAT",
    "DOUBLE",
    "NC_DOUBLE",
    "BYTE",
    "NC_BYTE"
  )

  for (type in types) {
    expect_no_condition(ncDataType(type))
  }

  expect_error(ncDataType("noType"))
  expect_no_condition(ncDataType("noType", stopOnError = FALSE))
})


#------ Tests for `fillValue()` ------
test_that("fillValue", {
  types <- c(
    "SHORT",
    "NC_SHORT",
    "NC_FILL_SHORT",
    "INT",
    "INTEGER",
    "NC_INT",
    "NC_FILL_INT",
    "FLOAT",
    "NC_FLOAT",
    "NC_FILL_FLOAT",
    "DOUBLE",
    "NC_DOUBLE",
    "NC_FILL_DOUBLE",
    "BYTE",
    "NC_BYTE",
    "NC_FILL_BYTE"
  )

  for (type in types) {
    expect_no_condition(fillValue(type))
  }

  expect_error(fillValue("noType"))
})


#------ Tests for `uniqueDifferences()` ------
test_that("uniqueDifferences", {
  x <- seq(0, 1, by = 1 / 24)
  expect_length(uniqueDifferences(x), 1L)

  x[[2L]] <- x[[2L]] + sqrt(.Machine[["double.eps"]])
  expect_error(uniqueDifferences(x))
  expect_length(uniqueDifferences(x, tolerance = 1e-7), 1L)
  expect_length(uniqueDifferences(x, allowMultiple = TRUE), 3L)
})


#------ Tests that manipulate netCDFs for SOILWAT2 (nc- and mpi-mode) ------
test_that("manipulateNCforSOILWAT2", {
  dataStr <- c("xy", "xyt", "xyz", "xyzt")
  nameDimX <- "easting"
  nameDimY <- "northing"
  nameBndsX <- "x_bnds"
  nameBndsY <- "y_bnds"
  nameDimZ <- "vertical"
  nameDimT <- "time"

  tmpout_nc <- lapply(
    stats::setNames(nm = dataStr),
    tempfile,
    fileext = ".nc"
  )

  tmpin_nc <- create_example_netCDFs(
    path = tempdir(),
    data_str = dataStr,
    type_timeaxis = "timeseries",
    overwrite = TRUE
  )

  tmpnc <- RNetCDF::open.nc(tmpin_nc[["xyz"]])
  verticalValues <- RNetCDF::var.get.nc(tmpnc, "vertical")
  RNetCDF::close.nc(tmpnc)

  tmpnc <- RNetCDF::open.nc(tmpin_nc[["xyt"]])
  timeValues <- RNetCDF::var.get.nc(tmpnc, "time")
  RNetCDF::close.nc(tmpnc)

  globalAttributes <- c(featureType = "timeSeries", frequency = "day")
  deleteGlobalAttributes <- c("created_by", "created_date", "date")

  for (ds in dataStr) {
    unlink(tmpout_nc[[ds]])

    hasTime <- grepl("t", ds, fixed = TRUE)
    hasVertical <- grepl("z", ds, fixed = TRUE)
    isXYZT <- identical(ds, "xyzt")

    xin <- terra::rast(tmpin_nc[[ds]])

    #--- Create netCDF
    xtmp <- if (isXYZT) {
      # Remove ZT which we will add afterwards separately
      xtmp <- xin[[1L]]
      terra::varnames(xtmp) <- paste0(terra::varnames(xtmp), "0")
      terra::time(xtmp) <- NULL
      xtmp
    } else {
      xin
    }

    suppressWarnings(
      # nolint start: commented_code_linter.
      # ignore warning on some configurations:
      # "GDAL Message 1: dimension #1 (easting) is not a Longitude/X dimension."
      # nolint end: commented_code_linter.
      writeTerraToNCSW(
        x = xtmp,
        filename = tmpout_nc[[ds]],
        nameDimX = nameDimX,
        nameDimY = nameDimY,
        addSpatialBounds = FALSE,
        nameAxisVertical = nameDimZ,
        verticalValues = if (hasVertical && !isXYZT) verticalValues,
        nameAxisTime = nameDimT,
        timeValues = if (hasTime && !isXYZT) timeValues,
        deleteGlobalAttributes = deleteGlobalAttributes
      )
    )

    #--- Check that file exists
    expect_true(file.exists(tmpout_nc[[ds]]))
    xnc <- RNetCDF::open.nc(tmpout_nc[[ds]], write = TRUE)

    #--- Add spatial bounds
    expect_no_condition(
      setSpatialBoundsNCSW(
        xnc,
        nameDimX = nameDimX,
        nameDimY = nameDimY,
        nameBndsX = nameBndsX,
        nameBndsY = nameBndsY
      )
    )

    #--- Add global attributes
    expect_no_condition(
      setGlobalAttributesNCSW(xnc, globalAttributes)
    )

    #--- Do more stuff if xyzt
    if (isXYZT) {
      #--- Add vertical dimension
      expect_no_condition(
        setAxisVerticalNCSW(
          xnc,
          nameAxis = nameDimZ,
          verticalValues = verticalValues
        )
      )

      #--- Add time dimension
      expect_no_condition(
        setAxisTimeNCSW(
          xnc,
          nameAxis = nameDimT,
          startYear = 1900,
          timeValues = timeValues
        )
      )

      #--- Create new variable with vertical and time dimensions
      expect_no_condition(
        setVariableNCSW(
          xnc,
          varName = terra::varnames(xin),
          long_name = terra::longnames(xin),
          dimensions = c(nameDimT, nameDimZ, nameDimX, nameDimY),
          units = "1",
          coordinates = c(nameDimX, nameDimY),
          values = NULL
        )
      )

      #--- Successively add values to new variable
      kk <- 1L
      for (kv in seq_along(verticalValues)) {
        for (kt in seq_along(timeValues)) {
          expect_no_condition(
            setVariableNCSW(
              xnc,
              varName = terra::varnames(xin),
              values = as.vector(terra::values(xin[[kk]])),
              start = c(kt, kv, 1L, 1L),
              count = c(1L, 1L, NA, NA)
            )
          )
          kk <- kk + 1L
        }
      }
    }

    #--- Check spatial structure
    expect_no_condition(
      checkSpatialNCSW(
        xnc,
        nameDimX = nameDimX,
        nameDimY = nameDimY,
        expectedSpatialDims = dim(xin)[2L:1L],
        expectedSpatialExtent = c(
          xmin = terra::xmin(xin),
          xmax = terra::xmax(xin),
          ymin = terra::ymin(xin),
          ymax = terra::ymax(xin)
        )
      )
    )

    #--- Check that all expected variables exist
    varList <- c(
      terra::varnames(xin),
      nameDimX,
      nameDimY,
      nameBndsX,
      nameBndsY,
      if (hasVertical) nameDimZ,
      if (hasTime) nameDimT
    )

    for (k in seq_along(varList)) {
      expect_identical(
        RNetCDF::var.inq.nc(xnc, varList[[k]])[["name"]],
        varList[[k]]
      )
    }

    #--- Check that expected global attributes exist
    for (k in seq_along(globalAttributes)) {
      attName <- names(globalAttributes)[[k]]
      expect_identical(
        RNetCDF::att.inq.nc(xnc, "NC_GLOBAL", attName)[["name"]],
        attName
      )
    }

    #--- Check that deleted global attributes do not exist
    for (k in seq_along(deleteGlobalAttributes)) {
      attName <- deleteGlobalAttributes[[k]]
      expect_s3_class(
        try(RNetCDF::att.inq.nc(xnc, "NC_GLOBAL", attName), silent = TRUE),
        "try-error"
      )
    }

    RNetCDF::close.nc(xnc)
  }

  #--- Clean up
  unlink(unlist(tmpin_nc))
  unlink(unlist(tmpout_nc))
})


test_that("writeTerraToNCSW: latitude order", {
  skip_if_not_installed("ncdf4")

  # Rows from north to south have values 30, 20, 10
  r <- terra::rast(
    nrows = 3L,
    ncols = 2L,
    xmin = 0,
    xmax = 2,
    ymin = 0,
    ymax = 3,
    crs = "EPSG:4326",
    vals = rep(c(30, 20, 10), each = 2L)
  )
  terra::varnames(r) <- "v"

  for (increasingLat in c(TRUE, FALSE)) {
    tmp_nc <- tempfile(fileext = ".nc")

    suppressWarnings(
      writeTerraToNCSW(
        x = r,
        filename = tmp_nc,
        increasingLat = increasingLat,
        addSpatialBounds = FALSE
      )
    )

    xnc <- RNetCDF::open.nc(tmp_nc)
    lat <- as.vector(RNetCDF::var.get.nc(xnc, "latitude"))
    vals <- RNetCDF::var.get.nc(xnc, "v", collapse = FALSE)
    RNetCDF::close.nc(xnc)
    unlink(tmp_nc)

    expect_identical(
      lat,
      if (increasingLat) c(0.5, 1.5, 2.5) else c(2.5, 1.5, 0.5)
    )

    # Values match latitudes: value = 10 * (latitude + 0.5)
    for (k in seq_along(lat)) {
      expect_identical(unique(vals[, k]), 10 * (lat[[k]] + 0.5))
    }
  }
})


test_that("writeTerraToNCSW: round trips from GeoTIFF and netCDF", {
  skip_if_not_installed("ncdf4")

  tol <- sqrt(.Machine[["double.eps"]])

  # Write values to a netCDF with RNetCDF only
  # `vals`: matrix with rows along `lon` and columns along `lat`
  writeRNetCDF <- function(filename, lon, lat, vals) {
    xnc <- RNetCDF::create.nc(filename)
    on.exit(RNetCDF::close.nc(xnc))
    RNetCDF::dim.def.nc(xnc, "longitude", length(lon))
    RNetCDF::dim.def.nc(xnc, "latitude", length(lat))
    RNetCDF::var.def.nc(xnc, "longitude", "NC_DOUBLE", "longitude")
    RNetCDF::var.def.nc(xnc, "latitude", "NC_DOUBLE", "latitude")
    RNetCDF::var.def.nc(xnc, "v", "NC_DOUBLE", c("longitude", "latitude"))
    RNetCDF::att.put.nc(xnc, "longitude", "units", "NC_CHAR", "degrees_east")
    RNetCDF::att.put.nc(xnc, "latitude", "units", "NC_CHAR", "degrees_north")
    RNetCDF::att.put.nc(xnc, "v", "grid_mapping", "NC_CHAR", "crs")
    RNetCDF::var.put.nc(xnc, "longitude", lon)
    RNetCDF::var.put.nc(xnc, "latitude", lat)
    RNetCDF::var.put.nc(xnc, "v", vals)
    setCRSWGS84NCSW(xnc, nameCRS = "crs")
  }

  # Reference: `lat` from north to south,
  # `vals` with rows along `lon` and columns along `lat`
  describeRef <- function(lon, lat, vals) {
    dx <- abs(lon[[2L]] - lon[[1L]])
    dy <- abs(lat[[2L]] - lat[[1L]])
    list(
      lon = lon,
      lat = lat,
      vals = vals,
      ext = c(
        min(lon) - dx / 2,
        max(lon) + dx / 2,
        min(lat) - dy / 2,
        max(lat) + dy / 2
      )
    )
  }

  #--- netCDF inputs: each cell has a distinct value
  lon <- c(0.5, 1.5)
  lat <- c(2.5, 1.5, 0.5) # north to south
  vals <- matrix(1:6 + 0.5, nrow = length(lon), ncol = length(lat))

  #--- GeoTIFF input
  ftif <- system.file("ex", "elev.tif", package = "terra")
  rtif <- terra::rast(ftif)

  #--- Input files and their references
  fin <- c(
    tif = ftif,
    ncDecreasingLat = tempfile(fileext = ".nc"),
    ncIncreasingLat = tempfile(fileext = ".nc")
  )

  writeRNetCDF(fin[["ncDecreasingLat"]], lon = lon, lat = lat, vals = vals)
  writeRNetCDF(
    fin[["ncIncreasingLat"]],
    lon = lon,
    lat = rev(lat),
    vals = vals[, rev(seq_along(lat))]
  )

  refs <- list(
    tif = describeRef(
      lon = terra::xFromCol(rtif, seq_len(terra::ncol(rtif))),
      lat = terra::yFromRow(rtif, seq_len(terra::nrow(rtif))),
      vals = matrix(terra::values(rtif), nrow = terra::ncol(rtif))
    ),
    ncDecreasingLat = describeRef(lon = lon, lat = lat, vals = vals),
    ncIncreasingLat = describeRef(lon = lon, lat = lat, vals = vals)
  )

  #--- Read input files, write with writeTerraToNCSW(), and compare
  for (k in seq_along(fin)) {
    ref <- refs[[k]]

    xin <- terra::rast(fin[[k]])
    terra::varnames(xin) <- "v"

    # Input is read correctly (terra values are row-wise from north-west)
    expect_equal(
      as.vector(terra::values(xin)),
      as.vector(ref[["vals"]]),
      tolerance = tol
    )
    expect_equal(
      as.vector(terra::ext(xin)),
      ref[["ext"]],
      tolerance = tol,
      ignore_attr = TRUE
    )

    for (increasingLat in c(TRUE, FALSE)) {
      info <- paste(names(fin)[[k]], "increasingLat =", increasingLat)
      tmp_nc <- tempfile(fileext = ".nc")

      suppressWarnings(
        writeTerraToNCSW(
          x = xin,
          filename = tmp_nc,
          increasingLat = increasingLat,
          addSpatialBounds = FALSE
        )
      )

      # Latitude order and values match latitudes in the netCDF
      xnc <- RNetCDF::open.nc(tmp_nc)
      latOut <- as.vector(RNetCDF::var.get.nc(xnc, "latitude"))
      lonOut <- as.vector(RNetCDF::var.get.nc(xnc, "longitude"))
      valsOut <- RNetCDF::var.get.nc(xnc, "v", collapse = FALSE)
      RNetCDF::close.nc(xnc)

      ids <- seq_along(ref[["lat"]])
      if (increasingLat) {
        ids <- rev(ids)
      }

      expect_equal(lonOut, ref[["lon"]], tolerance = tol, info = info)
      expect_equal(latOut, ref[["lat"]][ids], tolerance = tol, info = info)
      expect_equal(
        unname(valsOut),
        ref[["vals"]][, ids],
        tolerance = tol,
        info = info
      )

      # Round trip: terra reads the same raster
      xout <- terra::rast(tmp_nc)
      expect_equal(
        as.vector(terra::values(xout)),
        as.vector(ref[["vals"]]),
        tolerance = tol,
        info = info
      )
      expect_equal(
        as.vector(terra::ext(xout)),
        ref[["ext"]],
        tolerance = tol,
        ignore_attr = TRUE,
        info = info
      )
      expect_true(terra::same.crs(xout, "EPSG:4326"), info = info)

      unlink(tmp_nc)
    }
  }

  unlink(fin[c("ncDecreasingLat", "ncIncreasingLat")])
})
