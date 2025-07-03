

#------ Tests for `fillValue()` ------
test_that("fillValue", {
  types <- c(
    "SHORT", "NC_SHORT", "NC_FILL_SHORT",
    "INT", "INTEGER", "NC_INT", "NC_FILL_INT",
    "FLOAT", "NC_FLOAT", "NC_FILL_FLOAT",
    "DOUBLE", "NC_DOUBLE", "NC_FILL_DOUBLE",
    "BYTE", "NC_BYTE", "NC_FILL_BYTE"
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

  x <- terra::rast(tmpin_nc[["xyzt"]])
  verticalValues <- unique(terra::depth(x))
  timeValues <- unique(terra::time(x))

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
      xtmp <- xin[[1L]]
      terra::varnames(xtmp) <- paste0(terra::varnames(xtmp), "0")
      xtmp
    } else {
      xin
    }

    expect_no_condition(
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
          xnc, nameAxis = nameDimZ, verticalValues = verticalValues
        )
      )

      #--- Add time dimension
      expect_no_condition(
        setAxisTimeNCSW(
          xnc, nameAxis = nameDimT, startYear = 1900, timeValues = timeValues
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
