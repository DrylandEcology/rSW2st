
test_that("gridcell areas", {
  #--- Geographical WGS84
  r1 <- terra::rast(
    xmin = 0, xmax = 1,
    ymin = -90, ymax = 90,
    crs = "OGC:CRS84",
    resolution = c(1, 1),
    vals = 1:180
  )
  xy <- terra::spatSample(
    r1,
    size = terra::ncell(r1),
    method = "regular",
    as.points = TRUE
  )

  ## Calculate area for a subset of cells in grid
  cell_areas1a <- calculate_cell_area(xy, grid = r1)

  expect_identical(
    calculate_cell_area(sf::st_as_sf(xy), grid = r1),
    cell_areas1a
  )

  expect_equal(
    calculate_cell_area(xy, grid = stars::st_as_stars(r1), debug = TRUE),
    cell_areas1a,
    tolerance = 0.01
  )

  expect_equal(
    calculate_cell_area(sf::st_as_sf(xy), grid = stars::st_as_stars(r1)),
    cell_areas1a,
    tolerance = 0.01
  )


  ## Calculate area for all cells in grid
  cell_areas1b <- calculate_cell_area(grid = r1)

  expect_equal(
    calculate_cell_area(grid = stars::st_as_stars(r1)),
    cell_areas1b,
    tolerance = 0.01
  )


  expect_identical(
    as.matrix(unique(cell_areas1a)),
    as.matrix(unique(cell_areas1b))
  )
  expect_equal(max(cell_areas1a[, "rel"]), 1., tolerance = 0.0001)


  #--- USA Contiguous Albers Equal Area Conic USGS version
  r2 <- terra::rast(
    xmin = -2480000, xmax = 90000,
    ymin = 650000, ymax = 4020000,
    crs = "EPSG:6350",
    resolution = c(10000, 10000),
    vals = 1:86609
  )

  xy <- terra::spatSample(
    r2,
    size = 200L,
    method = "regular",
    as.points = TRUE
  )


  ## Calculate area for a subset of cells in grid
  cell_areas2a <- calculate_cell_area(xy, grid = r2)

  expect_equal(
    calculate_cell_area(sf::st_as_sf(xy), grid = stars::st_as_stars(r2)),
    cell_areas2a,
    tolerance = 0.01
  )

  expect_equal(
    cell_areas2a[, "km2"],
    rep(100, nrow(cell_areas2a)),
    tolerance = 0.01
  )
  expect_identical(cell_areas2a[, "rel"], rep(NA_real_, nrow(cell_areas2a)))
})


test_that("nominal resolution", {
  #--- Geographic CRS
  tests <- data.frame(
    res = c(1 / 24, 1 / 16, 1 / 2, 1, 2),
    nr = c("5 km", "10 km", "50 km", "100 km", "250 km"),
    stringsAsFactors = FALSE
  )

  for (k in seq_len(nrow(tests))) {
    r1 <- terra::rast(
      xmin = -120, xmax = -90,
      ymin = 30, ymax = 50,
      crs = "OGC:CRS84",
      resolution = rep(tests[k, "res"], 2L)
    )
    n <- min(200L, terra::ncell(r1))
    xy <- suppressWarnings(terra::spatSample(r1, size = n, as.points = TRUE))
    r1[xy] <- 1L


    expect_identical(
      calculate_nominal_resolution(r1),
      tests[k, "nr"]
    )

    expect_identical(
      calculate_nominal_resolution(stars::st_as_stars(r1)),
      tests[k, "nr"]
    )
  }


  #--- Projected CRS: CONUS Albers Equal Area (USGS)
  tests <- data.frame(
    res = c(1e3, 1e4, 2e4, 1e5, 5e5),
    nr = c("1 km", "10 km", "25 km", "100 km", "500 km"),
    stringsAsFactors = FALSE
  )

  for (k in seq_len(nrow(tests))) {
    # Warning message: In showSRID(SRS_string, format = "PROJ", multiline =
    # "NO", prefer_proj = prefer_proj) : Discarded datum NAD83 (National Spatial
    # Reference System 2011) in Proj4 definition
    r2 <- terra::rast(
      xmin = -1000000, xmax = 0,
      ymin = 650000, ymax = 1000000,
      crs = "EPSG:6350",
      resolution = rep(tests[k, "res"], 2L)
    )
    n <- min(200L, terra::ncell(r2))
    xy <- suppressWarnings(terra::spatSample(r2, size = n, as.points = TRUE))
    r2[xy] <- 1L

    expect_identical(
      calculate_nominal_resolution(r2),
      tests[k, "nr"]
    )

    expect_identical(
      calculate_nominal_resolution(stars::st_as_stars(r2)),
      tests[k, "nr"]
    )
  }
})
