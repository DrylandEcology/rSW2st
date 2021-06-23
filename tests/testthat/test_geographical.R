
test_that("gridcell areas", {
  #--- Geographical WGS84
  r1 <- raster::raster(
    xmn = 0, xmx = 1,
    ymn = -90, ymx = 90,
    crs = "OGC:CRS84",
    resolution = c(1, 1)
  )

  n <- prod(dim(r1))
  r1[] <- seq_len(n)
  xy <- raster::sampleRegular(r1, size = n, sp = TRUE)

  ## Calculate area for a subset of cells in grid
  cell_areas1a <- calculate_cell_area(xy, grid = r1)

  expect_equal(
    calculate_cell_area(sf::st_as_sf(xy), grid = r1),
    cell_areas1a
  )

  expect_equal(
    calculate_cell_area(xy, grid = stars::st_as_stars(r1)),
    cell_areas1a
  )

  expect_equal(
    calculate_cell_area(sf::st_as_sf(xy), grid = stars::st_as_stars(r1)),
    cell_areas1a
  )


  ## Calculate are for all cells in grid
  cell_areas1b <- calculate_cell_area(grid = r1)

  expect_equal(
    calculate_cell_area(grid = stars::st_as_stars(r1)),
    cell_areas1b
  )


  expect_equal(as.matrix(unique(cell_areas1a)), as.matrix(unique(cell_areas1b)))
  expect_equal(max(cell_areas1a[, "rel"]), 1)


  #--- USA Contiguous Albers Equal Area Conic USGS version
  r2 <- suppressWarnings(raster::raster(
    xmn = -2480000, xmx = 90000,
    ymn = 650000, ymx = 4020000,
    crs = "EPSG:6350",
    resolution = c(10000, 10000)
  ))

  n <- prod(dim(r2))
  r2[] <- seq_len(n)
  xy <- raster::sampleRegular(r2, size = 200, sp = TRUE)

  ## Calculate area for a subset of cells in grid
  cell_areas2a <- calculate_cell_area(xy, grid = r2)

  expect_equal(
    calculate_cell_area(sf::st_as_sf(xy), grid = stars::st_as_stars(r2)),
    cell_areas2a
  )

  expect_equal(cell_areas2a[, "km2"], rep(100, nrow(cell_areas2a)))
  expect_equal(cell_areas2a[, "rel"], rep(1, nrow(cell_areas2a)))
})


test_that("nominal resolution", {
  #--- Geographic CRS
  tests <- data.frame(
    res = c(1 / 24, 1 / 16, 1 / 2, 1, 2),
    nr = c("5 km", "10 km", "50 km", "100 km", "250 km")
  )

  for (k in seq_len(nrow(tests))) {
    r1 <- raster::raster(
      xmn = -110, xmx = -100,
      ymn = 30, ymx = 40,
      crs = "OGC:CRS84",
      resolution = rep(tests[k, "res"], 2)
    )
    ext <- raster::cellsFromExtent(r1, raster::extent(-107, -102, 32, 38))
    r1[ext] <- 1

    expect_equal(
      calculate_nominal_resolution(r1),
      tests[k, "nr"]
    )

    expect_equal(
      calculate_nominal_resolution(stars::st_as_stars(r1)),
      tests[k, "nr"]
    )
  }


  #--- Projected CRS: CONUS Albers Equal Area (USGS)
  tests <- data.frame(
    res = c(1e3, 1e4, 2e4, 1e5, 5e5),
    nr = c("1 km", "10 km", "25 km", "100 km", "500 km")
  )

  for (k in seq_len(nrow(tests))) {
    # Warning message: In showSRID(SRS_string, format = "PROJ", multiline =
    # "NO", prefer_proj = prefer_proj) : Discarded datum NAD83 (National Spatial
    # Reference System 2011) in Proj4 definition
    r2 <- suppressWarnings(raster::raster(
      xmn = -1000000, xmx = 0,
      ymn = 650000, ymx = 1000000,
      crs = "EPSG:6350",
      resolution = rep(tests[k, "res"], 2)
    ))
    ext <- raster::cellsFromExtent(
      r2,
      raster::extent(-900000, -10000, 700000, 900000)
    )
    r2[ext] <- 1

    expect_equal(
      calculate_nominal_resolution(r2),
      tests[k, "nr"]
    )

    expect_equal(
      calculate_nominal_resolution(stars::st_as_stars(r2)),
      tests[k, "nr"]
    )
  }
})
