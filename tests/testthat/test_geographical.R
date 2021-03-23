
test_that("gridcell areas", {
  #--- Geographical WGS84
  r1 <- raster::raster(
    xmn = 0, xmx = 1,
    ymn = -90, ymx = 90,
    crs ="EPSG:4326",
    resolution = c(1, 1)
  )

  n <- prod(dim(r1))
  r1[] <- seq_len(n)
  xy <- raster::sampleRegular(r1, size = n, sp = TRUE)

  ## Calculate area for a subset of cells in grid
  cell_areas1a <- calculate_cell_area(xy, grid = r1)

  ## Calculate are for all cells in grid
  cell_areas1b <- calculate_cell_area(grid = r1)

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

  expect_equal(cell_areas2a[, "km2"], rep(100, nrow(cell_areas2a)))
  expect_equal(cell_areas2a[, "rel"], rep(1, nrow(cell_areas2a)))
})
