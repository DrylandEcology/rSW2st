test_that("create_raster_from_variables", {
  r <- raster::raster(
    xmn = 0, xmx = 10,
    ymn = 0, ymx = 10,
    crs ="EPSG:4326",
    resolution = c(1, 1)
  )

  #--- Values are vector ---
  v1 <- 1:10
  xy <- as_points(
    0.5 + cbind(0:9, 0:9),
    crs = 4326,
    to_class = "sf"
  )

  rv1 <- create_raster_from_variables(
    data = v1,
    site_locations = xy,
    grid = r
  )

  expect_s4_class(rv1, "RasterLayer")

  # Extracted values are equal to input values
  expect_equal(raster::extract(rv1, xy), v1)

  # Resulting raster is of same kind as input grid
  expect_true(
    raster::compareRaster(
      rv1, r,
      extent = TRUE, rowcol = TRUE, crs = TRUE, res = TRUE, orig = TRUE,
      rotation = TRUE, values = FALSE
    )
  )


  #--- Multiple variables in data.frame
  v2 <- data.frame(a = 1:10, b = 101:110)

  rv2 <- create_raster_from_variables(
    data = v2,
    site_locations = xy,
    grid = r
  )

  expect_s4_class(rv2, "RasterBrick")

  # Extracted values are equal to input values
  expect_equal(raster::extract(rv2, xy), data.matrix(v2))

  # Resulting raster is of same kind as input grid
  expect_true(
    raster::compareRaster(
      rv2, r,
      extent = TRUE, rowcol = TRUE, crs = TRUE, res = TRUE, orig = TRUE,
      rotation = TRUE, values = FALSE
    )
  )

})
