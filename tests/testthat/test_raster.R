test_that("create_raster_from_variables", {
  skip_if_not(requireNamespace("raster"))

  r <- raster::raster(
    xmn = 0, xmx = 10,
    ymn = 0, ymx = 10,
    crs = "OGC:CRS84",
    resolution = c(1, 1)
  )

  #--- Values are vector ---
  v1 <- as.numeric(1:10)
  xy <- as_points(
    0.5 + cbind(0:9, 0:9),
    crs = "OGC:CRS84",
    to_class = "sf"
  )

  rv1 <- create_raster_from_variables(
    data = v1,
    site_locations = xy,
    grid = r
  )

  expect_s4_class(rv1, "RasterLayer")

  # Extracted values are equal to input values
  expect_identical(raster::extract(rv1, xy), v1)

  # Resulting raster is of same kind as input grid
  expect_true(
    raster::compareRaster(
      rv1, r,
      extent = TRUE, rowcol = TRUE, crs = TRUE, res = TRUE, orig = TRUE,
      rotation = TRUE, values = FALSE
    )
  )


  #--- Multiple variables in data.frame
  v2 <- data.frame(a = as.numeric(1:10), b = as.numeric(101:110))

  rv2 <- create_raster_from_variables(
    data = v2,
    site_locations = xy,
    grid = r
  )

  expect_s4_class(rv2, "RasterBrick")

  # Extracted values are equal to input values
  expect_identical(raster::extract(rv2, xy), data.matrix(v2))

  # Resulting raster is of same kind as input grid
  expect_true(
    raster::compareRaster(
      rv2, r,
      extent = TRUE, rowcol = TRUE, crs = TRUE, res = TRUE, orig = TRUE,
      rotation = TRUE, values = FALSE
    )
  )
})



test_that("isoline_from_raster", {
  grid_size <- c(10L, 20L)
  grid_n <- prod(grid_size)

  grid_template <- stars::st_as_stars(
    data.frame(
      x = rep(seq_len(grid_size[[1L]]), times = grid_size[[2L]]),
      y = rep(rev(seq_len(grid_size[[2L]])), each = grid_size[[1L]]),
      value = seq_len(grid_n)
    ),
    dims = c("x", "y")
  )

  sf::st_crs(grid_template) <- "OGC:CRS84"


  types <- c("stars", "Raster")

  if (requireNamespace("terra", quietly = TRUE)) {
    types <- c(types, "SpatRaster")
  }

  for (type in types) {
    rs <- switch(
      EXPR = type,
      Raster = raster::raster(terra::rast(grid_template)),
      SpatRaster = terra::rast(grid_template),
      stars = stars::st_as_stars(grid_template)
    )


    #--- All grid values >= threshold
    threshold <- -5
    ip1 <- isoline_from_raster(rs, alpha = threshold)
    expect_s3_class(ip1, "sf")
    expect_gte(min(grid_template[ip1][[1L]], na.rm = TRUE), threshold)

    #--- Some grid values >= threshold
    threshold <- as.integer(grid_n / 2L)
    ip2 <- isoline_from_raster(rs, alpha = threshold)
    expect_s3_class(ip2, "sf")
    expect_gte(min(grid_template[ip2][[1L]], na.rm = TRUE), threshold)

    #--- No grid values >= threshold
    threshold <- grid_n + 10L
    ip3 <- isoline_from_raster(rs, alpha = threshold)
    expect_s3_class(ip3, "sf")
    expect_identical(nrow(ip3), 0L)
  }
})
