test_that("variogram_range", {
  r <- suppressWarnings(raster::raster(
    xmn = 0, xmx = 10,
    ymn = 0, ymx = 10,
    crs = "EPSG:6350",
    resolution = c(1, 1)
  ))

  xy <- as_points(
    0.5 + cbind(0:9, 0:9),
    crs = 6350,
    to_class = "sf"
  )

  rv1 <- create_raster_from_variables(
    data = rep(1, 10),
    site_locations = xy,
    grid = r
  )

  expect_identical(
    variogram_range(x = rv1),
    variogram_range(x = xy)
  )

  expect_equal(
    variogram_range(x = rv1, sub_samplepoints_N = 5, seed = 2017),
    variogram_range(x = xy, sub_samplepoints_N = 5, seed = 2017),
    tolerance = 1e-6
  )
})
