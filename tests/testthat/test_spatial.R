test_that("variogram_range", {
  rv1 <- terra::rast(
    xmin = 0, xmax = 10,
    ymin = 0, ymax = 10,
    crs = "EPSG:6350",
    resolution = c(1, 1)
  )

  xy <- as_points(
    0.5 + cbind(0:9, 0:9),
    crs = 6350,
    to_class = "sf"
  )

  ids <- terra::cellFromXY(rv1, sf::st_coordinates(xy))
  rv1[ids] <- 1L

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
