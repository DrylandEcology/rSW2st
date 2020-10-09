context("conversions")


test_that("as_points", {
  #--- Check conversions from two-dimensional numerical matrix to sp/sf/sfc
  locations <- matrix(
    data = c(-120.325, -111.245, 39.855, 36.753),
    nrow = 2
  )

  pts_sf <- as_points(locations, to_class = "sf")
  expect_s3_class(pts_sf, "sf")
  expect_equal(sf::st_coordinates(pts_sf), locations, check.attributes = FALSE)

  pts_sfc <- as_points(locations, to_class = "sfc")
  expect_s3_class(pts_sfc, "sfc")
  expect_equal(
    sf::st_coordinates(pts_sfc),
    locations,
    check.attributes = FALSE
  )

  pts_sp <- as_points(locations, to_class = "sp")
  expect_s4_class(pts_sp, "SpatialPoints")
  expect_equal(sp::coordinates(pts_sp), locations, check.attributes = FALSE)


  #--- Check conversions from two-dimensional numerical data.frame to sp/sf/sfc
  locations_df <- as.data.frame(locations)

  pts_df_sf <- as_points(locations_df, to_class = "sf")
  expect_s3_class(pts_df_sf, "sf")
  expect_equal(
    sf::st_coordinates(pts_df_sf),
    locations,
    check.attributes = FALSE
  )
  expect_true(sf::st_crs(pts_df_sf) == sf::st_crs(pts_sf))

  pts_df_sfc <- as_points(locations_df, to_class = "sfc")
  expect_s3_class(pts_df_sfc, "sfc")
  expect_equal(
    sf::st_coordinates(pts_df_sfc),
    locations,
    check.attributes = FALSE
  )
  expect_true(sf::st_crs(pts_df_sfc) == sf::st_crs(pts_sfc))

  pts_df_sp <- as_points(locations_df, to_class = "sp")
  expect_s4_class(pts_df_sp, "SpatialPoints")
  expect_equal(sp::coordinates(pts_df_sp), locations, check.attributes = FALSE)



  #--- Check conversions from sp to sf/sfc
  pts_sp_sf <- as_points(pts_sp, to_class = "sf")
  expect_s3_class(pts_sp_sf, "sf")
  expect_equal(
    sf::st_coordinates(pts_sp_sf),
    locations,
    check.attributes = FALSE
  )
  expect_true(sf::st_crs(pts_sp_sf) == sf::st_crs(pts_sf))
  expect_true(sf::st_crs(pts_sp_sf) == sf::st_crs(pts_sp))

  pts_sp_sfc <- as_points(pts_sp, to_class = "sfc")
  expect_s3_class(pts_sp_sfc, "sfc")
  expect_equal(
    sf::st_coordinates(pts_sp_sfc),
    locations,
    check.attributes = FALSE
  )
  expect_true(sf::st_crs(pts_sp_sfc) == sf::st_crs(pts_sfc))
  expect_true(sf::st_crs(pts_sp_sfc) == sf::st_crs(pts_sp))



  #--- Check conversions from sfc -> sp/sf
  pts_sfc_sp <- as_points(pts_sfc, to_class = "sp")
  expect_s4_class(pts_sfc_sp, "SpatialPoints")
  expect_equal(sp::coordinates(pts_sfc_sp), locations, check.attributes = FALSE)
  expect_true(sf::st_crs(pts_sfc_sp) == sf::st_crs(pts_sp))
  expect_true(sf::st_crs(pts_sfc_sp) == sf::st_crs(pts_sfc))

  pts_sfc_sf <- as_points(pts_sfc, to_class = "sf")
  expect_s3_class(pts_sfc_sf, "sf")
  expect_equal(
    sf::st_coordinates(pts_sfc_sf),
    locations,
    check.attributes = FALSE
  )
  expect_true(sf::st_crs(pts_sfc_sf) == sf::st_crs(pts_sf))
  expect_true(sf::st_crs(pts_sfc_sf) == sf::st_crs(pts_sfc))



  #--- Check conversions from sf -> sp/sfc
  pts_sf_sp <- as_points(pts_sf, to_class = "sp")
  expect_s4_class(pts_sf_sp, "SpatialPoints")
  expect_equal(sp::coordinates(pts_sf_sp), locations, check.attributes = FALSE)
  expect_true(sf::st_crs(pts_sf_sp) == sf::st_crs(pts_sp))
  expect_true(sf::st_crs(pts_sf_sp) == sf::st_crs(pts_sf))

  pts_sf_sfc <- as_points(pts_sf, to_class = "sfc")
  expect_s3_class(pts_sf_sfc, "sfc")
  expect_equal(
    sf::st_coordinates(pts_sf_sfc),
    locations,
    check.attributes = FALSE
  )
  expect_true(sf::st_crs(pts_sf_sfc) == sf::st_crs(pts_sfc))
  expect_true(sf::st_crs(pts_sf_sfc) == sf::st_crs(pts_sf))



  #--- Check conversions from one-dimensional numerical vector
  # A vector of length two is interpreted as a single point location
  pts_sf1 <- as_points(locations[1, ], to_class = "sf")
  expect_s3_class(pts_sf1, "sf")
  expect_equal(
    sf::st_coordinates(pts_sf1),
    locations[1, ],
    check.attributes = FALSE
  )

  # Longer/shorter vectors fail
  expect_error(as_points(1, to_class = "sf"))
  expect_error(as_points(c(1, 2, 3), to_class = "sf"))
  expect_error(as_points(1, to_class = "sp"))
  expect_error(as_points(c(1, 2, 3), to_class = "sp"))
})
