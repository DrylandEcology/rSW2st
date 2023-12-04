test_that("as_points", {
  #--- Check conversions from 2-dim numerical matrix to sv/sp/sf/sfc ------
  locations <- matrix(
    data = c(-120.325, -111.245, 39.855, 36.753),
    nrow = 2L
  )

  pts_sf <- as_points(locations, crs = 4326, to_class = "sf")
  expect_s3_class(pts_sf, "sf")
  expect_equal(sf::st_coordinates(pts_sf), locations, ignore_attr = "dimnames")

  pts_sfc <- as_points(locations, crs = 4326, to_class = "sfc")
  expect_s3_class(pts_sfc, "sfc")
  expect_equal(sf::st_coordinates(pts_sfc), locations, ignore_attr = "dimnames")

  if (requireNamespace("sp")) {
    pts_sp <- as_points(locations, crs = 4326, to_class = "sp")
    expect_s4_class(pts_sp, "SpatialPoints")
    expect_equal(sp::coordinates(pts_sp), locations, ignore_attr = "dimnames")
  }

  pts_sv <- as_points(locations, crs = 4326, to_class = "sv")
  expect_s4_class(pts_sv, "SpatVector")
  expect_equal(terra::crds(pts_sv), locations, ignore_attr = "dimnames")


  #--- Check conversions from 2-dim numerical data.frame to sv/sp/sf/sfc ------
  locations_df <- as.data.frame(locations)

  pts_df_sf <- as_points(locations_df, crs = 4326, to_class = "sf")
  expect_s3_class(pts_df_sf, "sf")
  expect_equal(
    sf::st_coordinates(pts_df_sf),
    locations,
    ignore_attr = "dimnames"
  )
  # nolint start: expect_comparison_linter.
  expect_true(sf::st_crs(pts_df_sf) == sf::st_crs(pts_sf))
  # nolint end

  pts_df_sfc <- as_points(locations_df, crs = 4326, to_class = "sfc")
  expect_s3_class(pts_df_sfc, "sfc")
  expect_equal(
    sf::st_coordinates(pts_df_sfc),
    locations,
    ignore_attr = "dimnames"
  )
  # nolint start: expect_comparison_linter.
  expect_true(sf::st_crs(pts_df_sfc) == sf::st_crs(pts_sfc))
  # nolint end

  if (requireNamespace("sp")) {
    pts_df_sp <- as_points(locations_df, crs = 4326, to_class = "sp")
    expect_s4_class(pts_df_sp, "SpatialPoints")
    expect_equal(
      sp::coordinates(pts_df_sp),
      locations,
      ignore_attr = "dimnames"
    )
  }

  pts_df_sv <- as_points(locations_df, crs = 4326, to_class = "sv")
  expect_s4_class(pts_df_sv, "SpatVector")
  expect_equal(terra::crds(pts_df_sv), locations, ignore_attr = "dimnames")


  #--- Check conversions from sv -> sp/sf/sfc ------
  if (requireNamespace("sp")) {
    pts_sv_sp <- as_points(pts_sv, to_class = "sp")
    expect_s4_class(pts_sv_sp, "SpatialPoints")
    expect_equal(
      sp::coordinates(pts_sv_sp),
      locations,
      ignore_attr = "dimnames"
    )
    # nolint start: expect_comparison_linter.
    expect_true(sf::st_crs(pts_sv_sp) == sf::st_crs(pts_sf))
    expect_true(sf::st_crs(pts_sv_sp) == sf::st_crs(pts_sv))
    # nolint end
  }

  pts_sv_sf <- as_points(pts_sv, to_class = "sf")
  expect_s3_class(pts_sv_sf, "sf")
  expect_equal(
    sf::st_coordinates(pts_sv_sf),
    locations,
    ignore_attr = "dimnames"
  )
  # nolint start: expect_comparison_linter.
  expect_true(sf::st_crs(pts_sv_sf) == sf::st_crs(pts_sf))
  expect_true(sf::st_crs(pts_sv_sf) == sf::st_crs(pts_sv))
  # nolint end

  pts_sv_sfc <- as_points(pts_sv, to_class = "sfc")
  expect_s3_class(pts_sv_sfc, "sfc")
  expect_equal(
    sf::st_coordinates(pts_sv_sfc),
    locations,
    ignore_attr = "dimnames"
  )
  # nolint start: expect_comparison_linter.
  expect_true(sf::st_crs(pts_sv_sfc) == sf::st_crs(pts_sfc))
  expect_true(sf::st_crs(pts_sv_sfc) == sf::st_crs(pts_sv))
  # nolint end


  #--- Check conversions from sp -> sv/sf/sfc ------
  if (requireNamespace("sp")) {
    pts_sp_sv <- as_points(pts_sp, to_class = "sv")
    expect_s4_class(pts_sp_sv, "SpatVector")
    expect_equal(terra::crds(pts_sp_sv), locations, ignore_attr = "dimnames")
    # nolint start: expect_comparison_linter.
    expect_true(sf::st_crs(pts_sp_sv) == sf::st_crs(pts_sf))
    expect_true(sf::st_crs(pts_sp_sv) == sf::st_crs(pts_sp))
    # nolint end

    pts_sp_sf <- as_points(pts_sp, to_class = "sf")
    expect_s3_class(pts_sp_sf, "sf")
    expect_equal(
      sf::st_coordinates(pts_sp_sf),
      locations,
      ignore_attr = "dimnames"
    )
    # nolint start: expect_comparison_linter.
    expect_true(sf::st_crs(pts_sp_sf) == sf::st_crs(pts_sf))
    expect_true(sf::st_crs(pts_sp_sf) == sf::st_crs(pts_sp))
    # nolint end

    pts_sp_sfc <- as_points(pts_sp, to_class = "sfc")
    expect_s3_class(pts_sp_sfc, "sfc")
    expect_equal(
      sf::st_coordinates(pts_sp_sfc),
      locations,
      ignore_attr = "dimnames"
    )
    # nolint start: expect_comparison_linter.
    expect_true(sf::st_crs(pts_sp_sfc) == sf::st_crs(pts_sfc))
    expect_true(sf::st_crs(pts_sp_sfc) == sf::st_crs(pts_sp))
    # nolint end
  }


  #--- Check conversions from sfc -> sv/sp/sf ------
  pts_sfc_sv <- as_points(pts_sfc, to_class = "sv")
  expect_s4_class(pts_sfc_sv, "SpatVector")
  expect_equal(terra::crds(pts_sfc_sv), locations, ignore_attr = "dimnames")
  # nolint start: expect_comparison_linter.
  expect_true(sf::st_crs(pts_sfc_sv) == sf::st_crs(pts_sp))
  expect_true(sf::st_crs(pts_sfc_sv) == sf::st_crs(pts_sfc))
  # nolint end

  if (requireNamespace("sp")) {
    pts_sfc_sp <- as_points(pts_sfc, to_class = "sp")
    expect_s4_class(pts_sfc_sp, "SpatialPoints")
    expect_equal(
      sp::coordinates(pts_sfc_sp),
      locations,
      ignore_attr = "dimnames"
    )
    # nolint start: expect_comparison_linter.
    expect_true(sf::st_crs(pts_sfc_sp) == sf::st_crs(pts_sp))
    expect_true(sf::st_crs(pts_sfc_sp) == sf::st_crs(pts_sfc))
    # nolint end
  }

  pts_sfc_sf <- as_points(pts_sfc, to_class = "sf")
  expect_s3_class(pts_sfc_sf, "sf")
  expect_equal(
    sf::st_coordinates(pts_sfc_sf),
    locations,
    ignore_attr = "dimnames"
  )
  # nolint start: expect_comparison_linter.
  expect_true(sf::st_crs(pts_sfc_sf) == sf::st_crs(pts_sf))
  expect_true(sf::st_crs(pts_sfc_sf) == sf::st_crs(pts_sfc))
  # nolint end



  #--- Check conversions from sf -> sv/sp/sfc ------
  pts_sf_sv <- as_points(pts_sf, to_class = "sv")
  expect_s4_class(pts_sf_sv, "SpatVector")
  expect_equal(terra::crds(pts_sf_sv), locations, ignore_attr = "dimnames")
  # nolint start: expect_comparison_linter.
  expect_true(sf::st_crs(pts_sf_sv) == sf::st_crs(pts_sp))
  expect_true(sf::st_crs(pts_sf_sv) == sf::st_crs(pts_sf))
  # nolint end

  if (requireNamespace("sp")) {
    pts_sf_sp <- as_points(pts_sf, to_class = "sp")
    expect_s4_class(pts_sf_sp, "SpatialPoints")
    expect_equal(
      sp::coordinates(pts_sf_sp),
      locations,
      ignore_attr = "dimnames"
    )
    # nolint start: expect_comparison_linter.
    expect_true(sf::st_crs(pts_sf_sp) == sf::st_crs(pts_sp))
    expect_true(sf::st_crs(pts_sf_sp) == sf::st_crs(pts_sf))
    # nolint end
  }

  pts_sf_sfc <- as_points(pts_sf, to_class = "sfc")
  expect_s3_class(pts_sf_sfc, "sfc")
  expect_equal(
    sf::st_coordinates(pts_sf_sfc),
    locations,
    ignore_attr = "dimnames"
  )
  # nolint start: expect_comparison_linter.
  expect_true(sf::st_crs(pts_sf_sfc) == sf::st_crs(pts_sfc))
  expect_true(sf::st_crs(pts_sf_sfc) == sf::st_crs(pts_sf))
  # nolint end


  #--- Check conversions from one-dimensional numerical vector ------
  # A vector of length two is interpreted as a single point location
  pts_sf1 <- as_points(locations[1, ], crs = 4326, to_class = "sf")
  expect_s3_class(pts_sf1, "sf")
  expect_equal(
    sf::st_coordinates(pts_sf1),
    locations[1, , drop = FALSE],
    ignore_attr = "dimnames"
  )

  pts_sv1 <- as_points(locations[1, ], crs = 4326, to_class = "sv")
  expect_s4_class(pts_sv1, "SpatVector")
  expect_equal(
    terra::crds(pts_sv1),
    locations[1, , drop = FALSE],
    ignore_attr = "dimnames"
  )

  # Errors: longer/shorter vectors fail
  expect_error(as_points(1, crs = 4326, to_class = "sf"))
  expect_error(as_points(c(1, 2, 3), crs = 4326, to_class = "sf"))
  expect_error(as_points(1, crs = 4326, to_class = "sv"))
  expect_error(as_points(c(1, 2, 3), crs = 4326, to_class = "sv"))
  if (requireNamespace("sp")) {
    expect_error(as_points(1, crs = 4326, to_class = "sp"))
    expect_error(as_points(c(1, 2, 3), crs = 4326, to_class = "sp"))
  }

  # Errors: missing crs
  expect_error(as_points(locations, to_class = "sf"))
})
