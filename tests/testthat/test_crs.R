test_that("crs", {

  #--- Input test cases
  test_crs <- data.frame(
    epsg = c(
      WGS84 = 4326,
      Web_PseudoMercator = 3857,
      # NAD27(USA) was 2163; however, it was deprecated by https://epsg.org
      # with change id "2020.020" and replaced by 9311.
      # Yet, 9311 is not yet available as of
      # GDAL 3.1.1 / PROJ 6.3.1
      NAD27_USA = 2163,
      NAD83_USA = 4269,
      NAD83_2011_USA = 6318
    ),
    units = c(
      WGS84 = "degree",
      Web_PseudoMercator = "metre",
      NAD27_USA = "metre",
      NAD83_USA = "degree",
      NAD83_2011_USA = "degree"
    ),
    stringsAsFactors = FALSE
  )

  locs <- matrix(c(1, 1), ncol = 2)
  r <- raster::raster(nrows = 1, ncols = 1, xmn = 0, xmx = 1)


  #--- Run checks
  for (k in seq_len(nrow(test_crs))) {
    epsg <- test_crs[k, "epsg"]
    txt_epsg <- paste0("EPSG:", epsg)

    expected_class <- "crs"
    expected_unit <- test_crs[k, "units"]

    raster::crs(r) <- epsg

    #--- Retrieve crs
    expect_s3_class(sf::st_crs(epsg), expected_class)
    expect_s3_class(sf::st_crs(txt_epsg), expected_class)

    tmp_spCRS <- sp::CRS(SRS_string = txt_epsg)
    expect_s3_class(sf::st_crs(tmp_spCRS), expected_class)


    expect_s3_class(
      sf::st_crs(as_points(locs, "sp", crs = epsg)),
      expected_class
    )
    expect_s3_class(
      sf::st_crs(as_points(locs, "sf", crs = epsg)),
      expected_class
    )
    expect_s3_class(
      sf::st_crs(sf::st_as_sf(as_points(locs, "sf", crs = epsg))),
      expected_class
    )

    expect_s3_class(sf::st_crs(r), expected_class)



    #--- Determine crs units
    expect_identical(crs_units(epsg), expected_unit)
    expect_identical(crs_units(txt_epsg), expected_unit)

    expect_identical(
      # nolint start: commented_code_linter.
      # See #22: `crs_units(sp::CRS(SRS_string = txt_epsg))` returns NA
      # during package checks with `devtools::check()`
      # while returning a correct unit text string with `devtools::test()`
      # nolint end: commented_code_linter.
      crs_units(as(sf::st_crs(txt_epsg), "CRS")),
      expected_unit
    )

    expect_identical(
      crs_units(sf::st_crs(txt_epsg)),
      expected_unit
    )

    expect_identical(
      crs_units(as_points(locs, "sp", crs = epsg)),
      expected_unit
    )
    expect_identical(
      crs_units(as_points(locs, "sf", crs = epsg)),
      expected_unit
    )
    expect_identical(
      crs_units(as_points(locs, "sf", crs = epsg)),
      expected_unit
    )

    expect_identical(crs_units(r), expected_unit)
  }
})



test_that("UTM", {
  tmp <- c(-120, -100, -90, 0, 15, 90, 135, 150)

  locations <- data.frame(
    longitude = rep(tmp, each = 2L),
    latitude = rep(c(-40, 40), times = length(tmp)),
    expected_utm_zone = c(
      11L, 11L,
      14L, 14L,
      16L, 16L,
      31L, 31L,
      33L, 33L,
      46L, 46L,
      53L, 53L,
      56L, 56L
    ),
    expected_utm_epsg = c(
      32711, 32611,
      32714, 32614,
      32716, 32616,
      32731, 32631,
      32733, 32633,
      32746, 32646,
      32753, 32653,
      32756, 32656
    )
  )

  for (k in seq_len(nrow(locations))) {
    expect_identical(
      utm_zone(locations[k, 1:2])[["utm_zone"]],
      locations[k, "expected_utm_zone"]
    )

    expect_identical(
      epsg_for_utm(locations[k, 1:2]),
      locations[k, "expected_utm_epsg"]
    )
  }
})
