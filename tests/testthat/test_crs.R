context("coordinate reference system")

test_that("crs", {

  # Attempt to handle GDAL3+/PROJ6+ warnings
  has_thin_PROJ6_warnings <-
    requireNamespace("rgdal") &&
    getNamespaceVersion("rgdal") >= as.numeric_version("1.5.8")

  if (has_thin_PROJ6_warnings) {
    prev_warnings <- rgdal::get_thin_PROJ6_warnings()
    rgdal::set_thin_PROJ6_warnings(TRUE)
  }

  #--- Input test cases
  test_crs <- data.frame(
    epsg = c(
      WGS84 = 4326,
      Web_PseudoMercator = 3857,
      # NAD27(USA) was 2163; however, it was deprecated by https://epsg.org
      # with change id "2020.020" and replaced by 9311.
      # Yet, 9311 is not yet available as of
      # rgdal v1.5.16 / GDAL 3.1.1 / PROJ 6.3.1
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
    )
  )

  locs <- matrix(c(1, 1), ncol = 2)
  r <- raster::raster(nrows = 1, ncols = 1, xmn = 0, xmx = 1)


  #--- Run checks
  for (k in seq_len(nrow(test_crs))) {
    epsg <- test_crs[k, "epsg"]
    txt_epsg <- paste0("EPSG:", epsg)
    proj4_epsg <- paste0("+init=", txt_epsg)

    expected_class <- "crs"
    expected_unit <- test_crs[k, "units"]

    raster::crs(r) <- epsg

    #--- Retrieve crs
    expect_s3_class(sf::st_crs(epsg), expected_class)
    expect_s3_class(sf::st_crs(txt_epsg), expected_class)
    expect_s3_class(sf::st_crs(sp::CRS(proj4_epsg)), expected_class)

    expect_s3_class(
      sf::st_crs(convert_points(locs, "sp", crs = proj4_epsg)),
      expected_class
    )
    expect_s3_class(
      sf::st_crs(convert_points(locs, "sf", crs = epsg)),
      expected_class
    )
    expect_s3_class(
      sf::st_crs(sf::st_as_sf(convert_points(locs, "sf", crs = epsg))),
      expected_class
    )

    expect_s3_class(sf::st_crs(r), expected_class)



    #--- Determine crs units
    expect_equal(crs_units(epsg), expected_unit)
    expect_equal(crs_units(txt_epsg), expected_unit)
    expect_equal(crs_units(sp::CRS(proj4_epsg)), expected_unit)

    expect_equal(
      crs_units(convert_points(locs, "sp", crs = proj4_epsg)),
      expected_unit
    )
    expect_equal(
      crs_units(convert_points(locs, "sf", crs = epsg)),
      expected_unit
    )
    expect_equal(
      crs_units(sf::st_as_sf(convert_points(locs, "sf", crs = epsg))),
      expected_unit
    )

    expect_equal(crs_units(r), expected_unit)
  }

  # Clean up
  if (has_thin_PROJ6_warnings) {
    rgdal::set_thin_PROJ6_warnings(prev_warnings)
  }
})
