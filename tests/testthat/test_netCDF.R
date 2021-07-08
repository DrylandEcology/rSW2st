
# gridded data (coordinate value pairs uniquely identify gridcell):
#   --> expanded xy(ztv) or collapsed s(ztv)

# point data (coordinate value pairs identify irregularly spaced points):
#   --> collapsed s(ztv); can be expanded (but mostly nonsensical) or rasterized


#------ Tests for `get_data_dims()` ------
test_that("get_data_dims", {
  check_data_dims <- function(x, check_na = TRUE, vars_zero = "nv") {
    expect_named(x, c("ns", "nx", "ny", "nz", "nt", "nv"))
    if (check_na) expect_false(anyNA(x))
    expect_true(x["nx"] > 0 && x["ny"] > 0 || x["ns"] > 0)
    for (var in vars_zero) {
      expect_equal(x[var], 0, ignore_attr = "names")
    }
  }


  #--- Check well-behaved arguments
  tmp <- get_data_dims("xyzt", c(a = 17, 15, c = 12, 100))
  check_data_dims(tmp)

  tmp <- get_data_dims("xyzt", c(17, 15, 12, 100))
  check_data_dims(tmp)

  tmp <- get_data_dims("xyt", c(17, 15, 100))
  check_data_dims(tmp, vars_zero = c("nz", "nv"))

  tmp <- get_data_dims("xyz", c(17, 15, 12))
  check_data_dims(tmp, vars_zero = c("nt", "nv"))

  tmp <- get_data_dims("xy", c(17, 15))
  check_data_dims(tmp, vars_zero = c("nz", "nt", "nv"))

  tmp <- get_data_dims("xy", c(17, 15, 3))
  check_data_dims(tmp, vars_zero = c("nz", "nt"))


  tmp <- get_data_dims("szt", c(17 * 15, 12, 100))
  check_data_dims(tmp)

  tmp <- get_data_dims("st", c(17 * 15, 100))
  check_data_dims(tmp, vars_zero = c("nz", "nv"))

  tmp <- get_data_dims("sz", c(17 * 15, 12))
  check_data_dims(tmp, vars_zero = c("nt", "nv"))

  tmp <- get_data_dims("s", c(17 * 15))
  check_data_dims(tmp, vars_zero = c("nz", "nt", "nv"))

  tmp <- get_data_dims("s", c(17 * 15, 3))
  check_data_dims(tmp, vars_zero = c("nz", "nt"))


  #--- Check mis-specified arguments
  tmp <- get_data_dims("xyzt", c(17, 15, 100))
  expect_true(anyNA(tmp))
  check_data_dims(tmp, check_na = FALSE)

  tmp <- get_data_dims("xyzt", c(17, 15, NA, 12))
  expect_true(anyNA(tmp))
  check_data_dims(tmp, check_na = FALSE)

  tmp <- get_data_dims("xyzt")
  expect_true(all(tmp == 0))

  tmp <- get_data_dims("xyzt", NA)
  expect_true(anyNA(tmp))

  expect_error(get_data_dims("bad"))
})



#------ Tests for `xy_from_grid()` ------
test_that("xy_from_grid", {
  gd <- c(nx = 120, ny = 45)
  crs_wgs84 <- "OGC:CRS84"
  res <- c(1, 1)

  #--- `grid` cases:
  list_grids <- list()

  # 1) raster object or a filename of a raster object;
  list_grids[["raster"]] <- raster::raster(
    xmn = 0.5, xmx = 0.5 + gd["nx"],
    ymn = 0.5, ymx = 0.5 + gd["ny"],
    crs = crs_wgs84,
    resolution = res
  )

  # 2) stars object
  list_grids[["stars"]] <- stars::st_as_stars(list_grids[["raster"]])

  # 3) an object for `as_points()` with full gridcell coverage
  list_grids[["df"]] <- raster::coordinates(list_grids[["raster"]])

  # 4) a list with vectors for all x values and all y values
  list_grids[["list1"]] <- list(
    x = sort(unique(list_grids[["df"]][, 1])),
    y = sort(unique(list_grids[["df"]][, 2]))
  )

  # 5) a list with vectors for all x values and all y values and resolution
  list_grids[["list2"]] <- c(list_grids[["list1"]], list(res = res))


  # Loop over grid cases
  res_grids <- list()
  for (kg in names(list_grids)) {
    res_grids[[kg]] <- xy_from_grid(
      list_grids[[kg]],
      crs = crs_wgs84,
      res = res
    )

    # Check: type of grid specification does not matter
    expect_equal(res_grids[[1]], res_grids[[kg]])
  }
})



#------ Tests for `convert_xyspace()` ------
test_that("convert_xyspace", {
  dd <- c(nx = 13, ny = 7, nz = 2, nt = 3)
  d0 <- c(3, 5)
  gd <- c(nx = dd[["nx"]] + 2 * d0[1], ny = dd[["ny"]] + 2 * d0[2])


  #--- grid with full xy-space
  crs_wgs84 <- "OGC:CRS84"
  grid <- raster::raster(
    xmn = 0.5, xmx = 0.5 + gd["nx"],
    ymn = 0.5, ymx = 0.5 + gd["ny"],
    crs = crs_wgs84,
    resolution = c(1, 1)
  )


  #--- `data`
  locations <- cbind(
    x = d0[1] + seq_len(dd["nx"]),
    y = rep(d0[2] + seq_len(dd["ny"]), each = dd["nx"])
  )
  n_loc <- c(ns = nrow(locations))

  # Create indices of three locations to check correct transfer of values
  loc_checks <- list(
    c(
      tmp <- c(gx = d0[1] + 1, gy = d0[2] + 3),
      loc = which(locations[, 1] == tmp[1] & locations[, 2] == tmp[2])
    ),
    c(
      tmp <- c(gx = d0[1] + dd[["nx"]] - 10, gy = d0[2] + dd[["ny"]] - 3),
      loc = which(locations[, 1] == tmp[1] & locations[, 2] == tmp[2])
    ),
    c(
      tmp <- c(gx = d0[1] + dd[["nx"]] - 1, gy = d0[2] + dd[["ny"]] - 1),
      loc = which(locations[, 1] == tmp[1] & locations[, 2] == tmp[2])
    )
  )


  #--- Create expanded test data
  xy_grid <- xy_from_grid(grid)
  xy_data <- locations
  # Create matrix indices so that they match the collapsed/sparse version
  ids_x <- sapply(xy_data[, 1], function(x) which.min(abs(xy_grid[[1]] - x)))
  ids_y <- sapply(xy_data[, 2], function(x) which.min(abs(xy_grid[[2]] - x)))

  tmp_full <- array(
    dim = c(lengths(xy_grid[c("x", "y")]), dd["nz"], dd["nt"])
  )
  ids11 <- cbind(ids_x, ids_y, 1, 1)
  tmp_full[ids11] <- seq_len(dd["nx"] * dd["ny"])
  tmp_full[cbind(ids_x, ids_y, 2, 1)] <- 1000 + tmp_full[ids11]
  tmp_full[cbind(ids_x, ids_y, 1, 2)] <- 1
  tmp_full[cbind(ids_x, ids_y, 2, 2)] <- 1000
  tmp_full[cbind(ids_x, ids_y, 1, 3)] <- - tmp_full[ids11]
  tmp_full[cbind(ids_x, ids_y, 2, 3)] <- -1000 - tmp_full[ids11]
  # Add some NAs
  for (k in seq_len(dd["nt"])) {
    for (t2 in 1:2) {
      k0 <- if (t2 == 1) {
        (k + 1) * dd[["nx"]] + 4 + 3:4
      } else {
        k * dd[["nx"]] + 4 + 3:4
      }
      idsk <- cbind(ids_x[k0], ids_y[k0], rep(seq_len(dd["nz"]), each = 2), k)
      tmp_full[idsk] <- NA
    }
  }

  #--- Create collapsed test data (but corresponding to expanded cases)
  tmp_sparse <- array(dim = c(dd["nx"] * dd["ny"], dd["nz"], dd["nt"]))
  tmp_sparse[, 1, 1] <- seq_len(dd["nx"] * dd["ny"])
  tmp_sparse[, 2, 1] <- 1000 + tmp_sparse[, 1, 1]
  tmp_sparse[, 1, 2] <- 1
  tmp_sparse[, 2, 2] <- 1000
  tmp_sparse[, 1, 3] <- - tmp_sparse[, 1, 1]
  tmp_sparse[, 2, 3] <- -1000 - tmp_sparse[, 1, 1]
  # Add some NAs
  for (k in seq_len(dd["nt"])) {
    tmp_sparse[(k + 1) * dd[["nx"]] + 4 + 3:4, , k] <- NA
    tmp_sparse[k * dd[["nx"]] + 4 + 3:4, , k] <- NA
  }

  if (FALSE) {
    # Visualize test data
    tmp <- data.frame(
      Var1 = seq_len(dd["nx"]),
      Var2 = rep(seq_len(dd["ny"]), each = dd["nx"]),
      value = tmp_sparse[, 2, 3]
    )
    ggplot2::ggplot(tmp) +
      ggplot2::geom_raster(ggplot2::aes(x = Var1, y = Var2, fill = value))
  }

  #--- Put all test data cases together in a list
  list_data <- list(
    zt = list(
      str = "zt",
      collapse_degen = FALSE,
      data = list(collapsed = tmp_sparse, expanded = tmp_full)
    ),
    z = list(
      str = "z",
      collapse_degen = FALSE,
      data = list(collapsed = tmp_sparse[, 1, ], expanded = tmp_full[, , 1, ])
    ),
    t = list(
      str = "t",
      collapse_degen = FALSE,
      data = list(collapsed = tmp_sparse[, , 1], expanded = tmp_full[, , , 1])
    ),
    v1nodegen = list(
      str = "",
      collapse_degen = TRUE,
      data = list(
        collapsed = as.vector(tmp_sparse[, 1, 1]),
        expanded = tmp_full[, , 1, 1]
      )
    ),
    v1wdegen = list(
      str = "",
      collapse_degen = FALSE,
      data = list(
        collapsed = as.matrix(as.vector(tmp_sparse[, 1, 1])),
        expanded = tmp_full[, , 1, ][, , 1, drop = FALSE]
      )
    ),
    v = list(
      str = "",
      collapse_degen = FALSE,
      data = list(collapsed = tmp_sparse[, 1, ], expanded = tmp_full[, , 1, ])
    )
  )



  #--- Loop over data structure cases

  #------ Expanding a collapsed xy-dimension to separate x and y dimensions
  for (kd in names(list_data)) {
    data_str_ref <- paste0("s", list_data[[kd]][["str"]])
    data_str_res <- paste0("xy", list_data[[kd]][["str"]])

    ref <- list_data[[kd]][["data"]][["collapsed"]]

    res <- convert_xyspace(
      grid = grid,
      data = ref,
      locations = locations,
      locations_crs = crs_wgs84,
      data_str = data_str_res, #TODO: this should be data_str_ref?
      direction = "expand"
    )

    # Check that equal to expanded reference data
    expect_equal(
      if (list_data[[kd]][["collapse_degen"]]) drop(res) else res,
      list_data[[kd]][["data"]][["expanded"]],
      ignore_attr = c("names", "dimnames")
    )

    # Check: same number of values as original data
    expect_equal(sum(!is.na(res)), sum(!is.na(ref)))

    # Check: data dimensions
    expect_equal(
      get_data_dims(data_str_res, dim(res))[c("nx", "ny")],
      gd
    )

    # Check: are values correct at the few sample locations
    for (kc in seq_along(loc_checks)) {
      if (data_str_res == "xyzt") {
        expect_equal(
          res[loc_checks[[kc]]["gx"], loc_checks[[kc]]["gy"], , ],
          ref[loc_checks[[kc]]["loc"], , ],
          ignore_attr = "names"
        )

      } else if (
        data_str_res %in% c("xyz", "xyt", "xy") &&
        !is.null(dim(ref))
      ) {
        expect_equal(
          res[loc_checks[[kc]]["gx"], loc_checks[[kc]]["gy"], ],
          ref[loc_checks[[kc]]["loc"], ],
          ignore_attr = "names"
        )

      } else {
        expect_equal(
          res[loc_checks[[kc]]["gx"], loc_checks[[kc]]["gy"], 1],
          ref[loc_checks[[kc]]["loc"]],
          ignore_attr = "names"
        )
      }
    }


    #------ Collapsing separate x and y dimensions into a collapsed xy-dimension
    # i.e., check that round-trip works correctly
    res2 <- convert_xyspace(
      grid = grid,
      data = res,
      locations = locations,
      locations_crs = crs_wgs84,
      data_str = data_str_res,
      direction = "collapse"
    )

    expect_equal(
      if (list_data[[kd]][["collapse_degen"]]) drop(res2) else res2,
      ref,
      ignore_attr = c("names", "dimnames")
    )
  }


  #------ Collapsing separate x and y dimensions into a collapsed xy-dimension
  for (kd in names(list_data)) {
    data_str_ref <- paste0("xy", list_data[[kd]][["str"]])
    data_str_res <- paste0("s", list_data[[kd]][["str"]])

    ref <- list_data[[kd]][["data"]][["expanded"]]

    res <- convert_xyspace(
      grid = grid,
      data = ref,
      locations = locations,
      locations_crs = crs_wgs84,
      data_str = data_str_ref,
      direction = "collapse"
    )

    # Check that equal to collapsed reference data
    expect_equal(
      if (list_data[[kd]][["collapse_degen"]]) drop(res) else res,
      list_data[[kd]][["data"]][["collapsed"]],
      ignore_attr = c("names", "dimnames")
    )

    # Check: same number of values as original data
    expect_equal(sum(!is.na(res)), sum(!is.na(ref)))

    # Check: data dimensions
    expect_equal(
      get_data_dims(data_str_res, dim(res))["ns"],
      n_loc
    )

    # Check: are values correct at the few sample locations
    for (kc in seq_along(loc_checks)) {
      if (data_str_res == "szt") {
        expect_equal(
          res[loc_checks[[kc]]["loc"], , ],
          ref[loc_checks[[kc]]["gx"], loc_checks[[kc]]["gy"], , ],
          ignore_attr = "names"
        )

      } else if (data_str_res %in% c("sz", "st", "s") && length(dim(ref)) > 2) {
        expect_equal(
          res[loc_checks[[kc]]["loc"], ],
          ref[loc_checks[[kc]]["gx"], loc_checks[[kc]]["gy"], ],
          ignore_attr = "names"
        )

      } else {
        expect_equal(
          res[loc_checks[[kc]]["loc"], 1],
          ref[loc_checks[[kc]]["gx"], loc_checks[[kc]]["gy"]],
          ignore_attr = "names"
        )
      }
    }


    #------ Expanding collapsed xy-dimension into separate x and y dimensions
    # i.e., check that round-trip works correctly
    res2 <- convert_xyspace(
      grid = grid,
      data = res,
      locations = locations,
      locations_crs = crs_wgs84,
      data_str = data_str_ref,  #TODO: this should be data_str_res?
      direction = "expand"
    )

    expect_equal(
      if (list_data[[kd]][["collapse_degen"]]) drop(res2) else res2,
      ref,
      ignore_attr = c("names", "dimnames")
    )
  }
})



#------ Tests for `read_netCDF()` ------
test_that("read_netCDF", {
  tmp_methods <- c("array", "raster", "stars")

  #--- Create netCDFs to check reading
  tmp_nc <- create_example_netCDFs(
    path = tempdir(),
    data_str = c("xyzt", "xyt", "xyz", "xy", "szt", "st", "sz", "s"),
    type_timeaxis = c("timeseries", "climatology"),
    overwrite = TRUE
  )


  # Loop through netCDFs and check reading
  for (k in seq_along(tmp_nc)) {
    fnc <- tmp_nc[[k]]

    # Test for gridded/discrete netCDF type
    expect_equal(
      is_netCDF_gridded(fnc, xy_names = c("x", "y")),
      substr(names(tmp_nc[k]), 1, 2) == "xy"
    )


    # Loop over methods
    for (km in tmp_methods) {

      if (km == "raster" && basename(fnc) == "nc_s.nc") {
        # It appears that the raster package does not handle this case
        next
      }

      res <- read_netCDF(fnc, km, var = "sine", xy_names = c("x", "y"))

      if (km == "array") {
        expect_type(res, "list")
        expect_true(inherits(res[["data"]], "array"))

        expect_named(
          res,
          c(
            "data", "data_str", "type_timeaxis", "crs", "xyspace",
            "vertical_values", "vertical_bounds",
            "time_values", "time_bounds",
            paste0(
              c("var", "xy", "crs", "time", "vertical", "global"),
              "_attributes"
            ),
            if (grepl("\\<nc_s[[:alpha:]]{0,3}(|-clim).nc", basename(fnc))) {
              "site"
            }
          )
        )

        expect_true(length(dim(res[["data"]])) > 0)

        expect_null(
          read_netCDF(
            fnc, km, var = "sine", xy_names = c("x", "y"),
            load_values = FALSE
          )[["data"]]
        )

      } else if (km == "raster") {
        expect_s4_class(res, "RasterLayer")
        expect_true(length(dim(res)) > 0)

      } else if (km == "stars") {
        expect_s3_class(res, "stars")
        expect_true(length(dim(res)) > 0)
      }

      res_crs <- read_crs_from_netCDF(fnc)
      expect_s3_class(res_crs, "crs")
      expect_false(res_crs == sf::NA_crs_)

      res_atts <- read_attributes_from_netCDF(
        fnc,
        group = "all",
        var = "sine",
        xy_names = c("x", "y"),
        nc_name_crs = "crs"
      )
      expect_true(inherits(res_atts, "list"))
      expect_named(
        res_atts,
        paste0(
          c("var", "xy", "crs", "time", "vertical", "global"),
          "_attributes"
        )
      )
    }
  }


  unlink(unlist(tmp_nc))
})
