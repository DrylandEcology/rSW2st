# rSW2st v0.3.3

* `setVariableNCSW()` now spells the `"cell_methods"` argument correctly.
  The previous, incorrect spelling `"cell_method"` is retained for
  backwards compatibility.
* `setVariableNCSW()` gains argument `shuffle` to control the shuffle filter;
  the default retains the previous behavior.

## Bug fixes
* `writeTerraToNCSW()` with `increasingLat = FALSE` no longer reverses
  latitude values relative to the data (previously, the map was written
  upside down).
* `create_netCDF()` (via `populate_netCDF()`) can again write several
  variables for data structures `"xy"` and `"s"`.
* `read_netCDF_as_array()` can again read several variables of
  data structure `"xy"`.
* `read_netCDF_as_array()` can again read subsets of the time and/or
  vertical axis (arguments `time_ids` and `vertical_ids`; previously,
  this failed with an error). Vertical values and bounds are now subset
  by `vertical_ids` instead of `time_ids`.
* `create_netCDF()` no longer fails if `nc_compression` is `TRUE` and
  `data_type` is the default or if `nc_chunks` is an integer vector.
  Argument `nc_shuffle` is now applied (previously, the shuffle filter was
  always on if compression was activated).
* `create_netCDF()` with `type_timeaxis = "climatology"` now sets only the
  `"climatology"` and no longer also a `"bounds"` attribute on the time
  variable (as required by CF conventions).
* `read_attributes_from_netCDF()` now correctly reports whether the time
  dimension is unlimited.
* `read_netCDF_as_raster()` now correctly checks whether `raster` has
  read a valid `crs` (previously, the check failed with an error).
* `calculate_cell_area()` now works if `grid` is a `SpatRaster` with
  cells that have missing values or if `x` is a single point.
* `convert_xyspace(direction = "expand")` now works if some `locations`
  fall outside the `grid`; their values are dropped (as documented by the
  warning).
* `setAxisMonthClimatologyNCSW()` now sets the upper climatology bounds to
  the first day of the following month of `endYear` (as required by CF
  conventions); previously, the last day of each month was excluded.
* `utm_zone()` now assigns locations at the equator (latitude 0) to the
  northern hemisphere.
* `epsg_for_utm()` now returns an integer value (as documented).
* `read_netCDF_as_terra()` now reads the requested variable `var` and
  passes on those arguments in `...` that `terra::rast()` accepts;
  it now accepts `"ncdf4"` objects and gives a clear error for
  `"NetCDF"` connections; setting the `crs` from the `netCDF` if `terra`
  did not locate one no longer fails with an error.
* `variogram_range()` no longer changes the random number state of the
  caller; `set.seed()` is called only if `seed` is not `NULL`.
* `read_attributes_from_netCDF(group = "all")` now works with non-default
  values of `time_name` and `vertical_name`.
* Functions that accept an `"ncdf4"` object now close it (with a warning)
  before re-opening the file with `RNetCDF`; previously, the `"ncdf4"`
  connection was kept open, and writing to a `netCDF-4` file failed.
* `setAxisNCSW()` now signals an error if `values` is `NULL` and
  `isUnlimitedDim` is `FALSE`; previously, an unlimited dimension was
  created.
* `setAxisBoundsNCSW()` now signals a clear error if `valuesBnds` is `NULL`
  and `calculateValuesBndsIfMissing` is `FALSE`.
* `setVariableNCSW()` now creates variables of type `"NC_INT64"` and
  `"NC_UINT64"` (previously, adding the `"_FillValue"` attribute failed);
  it adds no `"_FillValue"` attribute for these types (with a warning),
  and the default fill value of the `netCDF` library applies.
* `read_netCDF_as_array()` no longer drops data variables whose names contain
  a dimension name (e.g., `"lat.mean"`).
* `create_netCDF()` now checks that `data_dims` agree with site-based
  data structures `"szt"`, `"st"`, `"sz"`, and `"s"`.
* `create_raster_from_variables()` now converts a character matrix to numeric
  values and accepts a one-column `data.frame`.
* `calculate_nominal_resolution()` now returns the smallest class instead of
  `character(0)` if the mean resolution is exactly 0.


# rSW2st v0.3.2

* `create_netCDF*()` via internal `.populate_netCDF_nocheck()` now fix
  a degenerate data dimension.
* `read_netCDF_as_array()` now returns a named data object using variable names
  if multiple variables are present.


# rSW2st v0.3.1

* Code now uses functionality from the `"RNetCDF"` package
  replacing the use of the `"ncdf4"` package (#8; @dschlaep).
* Improved support for input and output workflows for `nc`-based SOILWAT2.


# rSW2st v0.3.0

* New `netCDF` functionality that supports input and output workflows for
  `nc`-based SOILWAT2 (#25; @dschlaep).
* `RNetCDF` is now an imported package.
* Updates to package infrastructure including Github Actions, spelling, linting,
  and a few miscellaneous patches to fix deprecated dependencies.


# rSW2st v0.2.1

* `rSW2st` no longer depends on but instead suggests
  `"sp"` and `"raster"` packages (#18; @dschlaep).
    * Functions that gained the ability to handle `"SpatRaster"`
      (`"terra"` package) objects:
        `get_xyspace()`, `calculate_cell_area()`,
        `calculate_nominal_resolution()`, `read_netCDF()`
    * `as_points()` gained the ability to handle
      `"SpatVector"` (`"terra"` package) objects.


# rSW2st v0.2.0

* Github Actions are triggered for `release/**` branches in addition to `main`.
* `r-lib` Github Actions updated to `v2` (#6; @dschlaep).
* `get_data_dims()` now returns consistently an integer vector.
* Linting updated to `lintr` >= 3 and
  lint workflow switched from package tests to Github Action (#5; @dschlaep).
* `isoline_from_raster()` is now based on the `stars` and `sf` packages
  (removing a dependency on `rgeos`; addressing #3; @dschlaep).
* `variogram_range()` is now based on the `stars` and `sf` packages
  (removing a dependency on `rgdal`; addressing #3; @dschlaep).
* `read_netCDF_as_array()` gains arguments `"time_name"` and `"vertical_name"`
  with previous hard-coded values as defaults ("time" and "vertical")
  (#14; @dschlaep)
* New `get_nc_type()` identifies a suitable `netCDF` data type from an R object.
* `read_netCDF()` gains argument `"verbose_read"` which, if set to `"FALSE"`,
  attempts to silence communication (messages, warnings, and print statements)
  generated from reading a `netCDF` (#9; @dschlaep).


# rSW2st v0.1.0
Initial release
