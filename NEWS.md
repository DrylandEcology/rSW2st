# rSW2st v0.3.1-devel

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
