# rSW2st v0.2.0-9000
* Github Actions are triggered for `release/**` branches in addition to `main`.
* `r-lib` Github Actions updated to `v2`.
* `get_data_dims()` now returns consistently an integer vector.
* Linting updated to `lintr` >= 3 and
  lint workflow switched from package tests to Github Action (#5).
* `isoline_from_raster()` is now based on the `stars` and `sf` packages
  (removing a dependency on `rgeos`; addressing #3; @dschlaep).

# rSW2st v0.1.0
Initial release
