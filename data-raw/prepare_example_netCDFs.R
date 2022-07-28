#!/usr/bin/env Rscript

#--- rSW2st: use development version
# load "methods" in case this code is run via 'Rscript'
library("methods") # nolint: undesirable_function_linter, unused_import_linter.
stopifnot(requireNamespace("pkgload"))

pkgload::load_all()


#--- INPUTS
dir_in <- file.path("inst", "extdata")
dir.create(dir_in, recursive = TRUE, showWarnings = TRUE)


#------ Create example \var{netCDFs}
# Currently, the plan is to use `create_example_netCDFs()` in the examples
# instead of using up c. 400 KB by extdata/
if (FALSE) {
  create_example_netCDFs(
    path = dir_in,
    data_str = c("xyzt", "szt"),
    type_timeaxis = "timeseries"
  )
}
