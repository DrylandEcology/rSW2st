#!/usr/bin/env Rscript

#--- rSW2st: use development version
library("methods")  # in case this code is run via 'Rscript'
#stopifnot(requireNamespace("pkgbuild"))
stopifnot(requireNamespace("pkgload"))

#pkgbuild::clean_dll()
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
