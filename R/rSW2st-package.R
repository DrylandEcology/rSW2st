################################################################################
# rSW2space: Space-related tools for SOILWAT2/STEPWAT2 simulation experiments.
# Copyright (C) 2019 Daniel Schlaepfer, John Bradford, William Lauenroth,
#   Kyle Palmquist
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
################################################################################



#' Package \pkg{rSW2st}: Collection of general purpose functions that operate on
#' spatially and temporally explicit objects
#' to support \pkg{SOILWAT2} and \pkg{STEPWAT2} simulation experiments.
#'
#' @docType package
#' @name rSW2st
"_PACKAGE"


##------ Package level variables
rSW2_glovars <- new.env()


##------ Import from other packages
#' @importFrom stats aggregate coef complete.cases cor cov fitted median
#'   na.exclude na.omit predict quantile sd weighted.mean
#' @importFrom methods as
NULL
