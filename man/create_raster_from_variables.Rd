% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_raster.R
\name{create_raster_from_variables}
\alias{create_raster_from_variables}
\title{Fills a raster grid with variable values associated with geographic locations}
\usage{
create_raster_from_variables(
  data,
  site_locations,
  grid,
  site_crs = sf::st_crs(site_locations),
  filename = ""
)
}
\arguments{
\item{data}{A vector or two-dimensional object. Elements/rows correspond to
\code{site_locations}; columns (if present) represent variables.}

\item{site_locations}{An object that described geographic locations of
\code{data} that can be sent to \code{\link[rSW2st]{as_points}}}

\item{grid}{A \var{Raster*} object used as template.}

\item{site_crs}{The \code{crs} of \code{site_locations} passed to
\code{\link[rSW2st]{as_points}}.}

\item{filename}{A character string. Passed to \code{\link[raster]{brick}}.}
}
\value{
A \code{\link[raster:RasterLayer-class]{raster::RasterLayer}}
  (if \code{data} is a vector) or
  \code{\link[raster:RasterBrick-class]{raster::RasterBrick}}
  (if \code{data} is two-dimensional).
}
\description{
Fills a raster grid with variable values associated with geographic locations
}
\examples{
if (requireNamespace("raster")) {
  r <- raster::raster(
    xmn = 0, xmx = 10,
    ymn = 0, ymx = 10,
    crs = "OGC:CRS84",
    resolution = c(1, 1)
  )

  rv <- create_raster_from_variables(
    data = 1:10,
    site_locations = as_points(
      0.5 + cbind(0:9, 0:9),
      crs = "OGC:CRS84",
      to_class = "sf"
    ),
    grid = r
  )
}

}
