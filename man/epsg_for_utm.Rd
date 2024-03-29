% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crs.R
\name{epsg_for_utm}
\alias{epsg_for_utm}
\title{\var{EPSG} code for the \var{UTM} zone based on geographic location(s)}
\usage{
epsg_for_utm(x, crs = 4326)
}
\arguments{
\item{x}{A numerical two-dimensional object
(a \code{matrix}, \code{array}, or \code{data.frame})
with longitude/X, latitude/Y as columns;
a \code{\link[sp:SpatialPoints-class]{sp::SpatialPoints}} object; or
a \code{\link[terra:SpatVector-class]{terra::SpatVector}} object; or
a \var{sf} object with a point geometry,
i.e., an object with a class \var{sf} or \var{sfc}.}

\item{crs}{An object which is a \var{crs} or from which one can be derived.
\code{x} can
be numeric as a \var{EPSG} number;
a character string as a \var{wkt};
a character string as a \var{proj4} (not recommended because outdated);
or of a class including
  \code{\link[raster:Raster-class]{raster::Raster}},
  \code{\link[sp:Spatial-class]{sp::Spatial}},
  \code{\link[sp:CRS-class]{sp::CRS}},
  or a \code{\link[sf]{sf}} or \code{\link[sf]{sfc}} class.}
}
\value{
The \var{EPSG} code as integer value.
}
\description{
The function determines the \var{UTM} zone for the mid-point of \code{x}.
}
\examples{
locations <- matrix(
  data = c(-120.325, -111.245, 39.855, 36.753),
  nrow = 2
)

epsg_for_utm(locations)
sf::st_crs(epsg_for_utm(locations))

}
\references{
The python package \var{utm-zone} by Per Liedman available at
\url{https://pypi.org/project/utm-zone}.
}
\seealso{
\code{\link{utm_zone}}
}
