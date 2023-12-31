% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-defaults.R
\name{setDefaults}
\alias{setDefaults}
\title{sets the defaults for Ospsuite.plot}
\usage{
setDefaults(
  defaultOptions = list(),
  ColorMapList = NULL,
  shapeValues = NULL,
  PointAsUnicode = FALSE
)
}
\arguments{
\item{defaultOptions}{list of options}

\item{ColorMapList}{list of color maps}

\item{shapeValues}{list of Shapes}

\item{PointAsUnicode}{A \code{flag} to switch between mode for geom_point, if TRUE points will be plotted as unicode labels}
}
\value{
list of old settings which can be used to reset defaults with \code{resetDefaults()}
}
\description{
should be started at the beginning at each workflow
}
\details{
for detailed information see
\code{vignette("ospsuite.plots", package = "ospsuite.plots")}
}
\seealso{
Other setDefault functions: 
\code{\link{ColorMaps}},
\code{\link{Shapes}},
\code{\link{geomPointUnicode}()},
\code{\link{getDefaultGeomAttributes}()},
\code{\link{getDefaultOptions}()},
\code{\link{resetDefaultColorMapDistinct}()},
\code{\link{resetDefaultShapeDiscrete}()},
\code{\link{resetDefaultTheme}()},
\code{\link{resetDefaults}()},
\code{\link{setDefaultColorMapDistinct}()},
\code{\link{setDefaultShapeDiscrete}()},
\code{\link{setDefaultTheme}()}
}
\concept{setDefault functions}
