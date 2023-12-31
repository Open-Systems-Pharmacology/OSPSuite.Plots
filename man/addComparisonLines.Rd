% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotYVsX.R
\name{addComparisonLines}
\alias{addComparisonLines}
\title{add horizontal or diagonal comparison lines}
\usage{
addComparisonLines(
  plotObject,
  comparisonLineVector,
  addLinesDiagnonal,
  geomLineAttributes,
  xyscale
)
}
\arguments{
\item{comparisonLineVector}{either a double vector or a list of double values
if add \code{addLinesDiagnonal = FALSE} lines will be added as horizontal lines with the
intercept at values of \code{comparisonLineVector}
If add \code{addLinesDiagnonal = TRUE} lines will be added as fold distance lines to the identity.}

\item{addLinesDiagnonal}{A \code{boolean}which defines direction of comparison lines}

\item{geomLineAttributes}{line attributes e.g. color,linetype passed to \code{ggplot2::geom_hline} or \code{ggplot2::geom_abline}}

\item{xyscale}{either "linear" or "log" scale of x and y axis}
}
\value{
The updated \code{ggplot} object
}
\description{
add horizontal or diagonal comparison lines
}
\keyword{internal}
