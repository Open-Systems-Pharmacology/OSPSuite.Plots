% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{getFoldDistanceList}
\alias{getFoldDistanceList}
\title{creates a list with fold Distances}
\usage{
getFoldDistanceList(folds = c(1.5, 2), includeIdentity = TRUE)
}
\arguments{
\item{folds}{of folds e.g. c(1.5,2) must be >1}

\item{includeIdentity}{A \code{boolean}, if TRUE (default) line of identity is added}
}
\value{
named list with fold distances
}
\description{
this list is used as input for \code{plotRatioVsCov}, \code{plotPredVsObs}
}
