% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotYVsX.R
\name{getCountsWithin}
\alias{getCountsWithin}
\title{Counts entries within specific limits}
\usage{
getCountsWithin(
  data,
  yColumn,
  xColumn = NULL,
  comparisonLineVector = getFoldDistanceList(c(1.5, 2)),
  addGuestLimits = FALSE,
  deltaGuest = 1,
  groups = NULL
)
}
\arguments{
\item{data}{A \code{data.frame} with data to plot}

\item{yColumn}{y column name for values to count}

\item{xColumn}{x column name for values to count}

\item{comparisonLineVector}{either a double vector or a list of double values
if add \code{addLinesDiagnonal = FALSE} lines will be added as horizontal lines with the
intercept at values of \code{comparisonLineVector}
If add \code{addLinesDiagnonal = TRUE} lines will be added as fold distance lines to the identity.}

\item{addGuestLimits}{A \code{boolean} which activates insertion of regression line}

\item{deltaGuest}{Numeric value parameter of Guest function}

\item{groups}{column names to group}
}
\value{
data table with summary
}
\description{
Counts entries within specific limits
}
