% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_Layer.R
\name{addLayer}
\alias{addLayer}
\title{Add layer}
\usage{
addLayer(mappedData, geomAttributes, geom, plotObject, layerToCall)
}
\arguments{
\item{mappedData}{A \verb{MappedData object}}

\item{geomAttributes}{arguments passed on to the ggplot2 geom layer}

\item{geom}{character used to select appropriate aesthetics}

\item{plotObject}{A \code{ggplot} object on which to add the plot layer}

\item{layerToCall}{function ggplot2 geom layer}
}
\value{
The updated \code{ggplot} object
}
\description{
Add layer
}
\keyword{internal}
