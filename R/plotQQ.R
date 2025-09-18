#' @title generates residual quantile quantile plot
#' @description
#'
#' For details and examples see the vignettes:
#' * \code{vignette("Goodness of fit", package = "ospsuite.plots")}
#' * \code{vignette("ospsuite.plots", package = "ospsuite.plots")}
#'
#' @inheritParams plotTimeProfile
#' @param data  ´data.frame` with data to plot
#' @param residualScale Either `"linear"` or `"log"` for scaling residuals. 
#'   For linear: residuals = observed - predicted. For log: residuals = log(observed) - log(predicted).
#'   The y-axis scale remains linear in both cases.
#' @param geomQQAttributes A list of arguments passed to `ggplot2::stat_qq()`.
#' @param geomQQLineAttributes A list of arguments passed to `ggplot2::stat_qq_line()`.
#' @param groupAesthetics A character vector of aesthetic names used for grouping data points in the Q-Q plot.
#'   Common options include `"colour"`, `"fill"`, `"shape"`, `"linetype"`, and `"size"`.
#'
#'
#' @return A `ggplot` object
#' @export
#' @family plot functions
plotQQ <- function(data,
                   mapping,
                   metaData = NULL,
                   xscale.args = list(),
                   residualScale = ResidualScales$log,
                   yscale.args = list(),
                   geomQQAttributes = list(),
                   geomQQLineAttributes = geomQQAttributes,
                   groupAesthetics = c("colour", "fill", "shape")) {
  # Check validity
  checkmate::assertDataFrame(data)

  checkmate::assertChoice(residualScale, choices = c(ResidualScales$linear, ResidualScales$log), null.ok = TRUE)
  checkmate::assertList(xscale.args, null.ok = FALSE, min.len = 0)
  checkmate::assertList(yscale.args, null.ok = FALSE, min.len = 0)

  checkmate::assertList(geomQQAttributes, null.ok = FALSE, min.len = 0)
  checkmate::assertList(geomQQLineAttributes, null.ok = FALSE, min.len = 0)
  checkmate::assertCharacter(groupAesthetics, null.ok = TRUE, min.len = 1)

  checkmate::assertCharacter(groupAesthetics, min.len = 0, all.missing = TRUE, null.ok = TRUE)

  xscale <- AxisScales$linear
  yscale <- AxisScales$linear

  # data match --------------
  mappedData <- MappedData$new(
    data = data,
    mapping = mapping,
    xscale = xscale,
    yscale = yscale,
    xlimits = xscale.args$limits,
    ylimits = yscale.args$limits,
    isObserved = TRUE,
    groupAesthetics = groupAesthetics,
    residualScale = residualScale,
    residualAesthetic = "sample"
  )
  mappedData$addMetaData(metaData = metaData)


  #-  initialize plot
  plotObject <- initializePlot(mappedData = mappedData) +
    theme(aspect.ratio = 1)
  plotObject <- plotObject +
    labs(x = "Standard normal quantiles")
  if (mappedData$hasResidualMapping) {
    plotObject <- plotObject +
      labs(y = mappedData$residualLabel)
  }


  #----- Build layers -----

  plotObject <- addLayer(
    mappedData = mappedData,
    geom = "qq",
    geomAttributes = geomQQAttributes,
    plotObject = plotObject,
    layerToCall = stat_qq
  )

  plotObject <- addLayer(
    mappedData = mappedData,
    geom = "qq",
    geomAttributes = geomQQLineAttributes,
    plotObject = plotObject,
    layerToCall = stat_qq_line
  )


  # set scales ----

  plotObject <- addXYScale(
    plotObject = plotObject,
    xscale = xscale,
    xscale.args = xscale.args,
    yscale = yscale,
    yscale.args = yscale.args
  )

  return(plotObject)
}
