#' @title generates residual quantile quantile plot
#' @description
#'
#' For details and examples see the vignettes:
#' * \code{vignette("Goodness of fit", package = "ospsuite.plots")}
#' * \code{vignette("ospsuite.plots", package = "ospsuite.plots")}
#'
#' @inheritParams plotTimeProfile
#' @param data  ´data.frame` with data to plot
#' @param residualScale  either "linear" or "log" scale residuals,
#'        for linear residuals are calculated observed - predicted
#'        for log residuals are calculated log(observed) - log(predicted)
#'        y scale is always linear
#' @param geomQQAttributes arguments passed on to `ggplot2::qq_stat`
#' @param geomQQLineAttributes arguments passed on to `ggplot2::qq_stat_line`
#'
#'
#' @return A `ggplot` object
#' @export
#' @family plot functions
plotQQ <- function(data,
                   mapping,
                   metaData = NULL,
                   xscale.args = list(),
                   residualScale = "log",
                   yscale.args = list(),
                   geomQQAttributes = list(),
                   geomQQLineAttributes = geomQQAttributes,
                   groupAesthetics = c("colour", "fill", "shape")) {
  # Check validity
  checkmate::assertDataFrame(data)

  checkmate::assertChoice(residualScale, choices = c("linear", "log"), null.ok = TRUE)
  checkmate::assertList(xscale.args, null.ok = FALSE, min.len = 0)
  checkmate::assertList(yscale.args, null.ok = FALSE, min.len = 0)

  checkmate::assertList(geomQQAttributes, null.ok = FALSE, min.len = 0)
  checkmate::assertList(geomQQLineAttributes, null.ok = FALSE, min.len = 0)

  checkmate::assertCharacter(groupAesthetics, min.len = 0, all.missing = TRUE, null.ok = TRUE)

  # data match --------------
  mappedData <- MappedData$new(
    data = data,
    mapping = mapping,
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
    xscale = "linear",
    xscale.args = xscale.args,
    yscale = "linear",
    yscale.args = yscale.args
  )

  return(plotObject)
}
