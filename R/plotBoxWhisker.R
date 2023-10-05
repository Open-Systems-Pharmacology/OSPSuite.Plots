#' Title
#'
#' @param data
#' @param metaData
#' @param outliers
#' @param dataMapping
#' @param plotObject
#'
#' @return
#' @export
#'
#' @examples
plotBoxWhisker <- function(data,
                           mapping = NULL,
                           metaData = NULL,
                           plotObject = NULL,
                           percentiles = getOption(
                             x = "ospsuite.plots.Percentiles",
                             default = getDefaultggOSPOptions()[["ospsuite.plots.Percentiles"]]
                           ),
                           yscale = "linear",
                           yscale.args = list(),
                           xscale = "auto",
                           xscale.args = list(),
                           statFun = NULL,
                           outliers = FALSE,
                           statFunOutlier = NULL,
                           geomBoxplotAttributes = getDefaultGeomAttributes("Boxplot"),
                           geomPointAttributes = getDefaultGeomAttributes("Boxplot")) {
  ## Validation -----------
  checkmate::assertClass(plotObject, classes = "ggplot", null.ok = TRUE)
  checkmate::assertList(metaData, types = "list", null.ok = TRUE)
  checkmate::assertList(geomBoxplotAttributes, names = "named")
  checkmate::assertList(geomPointAttributes, names = "named")
  checkmate::assertDouble(
    percentiles,
    lower = 0,
    upper = 1,
    any.missing = FALSE,
    sorted = TRUE,
    null.ok = !is.null(statFun)
  )

  checkmate::assertChoice(xscale, choices = c("auto", "discrete", "linear", "log"), null.ok = FALSE)
  checkmate::assertList(xscale.args, null.ok = FALSE, min.len = 0)
  checkmate::assertChoice(yscale, choices = c("linear", "log"), null.ok = TRUE)
  checkmate::assertList(yscale.args, null.ok = FALSE, min.len = 0)

  checkmate::assertFunction(statFun, null.ok = !is.null(percentiles))
  checkmate::assertFlag(outliers)
  checkmate::assertFunction(statFunOutlier, null.ok = TRUE)


  ## map Data ----------

  mappedData <- MappedDataBoxplot$new(
    data = data,
    mapping = mapping,
    xscale = xscale
  )


  #-  create default plot ------
  if (is.null(plotObject)) {
    plotObject <- initializePlot(
      metaData = metaData,
      mapping = mappedData$mapping,
      data = mappedData$dataForPlot
    )
    if (!mappedData$hasXmapping) {
      plotObject$labels[["x"]] <-
        plotObject$labels[["x"]] %||% " "
    }
  }



  # set aggregation function ---------

  if (is.null(statFun)) {
    statFun <- function(y) {
      r <- stats::quantile(y, probs = percentiles, names = FALSE, na.rm = TRUE)
      names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
      return(r)
    }

    # add names of box whisker limits to ggplot for use in function getBoxWhiskerLimits
    statFunExport <- function(y) {
      r <- stats::quantile(y, probs = percentiles, names = FALSE, na.rm = TRUE)
      names(r) <- labelsForPercentile(percentiles = percentiles * 100)
      return(r)
    }

    plotObject$statFun <- statFunExport
  } else {
    plotObject$statFun <- statFun
  }


  # add x and y scale
  plotObject <- addXYScale(
    plotObject = plotObject,
    xscale = mappedData$xscale,
    xscale.args = xscale.args,
    yscale = yscale,
    yscale.args = yscale.args
  )



  plotObject <- plotObject +
    do.call(
      what = ggplot2::stat_summary,
      args = c(
        list(
          mapping = mappedData$boxwhiskerMapping,
          fun.data = statFun,
          geom = "boxplot",
          na.rm = TRUE
        ),
        geomBoxplotAttributes
      )
    )

  # outlier
  if (outliers) {
    # set function for outlier
    if (is.null(statFunOutlier)) {
      statFunOutlier <-
        function(x) {
          q <- stats::quantile(x, probs = c(0.25, 0.75), names = FALSE, na.rm = TRUE)
          iqr <- diff(range(q))
          pp <- subset(x, x < (q[1] - (1.5 * iqr)) |
            x > (q[2] + (1.5 * iqr)))
          if (length(pp) < 1) {
            return(as.double(NA))
          } else {
            return(pp)
          }
        }
    }
    plotObject$statFunOutlier <- statFunOutlier


    plotObject <- plotObject +
      do.call(
        what = ggplot2::stat_summary,
        args = c(
          list(
            fun = statFunOutlier,
            geom = "point",
            na.rm = TRUE
          ),
          geomPointAttributes
        )
      )
  }




  return(plotObject)
}
