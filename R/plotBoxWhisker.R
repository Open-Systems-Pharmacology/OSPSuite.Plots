#' @title generate  box-whisker plots
#' @description
#' Producing box-and-whisker plots
#' For more details and examples see the vignettes:
#' * \code{vignette("Box-Whisker Plots", package = "ospsuite.plots")}
#' * \code{vignette("ospsuite.plots", package = "ospsuite.plots")}
#'
#'
#' @param data  data.frame with data to aggregate
#' @param mapping  a list of aesthetic mappings to use for plot
#' @inheritParams plotTimeProfile
#' @inheritParams plotYVsX
#' @param percentiles vector with percentiles used for the box whiskers and boxes e.g. c(0.05,0.25,0.5,0.75,0.95)
#'       default defined by ospsuite.plots option
#' @param statFun (default NULL) if not NULL function to calculate whiskers and box ranges, overwrites variable percentiles
#' @param outliers Logical defining if outliers should be included in boxplot.
#'        outliers are flagged when outside the range from "25th" percentile - 1.5 x IQR to "75th"
#'        percentiles + 1.5 x IQR, as suggested by McGill and
#' @param statFunOutlier (default NULL) if not NULL overwrites default calculation of outliers
#' @param geomBoxplotAttributes A `list` with arguments which are passed on to the geom boxplot call
#' @param geomPointAttributes A `list` with arguments which are passed on to the call `ggplot2::geom_point`
#' @param xscale either 'linear','log', discrete or 'auto' (default) auto select linear for continuous data and discrete for categorical data
#' @param xscale.args list of arguments passed to `ggplot2::scale_x_continuous()`, `ggplot2::scale_x_log10()` or
#'    `ggplot2::scale_x_discrete()`
#'
#' @return A `ggplot` object
#' @export
#' @family plot functions
plotBoxWhisker <- function(data,
                           mapping,
                           metaData = NULL,
                           plotObject = NULL,
                           percentiles = getOspsuite.plots.option(optionKey = OptionKeys$Percentiles),
                           yscale = "linear",
                           yscale.args = list(),
                           xscale = "auto",
                           xscale.args = list(),
                           statFun = NULL,
                           outliers = FALSE,
                           statFunOutlier = NULL,
                           geomBoxplotAttributes = getDefaultGeomAttributes("Boxplot"),
                           geomPointAttributes = getDefaultGeomAttributes("Boxplot"),
                           residualScale = "log") {
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
    xscale = xscale,
    yscale = yscale,
    groupAesthetics = "fill",
    isObserved = TRUE,
    residualScale = residualScale,
    residualAesthetic = "y"
  )
  mappedData$addMetaData(metaData)
  mappedData$doAdjustmentsWithMetaData(
    originalmapping = mapping,
    xscale = xscale,
    xscale.args = xscale.args
  )

  #-  create default plot ------
  if (is.null(plotObject)) {
    plotObject <- initializePlot(mappedData)
    if (!mappedData$hasXmapping) {
      plotObject$labels[["x"]] <-
        plotObject$labels[["x"]] %||% " "
    }

    if (mappedData$hasResidualMapping) {
      plotObject <-
        plotObject +
        labs(y = mappedData$residualLabel)
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
      y <- y[!is.na(y)]
      rQuantiles <- stats::quantile(y, probs = percentiles, names = FALSE, na.rm = TRUE)
      names(rQuantiles) <- paste(scales::label_ordinal()(x = percentiles * 100),'percentile')

      r <- c(N = length(y),
             rQuantiles,
             "arith mean" = mean(y),
             "arith standard deviation" = sd(y),
             "arith CV" = sd(y)/mean(y),
             "geo mean"	= exp(mean(log(y))),
             "geo standard deviation"	= exp(sd(log(y))),
             "geo CV" = sqrt(exp((log(sd(y)))^2)-1)
      )

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
