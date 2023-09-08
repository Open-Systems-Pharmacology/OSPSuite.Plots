#' @title generate time profile plots
#'
#' @description Produces timeprofiles for simulated and observed data.
#'
#' @details
#' For the simulated data a `geom_line` and a `geom_ribbon` layer are added
#' For the observed data a `geom_point` and a `geom_errorbar` layer are added
#'
#' As one of the intention of this plotting routine is to display simulated
#' and observed data with same grouping aesthetics like color and line type, but with different legend,
#' between the layers for simulated data and layers for observed  data new color scales are introduced.
#'
#' Non default mappings which can be used are:
#'
#'  * lloq
#'
#'      * only relevant for observed data
#'      * adds horizontal lines and displays measurement below LLOQ with a smaller alpha)
#'
#'  * mdv
#'
#'      * missing dependent Variable , should be 0 or 1
#'      * only relevant for observed data,
#'      * data with mdv 1 are not plotted
#'
#'  * error, error_relative
#'
#'      * calculates ymin and ymax for additive ore relative
#'
#'  * y2axis
#'
#'      * should be logical, defines if some of the outputs should be displayed wit a secondary axis
#'
#' For more details and examples see the vignettes:
#' \code{vignette("Time Profile Plots", package = "ospsuite.plots")}
#' \code{vignette("ospsuite.plots", package = "ospsuite.plots")}
#'
#' @param data  data.frame with simulated data
#' @param observedData data.frame with observed data
#' @param mapping  Default list of aesthetic mappings to use for plot
#' @param observedMapping Default list of aesthetic mappings to use for observed data
#' @param metaData A named list of information about `data` such as the `dimension` and `unit` of its variables.
#' @param plotObject An optional `ggplot` object on which to add the plot layers
#' @param geomLineAttributes A `list` with arguments which are passed on to the `ggplot2::geom_line`
#' @param geomRibbonAttributes A `list` with arguments which are passed on to the `ggplot2::geom_ribbon`
#' @param geomPointAttributes A `list` with arguments which are passed on to the `ggplot2::geom_point`
#' @param geomErrorbarAttributes A `list` with arguments which are passed on to the `ggplot2::geom_errorbar`
#' @param geomLLOQAttributes A `list` with arguments which are passed on to the `ggplot2::geom_hline`
#' @param groupAesthetics vector of aesthetics, which are used for columns mapped with group,
#'            use of group aesthetics triggers second axis after simulation layers
#' @param xscale either 'linear' then `ggplot2::scale_x_continuous()` or 'log' then `ggplot2::scale_x_log10()` is used
#' @param xscale.args list of arguments passed to `ggplot2::scale_x_continuous()` or `ggplot2::scale_x_log10()`
#' @param yscale either 'linear' then `ggplot2::scale_y_continuous()` or 'log' then `ggplot2::scale_y_log10()` is used
#' @param yscale.args list of arguments passed to `ggplot2::scale_y_continuous()` or `ggplot2::scale_y_log10()`
#' @param y2scale either 'linear' the secondary axis is displayed linear, or 'log' secondary axis is displayed with log scale
#' @param y2scale.args list of arguments passed to `ggplot2::sec_axis()`, trans, break are set by code
#'
#' @return A `ggplot` object
#' @export
#' @family plot functions
plotTimeProfile <- function(data = NULL,
                            mapping = NULL,
                            observedData = NULL,
                            observedMapping = mapping,
                            metaData = NULL,
                            xscale = "linear",
                            xscale.args = list(limits = c(0, NA)),
                            yscale = "linear",
                            yscale.args = list(),
                            y2scale = "linear",
                            y2scale.args = list(),
                            plotObject = NULL,
                            geomLineAttributes = getDefaultGeomAttributes("Line"),
                            geomRibbonAttributes = getDefaultGeomAttributes("Ribbon"),
                            geomPointAttributes = getDefaultGeomAttributes("Point"),
                            geomErrorbarAttributes = getDefaultGeomAttributes("Errorbar"),
                            geomLLOQAttributes = getDefaultGeomAttributes("LLOQ"),
                            groupAesthetics = c("colour", "fill", "linetype", "shape")) {
  ## Validation and formatting of input arguments ----------
  if (all(isEmpty(data), isEmpty(observedData))) {
    stop("At least 'data' or 'observedData' is required.")
  }

  checkmate::assertClass(plotObject, classes = "ggplot", null.ok = TRUE)
  checkmate::assertList(metaData, types = "list", null.ok = TRUE)

  checkmate::assertChoice(xscale, choices = c("linear", "log"), null.ok = TRUE)
  checkmate::assertList(xscale.args, null.ok = FALSE, min.len = 0)
  checkmate::assertChoice(yscale, choices = c("linear", "log"), null.ok = TRUE)
  checkmate::assertList(yscale.args, null.ok = FALSE, min.len = 0)
  checkmate::assertChoice(y2scale, choices = c("linear", "log"), null.ok = TRUE)
  checkmate::assertList(y2scale.args, null.ok = FALSE, min.len = 0)

  checkmate::assertList(geomLineAttributes, null.ok = FALSE, min.len = 0)
  checkmate::assertList(geomRibbonAttributes, null.ok = FALSE, min.len = 0)
  checkmate::assertList(geomLineAttributes, null.ok = FALSE, min.len = 0)
  checkmate::assertList(geomPointAttributes, null.ok = FALSE, min.len = 0)
  checkmate::assertList(geomErrorbarAttributes, null.ok = FALSE, min.len = 0)
  checkmate::assertList(geomLLOQAttributes, null.ok = FALSE, min.len = 0)

  checkmate::assertCharacter(groupAesthetics, min.len = 0, all.missing = TRUE, null.ok = TRUE)

  #-  create default plot ----------
  # mapping can not be set in ggplot as observed and simulated mappings may differ
  if (is.null(plotObject)) {
    plotObject <- initializePlot(
      metaData = metaData,
      mapping = mapping %||% observedMapping,
      setMapping = FALSE
    )

    # add y2 label to y2scale.args
    if (!is.null(plotObject$labels$y2) &
      is.null(y2scale.args$name)) {
      y2scale.args$name <- plotObject$labels$y2
    }
  }

  ## - create MappedDataTimeprofile and get common ylimits ----

  requireDualAxis <- FALSE
  commonLimits <- list(
    y = yscale.args$limits,
    y2 = y2scale.args$limits
  )

  secAxis <- waiver()


  if (!isEmpty(data)) {
    simMappedData <- MappedDataTimeProfile$new(
      data = data,
      mapping = mapping,
      groupAesthetics = groupAesthetics,
      direction = "y",
      isObserved = FALSE,
      scaleOfDirection = yscale,
      scaleOfSecondaryAxis = y2scale,
      ylimits = yscale.args$limits,
      y2limits = y2scale.args$limits
    )
    requireDualAxis <- simMappedData$requireDualAxis
    commonLimits <- .adjustLimits(
      commonLimits,
      list(
        y = simMappedData$ylimits,
        y2 = simMappedData$y2limits
      )
    )
  }
  if (!isEmpty(observedData)) {
    obsMappedData <- MappedDataTimeProfile$new(
      data = observedData,
      mapping = observedMapping %||% mapping,
      groupAesthetics = groupAesthetics,
      isObserved = TRUE,
      direction = "y",
      scaleOfDirection = yscale,
      scaleOfSecondaryAxis = y2scale,
      ylimits = yscale.args$limits,
      y2limits = y2scale.args$limits
    )

    requireDualAxis <- requireDualAxis | obsMappedData$requireDualAxis
    commonLimits <- .adjustLimits(
      commonLimits,
      list(
        y = obsMappedData$ylimits,
        y2 = obsMappedData$y2limits
      )
    )

    if (requireDualAxis) {
      obsMappedData <- obsMappedData$scaleDataForSecondaryAxis(
        ylimits = commonLimits$y,
        y2limits = commonLimits$y2,
        y2scale.args = y2scale.args
      )

      secAxis <- obsMappedData$secAxis %||% secAxis
    }
  }
  if (!isEmpty(data) & requireDualAxis) {
    simMappedData <- simMappedData$scaleDataForSecondaryAxis(
      ylimits = commonLimits$y,
      y2limits = commonLimits$y2,
      y2scale.args = y2scale.args
    )

    secAxis <- simMappedData$secAxis %||% secAxis
  }

  # to suppress warnings add limits if secondary axis exists
  if (requireDualAxis) yscale.args$limits <- commonLimits$y


  ## -- set scale before the layers especially for creating new axis with ggnewscale, ----
  # as otherwise warnings appear

  # check for timeUnit scaling
  xscale.args <- updateScaleArgumentsForTimeUnit(
    scale.args = xscale.args,
    metaData = metaData,
    mapping = mapping %||% observedMapping,
    scaleDirection = "x"
  )

  plotObject <- addXYScale(
    plotObject = plotObject,
    xscale = xscale,
    xscale.args = xscale.args,
    yscale = yscale,
    yscale.args = yscale.args,
    secAxis = secAxis
  )



  ## -- addLayers --------
  #- plot simulated data
  if (!isEmpty(data)) {
    # If available, add ribbons for population time profiles
    plotObject <- addLayer(
      mappedData = simMappedData,
      geom = "ribbon",
      geomAttributes = geomRibbonAttributes,
      plotObject = plotObject,
      layerToCall = geom_ribbon
    )


    # If available, add simulated time profile
    plotObject <- addLayer(
      mappedData = simMappedData,
      geom = "line",
      geomAttributes = geomLineAttributes,
      plotObject = plotObject,
      layerToCall = geom_line
    )
  }


  #-  plot observed data
  if (!isEmpty(observedData)) {
    # add new scales for all aesthetics which occurs in simulated AND in observed data
    # to separate simulated and observed legend entries and start again with default colors
    for (aesthetic in groupAesthetics) {
      plotObject <- plotObject + ggnewscale::new_scale(aesthetic)
    }


    # - If available, add error bars
    plotObject <- addLayer(
      mappedData = obsMappedData,
      geom = "errorbar",
      geomAttributes = geomErrorbarAttributes,
      plotObject = plotObject,
      layerToCall = geom_errorbar
    )


    # - Add observed scatter points with scale for LLOQ
    plotObject <- addLayer(
      mappedData = obsMappedData,
      geom = "point",
      geomAttributes = geomPointAttributes,
      plotObject = plotObject,
      layerToCall = geom_point
    )


    # add lloq lines
    if (obsMappedData$hasLLOQMatch) {
      plotObject <- addLayer(
        mappedData = obsMappedData,
        geom = "hvline",
        geomAttributes = geomLLOQAttributes,
        plotObject = plotObject,
        layerToCall = geom_hline
      )
    }
  }


  return(plotObject)
}





#' compares old and new limits
#'
#' @param commonLimits_old  list of old limits
#' @param commonLimits_new  list of new limits
#'
#' @return updated limits
#' @keywords internal
.adjustLimits <- function(commonLimits_old, commonLimits_new) {
  for (aesthetic in names(commonLimits_old)) {
    if (!is.null(commonLimits_new[[aesthetic]])) {
      commonLimits_new[[aesthetic]][1] <-
        min(c(commonLimits_old[[aesthetic]][1], commonLimits_new[[aesthetic]][1]), na.rm = TRUE)
      commonLimits_new[[aesthetic]][2] <-
        max(c(commonLimits_old[[aesthetic]][2], commonLimits_new[[aesthetic]][2]), na.rm = TRUE)
    } else {
      commonLimits_new[[aesthetic]] <- commonLimits_old[[aesthetic]]
    }
  }

  return(commonLimits_new)
}
