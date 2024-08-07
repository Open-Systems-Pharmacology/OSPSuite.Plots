#' @title generate time profile plots
#' @description Produces timeprofiles for simulated and observed data.
#'
#' For the simulated data a `geom_line` and a `geom_ribbon` layer are added
#' For the observed data a `geom_point` and a `geom_errorbar` layer are added
#'
#' For more details and examples see the vignettes:
#' * \code{vignette("Time Profile Plots", package = "ospsuite.plots")}
#' * \code{vignette("ospsuite.plots", package = "ospsuite.plots")}
#'
#'
#' @param data  data.frame with simulated data will be displayed as lines with ribbons
#' @param observedData data.frame with observed data  will be displayed as points with errorbars
#' @param mapping  a list of aesthetic mappings to use for plot,
#'           additional to {ggplot2} aesthetics, the aesthetics `groupby`,`error`,`error_relative`,`lloq`, `mdv`, `y2axis` are available,
#'           see vignettes for more details and examples
#' @param observedMapping a list of aesthetic mappings to use for observed data,
#'           per default is is set to mapping. So if both data sets have the same mapping, use only `mapping`,
#'           if a different mapping is necessary use `mapping` and `observedMapping`
#' @param metaData A named list of information about `data` such as the `dimension` and `unit` of its variables.
#' @param mapSimulatedAndObserved table with columns observed and simulated which maps simulated and observed data
#'            use of `mapSimulatedAndObserved` triggers reset of aesthetic scales after simulation layers
#' @param plotObject An optional `ggplot` object on which to add the plot layers
#' @param geomLineAttributes A `list` with arguments which are passed on to the call `ggplot2::geom_line`
#' @param geomRibbonAttributes A `list` with arguments which are passed on to the call `ggplot2::geom_ribbon`
#' @param geomPointAttributes A `list` with arguments which are passed on to the call `ggplot2::geom_point`
#' @param geomErrorbarAttributes A `list` with arguments which are passed on to the call `ggplot2::geom_errorbar`
#' @param geomLLOQAttributes A `list` with arguments which are passed on to the call `ggplot2::geom_hline`
#' @param groupAesthetics vector of aesthetics, which are used for columns mapped with `groupby`,
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
plotTimeProfile <- function(data = NULL, # nolint
                            mapping = NULL,
                            observedData = NULL,
                            observedMapping = mapping,
                            metaData = NULL,
                            mapSimulatedAndObserved = NULL,
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
  checkmate::assertList(geomPointAttributes, null.ok = FALSE, min.len = 0)
  checkmate::assertList(geomErrorbarAttributes, null.ok = FALSE, min.len = 0)
  checkmate::assertList(geomLLOQAttributes, null.ok = FALSE, min.len = 0)

  checkmate::assertCharacter(groupAesthetics, min.len = 0, all.missing = TRUE, null.ok = TRUE)
  checkmate::assertDataFrame(mapSimulatedAndObserved, null.ok = TRUE)
  if (!is.null(mapSimulatedAndObserved)) {
    checkmate::assertNames(names(mapSimulatedAndObserved), must.include = c("simulated", "observed"))
    names(mapSimulatedAndObserved) <- standardise_aes_names(names(mapSimulatedAndObserved))
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
      groupOrder = if (!is.null(mapSimulatedAndObserved)) {
        mapSimulatedAndObserved$simulated
      } else {
        NULL
      },
      scaleOfPrimaryAxis = yscale,
      scaleOfSecondaryAxis = y2scale,
      ylimits = yscale.args$limits,
      y2limits = y2scale.args$limits
    )
    simMappedData$addMetaData(metaData = metaData)
    requireDualAxis <- simMappedData$requireDualAxis
    commonLimits <- .adjustLimits(
      commonLimits,
      list(
        y = simMappedData$ylimits,
        y2 = simMappedData$y2limits
      )
    )
  } else {
    simMappedData <- NULL
  }
  if (!isEmpty(observedData)) {
    obsMappedData <- MappedDataTimeProfile$new(
      data = observedData,
      mapping = observedMapping %||% mapping,
      groupAesthetics = groupAesthetics,
      groupOrder = if (!is.null(mapSimulatedAndObserved)) {
        mapSimulatedAndObserved$observed
      } else {
        NULL
      },
      isObserved = TRUE,
      direction = "y",
      scaleOfPrimaryAxis = yscale,
      scaleOfSecondaryAxis = y2scale,
      ylimits = yscale.args$limits,
      y2limits = y2scale.args$limits
    )

    obsMappedData$addMetaData(metaData = metaData)


    requireDualAxis <- requireDualAxis | obsMappedData$requireDualAxis
    commonLimits <- .adjustLimits(
      commonLimits,
      list(
        y = obsMappedData$ylimits,
        y2 = obsMappedData$y2limits
      )
    )
  } else {
    obsMappedData <- NULL
  }


  #-  create default plot ----------
  # mapping can not be set in ggplot as observed and simulated mappings may differ
  if (is.null(plotObject)) {
    plotObject <- initializePlot(
      mappedData = simMappedData %||% obsMappedData,
      setMapping = FALSE
    )

    # add y2 label to y2scale.args
    if (!is.null(plotObject$labels$y2) &
      is.null(y2scale.args$name)) {
      y2scale.args$name <- plotObject$labels$y2
    }
  }

  # add common limits and yscale arguments
  if (requireDualAxis) {
    if (!isEmpty(data)) {
      simMappedData <- simMappedData$scaleDataForSecondaryAxis(
        ylimits = commonLimits$y,
        y2limits = commonLimits$y2,
        y2scale.args = y2scale.args
      )

      secAxis <- simMappedData$secAxis %||% secAxis
    }
    if (!isEmpty(observedData)) {
      obsMappedData <- obsMappedData$scaleDataForSecondaryAxis(
        ylimits = commonLimits$y,
        y2limits = commonLimits$y2,
        y2scale.args = y2scale.args
      )

      secAxis <- obsMappedData$secAxis %||% secAxis
    }

    # to suppress warnings add limits if secondary axis exists
    yscale.args$limits <- commonLimits$y
  }

  ## -- set scale before the layers especially for creating new axis with ggnewscale, ----
  # as otherwise warnings appear

  # check for timeUnit scaling
  myMappedData <- simMappedData %||% obsMappedData
  xscale.args <- myMappedData$updateScaleArgumentsForTimeUnit(
    scale.args = xscale.args,
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


    if (!is.null(mapSimulatedAndObserved)) {
      for (aesthetic in intersect(groupAesthetics, names(mapSimulatedAndObserved))) {
        plotObject <- plotObject +
          scale_discrete_manual(aesthetic, values = mapSimulatedAndObserved[[aesthetic]], breaks = waiver())
      }
    }
  }

  #-  plot observed data
  if (!isEmpty(observedData)) {
    # add new scales for all aesthetics which occurs in simulated AND in observed data
    # to separate simulated and observed legend entries and start again with default colors
    if (!is.null(mapSimulatedAndObserved)) {
      for (aesthetic in groupAesthetics) {
        plotObject <- plotObject +
          ggnewscale::new_scale(new_aes = aesthetic) +
          labs(!!sym(aesthetic) := 'Observed') +
          labs(!!sym(paste0(aesthetic,'_ggnewscale_1')) := 'Simulated')
      }
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

    if (!is.null(mapSimulatedAndObserved)) {
      for (aesthetic in intersect(groupAesthetics, names(mapSimulatedAndObserved))) {
        plotObject <- plotObject +
          scale_discrete_manual(aesthetic, values = mapSimulatedAndObserved[[aesthetic]], breaks = waiver())
      }
    }
  }


  return(plotObject)
}


#' compares old and new limits
#'
#' @param commonLimitsOld  list of old limits
#' @param commonLimitsNew  list of new limits
#'
#' @return updated limits
#' @keywords internal
.adjustLimits <- function(commonLimitsOld, commonLimitsNew) {
  for (aesthetic in names(commonLimitsOld)) {
    if (!is.null(commonLimitsNew[[aesthetic]])) {
      commonLimitsNew[[aesthetic]][1] <-
        min(c(commonLimitsOld[[aesthetic]][1], commonLimitsNew[[aesthetic]][1]), na.rm = TRUE)
      commonLimitsNew[[aesthetic]][2] <-
        max(c(commonLimitsOld[[aesthetic]][2], commonLimitsNew[[aesthetic]][2]), na.rm = TRUE)
    } else {
      commonLimitsNew[[aesthetic]] <- commonLimitsOld[[aesthetic]]
    }
  }

  return(commonLimitsNew)
}
