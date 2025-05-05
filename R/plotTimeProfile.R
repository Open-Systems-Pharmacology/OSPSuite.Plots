#' @title generate time profile plots
#' @description Produces time profiles for simulated and observed data.
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
#' @param observedData data.frame with observed data  will be displayed as points with error-bars
#' @param mapping  a list of aesthetic mappings to use for plot,
#'           additional to `{ggplot2}` aesthetics, the aesthetics `groupby`,`error`,`error_relative`,`lloq`, `mdv`, `y2axis` are available,
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
                            groupAesthetics = c("colour", "fill", "shape")) {
  groupAesthetics <- ggplot2::standardise_aes_names(groupAesthetics)
  .validatePlotTimeProfileInputs(
    data = data,
    observedData = observedData,
    plotObject = plotObject,
    metaData = metaData,
    xscale = xscale,
    xscale.args = xscale.args,
    yscale = yscale,
    yscale.args = yscale.args,
    y2scale = y2scale,
    y2scale.args = y2scale.args,
    geomLineAttributes = geomLineAttributes,
    geomRibbonAttributes = geomRibbonAttributes,
    geomPointAttributes = geomPointAttributes,
    geomErrorbarAttributes = geomErrorbarAttributes,
    geomLLOQAttributes = geomLLOQAttributes,
    groupAesthetics = groupAesthetics,
    mapSimulatedAndObserved = mapSimulatedAndObserved
  )

  ## - create MappedDataTimeprofile and get common ylimits
  listMappedData <-
    .getMappedDataForTimeProfiles(
      data = data,
      mapping = mapping,
      observedData = observedData,
      observedMapping = observedMapping,
      metaData = metaData,
      xscale = xscale,
      yscale = yscale,
      yscale.args = yscale.args,
      y2scale = y2scale,
      y2scale.args = y2scale.args,
      groupAesthetics = groupAesthetics,
      mapSimulatedAndObserved = mapSimulatedAndObserved
    )

  plotObject <-
    .initialplotObjectForTimeProfile(
      simMappedData = listMappedData$simMappedData,
      obsMappedData = listMappedData$obsMappedData,
      plotObject = plotObject,
      xscale = xscale,
      xscale.args = xscale.args,
      yscale = yscale,
      yscale.args = listMappedData$yscale.args, # for y2scaling, limits are updated
      y2scale.args = listMappedData$y2scale.args,
      secAxis = listMappedData$secAxis
    )

  ## -- addLayers
  plotObject <-
    .addLayersForSimulatedData(
      plotObject = plotObject,
      simMappedData = listMappedData$simMappedData,
      geomRibbonAttributes = geomRibbonAttributes,
      geomLineAttributes = geomLineAttributes,
      mapSimulatedAndObserved = listMappedData$mapSimulatedAndObserved,
      groupAesthetics = groupAesthetics
    )

  plotObject <-
    .addLayersForObserveddData(
      plotObject = plotObject,
      obsMappedData = listMappedData$obsMappedData,
      geomErrorbarAttributes = geomErrorbarAttributes,
      geomPointAttributes = geomPointAttributes,
      geomLLOQAttributes = geomLLOQAttributes,
      useLLOQLinetypeAsAttribute = listMappedData$useLLOQLinetypeAsAttribute,
      mapSimulatedAndObserved = listMappedData$mapSimulatedAndObserved,
      groupAesthetics = groupAesthetics
    )

  # set legend of group aesthetic first
  guidesList <- stats::setNames(
    lapply(groupAesthetics, function(aesthetic) {
      guide_legend(order = 1)
    }),
    groupAesthetics
  )
  plotObject <- plotObject + guides(!!!guidesList)


  return(plotObject)
}



#' Validates input for `plotTimeProfile` function
#'
#' @inheritParams plotTimeProfile
#'
#' @keywords internal
.validatePlotTimeProfileInputs <-
  function(data,
           observedData,
           plotObject,
           metaData,
           xscale,
           xscale.args,
           yscale,
           yscale.args,
           y2scale,
           y2scale.args,
           geomLineAttributes,
           geomRibbonAttributes,
           geomPointAttributes,
           geomErrorbarAttributes,
           geomLLOQAttributes,
           groupAesthetics,
           mapSimulatedAndObserved) {
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
    return(invisible())
  }

#' prepares mapped Data object for plotting
#'
#' @inheritParams plotTimeProfile
#'
#' @return list with entries `simMappedData` and `obsMappedData`
#'
#' @keywords internal
.getMappedDataForTimeProfiles <- function(data,
                                          mapping,
                                          observedData,
                                          observedMapping,
                                          metaData,
                                          xscale,
                                          yscale,
                                          yscale.args,
                                          y2scale,
                                          y2scale.args,
                                          groupAesthetics,
                                          mapSimulatedAndObserved) {
  requireDualAxis <- FALSE
  commonLimits <- list(
    y = yscale.args$limits,
    y2 = y2scale.args$limits
  )

  if (!is.null(mapSimulatedAndObserved)) {
    mapSimulatedAndObserved <- data.table::setnames(data.table::setDT(mapSimulatedAndObserved),
      old = names(mapSimulatedAndObserved),
      new = ggplot2::standardise_aes_names(names(mapSimulatedAndObserved))
    )
  }

  useLLOQLinetypeAsAttribute <- FALSE

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
      xscale = xscale,
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
    useLLOQLinetypeAsAttribute <-
      "linetype" %in% names(simMappedData$mapping)
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
      xscale = xscale,
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

    useLLOQLinetypeAsAttribute <-
      "linetype" %in% names(simMappedData$mapping) |
        useLLOQLinetypeAsAttribute
  } else {
    obsMappedData <- NULL
  }

  listMappedData <-
    .addCommonLimitsAndYscaleArguments(
      simMappedData = simMappedData,
      obsMappedData = obsMappedData,
      commonLimits = commonLimits,
      requireDualAxis = requireDualAxis,
      yscale.args = yscale.args,
      y2scale.args = y2scale.args
    )


  return(list(
    simMappedData = listMappedData$simMappedData,
    obsMappedData = listMappedData$obsMappedData,
    yscale.args = listMappedData$yscale.args,
    secAxis = listMappedData$secAxis,
    mapSimulatedAndObserved = mapSimulatedAndObserved,
    useLLOQLinetypeAsAttribute = useLLOQLinetypeAsAttribute
  ))
}

#' set the common axis limits to `MappedData` object and update y2-scale arguments
#'
#' @param simMappedData object of class `MappedDataTimeprofile` for simulated data
#' @param obsMappedData object of class `MappedDataTimeprofile` for observed data
#' @param commonLimits common limits for simulated and observed data
#' @param y2scale.args list with arguments for secondary y-axis
#' @param requireDualAxis boolean if TRUE secondary axis is needed
#' @param yscale.args list with y2scale arguments
#'
#' @return list with
#'  `simMappedData` adjusted object of class `MappedDataTimeprofile` for simulated data
#'  `obsMappedData` adjusted object of class `MappedDataTimeprofile` for observed data
#'  `yscale.args` adjusted `yscale.args` with common limits for primary and secondary y-axis
#'  `secAxis`  secondary axis object
#'
#' @keywords internal
.addCommonLimitsAndYscaleArguments <- function(simMappedData,
                                               obsMappedData,
                                               commonLimits,
                                               requireDualAxis,
                                               yscale.args,
                                               y2scale.args) {
  secAxis <- waiver()
  if (requireDualAxis) {
    if (!is.null(simMappedData)) {
      simMappedData <- simMappedData$scaleDataForSecondaryAxis(
        ylimits = commonLimits$y,
        y2limits = commonLimits$y2,
        y2scale.args = y2scale.args
      )

      secAxis <- simMappedData$secAxis %||% secAxis
    }
    if (!is.null(obsMappedData)) {
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

  return(list(
    simMappedData = simMappedData,
    obsMappedData = obsMappedData,
    yscale.args = yscale.args,
    secAxis = secAxis
  ))
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


#' initializes plot object and set scaling
#'
#' @param simMappedData object of class `MappedDataTimeprofile` for simulated data
#' @param obsMappedData object of class `MappedDataTimeprofile` for observed data
#' @inheritParams plotTimeProfile
#'
#' @return plot object
#' @keywords internal
.initialplotObjectForTimeProfile <- function(simMappedData,
                                             obsMappedData,
                                             plotObject,
                                             xscale,
                                             xscale.args,
                                             yscale,
                                             yscale.args,
                                             y2scale.args,
                                             secAxis) {
  # mapping can not be set in ggplot as observed and simulated mappings may differ
  if (is.null(plotObject)) {
    plotObject <- initializePlot(
      mappedData = simMappedData %||% obsMappedData,
      setMapping = FALSE
    )

    # add y2 label to y2scale.args
    if (!is.null(plotObject$labels$y2) &
      is.null(y2scale.args$name)) {
      secAxis$name <- plotObject$labels$y2
    }
  }

  ## -- set scale before the layers especially for creating new axis with ggnewscale,
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

  return(plotObject)
}


#' set line and ribbon layer fro simulated data
#'
#' @param simMappedData object of class `MappedDataTimeprofile` for simulated data
#' @inheritParams plotTimeProfile
#'
#' @return plot object wit newly added layers
#' @keywords internal
.addLayersForSimulatedData <- function(plotObject,
                                       simMappedData,
                                       geomRibbonAttributes,
                                       geomLineAttributes,
                                       mapSimulatedAndObserved,
                                       groupAesthetics) {
  if (is.null(simMappedData)) {
    return(plotObject)
  }
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
        scale_discrete_manual(aesthetic,
          values = mapSimulatedAndObserved[[aesthetic]],
          guide = guide_legend(order = 1)
        )
    }
  }

  return(plotObject)
}


#' set line and ribbon layer fro simulated data
#'
#' @inheritParams plotTimeProfile
#' @param obsMappedData object of class `MappedDataTimeprofile` for observed data
#' @param useLLOQLinetypeAsAttribute boolean if TRUE line type for LLOQ is set as attribute
#'
#' @keywords internal
.addLayersForObserveddData <- function(plotObject,
                                       obsMappedData,
                                       geomErrorbarAttributes,
                                       geomPointAttributes,
                                       geomLLOQAttributes,
                                       useLLOQLinetypeAsAttribute,
                                       mapSimulatedAndObserved,
                                       groupAesthetics) {
  if (is.null(obsMappedData)) {
    return(plotObject)
  }

  # add new scales for all aesthetics which occurs in simulated AND in observed data
  # to separate simulated and observed legend entries and start again with default colors
  if (!is.null(mapSimulatedAndObserved)) {
    for (aesthetic in groupAesthetics) {
      plotObject <- plotObject +
        guides(aesthetic = guide_legend(oder = 1)) +
        ggnewscale::new_scale(new_aes = aesthetic) +
        labs(!!sym(aesthetic) := "Observed") +
        labs(!!sym(paste0(aesthetic, "_ggnewscale_1")) := "Simulated")
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
  plotObject <- addLLOQLayer(
    plotObject = plotObject,
    mappedData = obsMappedData,
    layerToCall = geom_hline,
    useLinetypeAsAttribute = useLLOQLinetypeAsAttribute,
    geomLLOQAttributes = geomLLOQAttributes
  )

  # scale vectors of map simulated
  if (!is.null(mapSimulatedAndObserved)) {
    for (aesthetic in intersect(groupAesthetics, names(mapSimulatedAndObserved))) {
      plotObject <- plotObject +
        scale_discrete_manual(aesthetic, values = mapSimulatedAndObserved[[aesthetic]])
    }
  }

  return(plotObject)
}
