#' Initialize Plot
#'
#' Initialize a `ggplot` object with a watermark and set its labels by metaData.
#'
#' @param mappedData A `MappedData` object.
#' @param setMapping A boolean indicating if TRUE (default) mapping is passed to ggplot; otherwise, mapping will be used only to create labels.
#'
#' @return A `ggplot` object.
#' @export
initializePlot <- function(mappedData = NULL,
                           setMapping = TRUE) {
  # Validation
  checkmate::assertClass(mappedData, classes = "MappedData", null.ok = TRUE)
  checkmate::assertFlag(setMapping)

  mappingToSet <- aes()
  if (setMapping && !is.null(mappedData)) {
    mappingToSet <- mappedData$mapping
  }
  plotObject <- ggplotWithWatermark(
    data = mappedData$dataForPlot,
    mapping = mappingToSet
  )

  # add labels
  plotObject <- addLabels(plotObject, mappedData)

  return(plotObject)
}
#' Add Layer
#'
#' Add a layer to a `ggplot` object.
#'
#' @param mappedData A `MappedData` object.
#' @param plotObject A `ggplot` object on which to add the plot layer.
#' @param geomAttributes Arguments passed on to the ggplot2 geom layer.
#' @param geom A character string used to select appropriate aesthetics.
#' @param layerToCall A function representing the ggplot2 geom layer.
#'
#' @return The updated `ggplot` object.
#' @keywords internal
addLayer <- function(mappedData,
                     geomAttributes,
                     geom,
                     plotObject,
                     layerToCall) {
  # Validate input attributes
  checkmate::assertClass(mappedData, "MappedData")
  checkmate::assertClass(plotObject, "gg")
  checkmate::assertList(geomAttributes, null.ok = TRUE)
  checkmate::assertCharacter(geom, len = 1)
  checkmate::assertFunction(layerToCall)

  filteredMapping <- mappedData$getAestheticsForGeom(
    geom = geom,
    geomAttributes = geomAttributes
  )

  # check for geomUnicodeMode
  geomUnicodeMode <- getOspsuite.plots.option(optionKey = OptionKeys$GeomPointUnicode)
  if (geomUnicodeMode &&
    geom == "point") {
    layerToCall <- geomPointUnicode
  }

  if (!is.null(filteredMapping)) {
    plotObject <- plotObject +
      do.call(
        what = layerToCall,
        args = c(
          list(
            data = mappedData$dataForPlot,
            mapping = filteredMapping
          ),
          utils::modifyList(
            x = list(na.rm = TRUE),
            val = geomAttributes
          )
        )
      )
  }

  if (geom == "point" & mappedData$hasLLOQMatch) {
    plotObject <- plotObject +
      scale_alpha_manual(values = getOspsuite.plots.option(optionKey = OptionKeys$LLOQAlphaVector)) +
      guides(alpha = "none")
  }

  return(plotObject)
}
#' Add LLOQ Layer with LLOQ Lines
#'
#' Add a layer for LLOQ lines to a `ggplot` object.
#'
#' @param plotObject A `ggplot` object on which to add the plot layer.
#' @param mappedData A `MappedData` object with LLOQ data.
#' @param layerToCall A function representing the ggplot2 geom layer.
#' @param useLinetypeAsAttribute A boolean indicating whether to set the line type
#' as an attribute (TRUE) or not (FALSE); if TRUE, no legend is created.
#' @param geomLLOQAttributes Additional attributes for the LLOQ layer.
#'
#' @return The updated `ggplot` object.
#' @export
addLLOQLayer <-
  function(plotObject,
           mappedData,
           layerToCall,
           useLinetypeAsAttribute,
           geomLLOQAttributes) {
    # Early return if no LLOQ data is present
    if (!mappedData$hasLLOQMatch) {
      return(plotObject)
    }

    # Configure LLOQ line appearance based on legend preference
    if (useLinetypeAsAttribute) {
      # When using linetype as attribute: no legend entry, direct styling
      geomLLOQAttributes <- utils::modifyList(
        list(linetype = getOspsuite.plots.option(optionKey = OptionKeys$LLOQLineType)),
        geomLLOQAttributes
      )
    }

    filteredMapping <- mappedData$getAestheticsForGeom(
      geom = "hvline",
      geomAttributes = geomLLOQAttributes
    )

    if (!useLinetypeAsAttribute) {
      # When not using linetype as attribute: create legend entry with "LLOQ" label
      filteredMapping <-
        structure(
          utils::modifyList(
            filteredMapping,
            aes(linetype = "LLOQ") # Maps to legend with "LLOQ" label
          ),
          class = "uneval"
        )
    }

    plotObject <- plotObject +
      do.call(
        what = layerToCall,
        args = c(
          list(
            data = mappedData$dataForPlot,
            mapping = filteredMapping
          ),
          utils::modifyList(
            x = list(na.rm = TRUE),
            val = geomLLOQAttributes
          )
        )
      )

    if (!useLinetypeAsAttribute) {
      # Add manual scale for legend: maps "LLOQ" label to specified line type
      plotObject <- plotObject +
        scale_linetype_manual(
          values = c(LLOQ = getOspsuite.plots.option(optionKey = OptionKeys$LLOQLineType)),
          guide = guide_legend(
            title = NULL,
            order = 10,
          )
        )
    }

    return(plotObject)
  }
#' Add X and Y Scale
#'
#' Add X and Y scales to a `ggplot` object.
#'
#' @param plotObject A `ggplot` object on which to add the scale.
#' @param xScale The x-axis scale type. Available is 'linear', 'log', 'discrete'
#' @param xScaleArgs A list of arguments for the x-axis scale.
#' @param yScale The y-axis scale type. Available is 'linear', 'log'
#' @param yScaleArgs A list of arguments for the y-axis scale.
#' @param secAxis Secondary axis arguments for scale_y functions.
#'
#' @return The updated `ggplot` object.
#' @export
addXYScale <- function(plotObject,
                       xScale = NULL,
                       xScaleArgs = list(),
                       yScale = NULL,
                       yScaleArgs = list(),
                       secAxis = waiver()) {
  if (!is.null(xScale)) {
    plotObject <- addXScale(plotObject,
      xScale = xScale,
      xScaleArgs = xScaleArgs
    )
  }

  if (!is.null(yScale)) {
    plotObject <- addYScale(plotObject,
      yScale = yScale,
      yScaleArgs = yScaleArgs,
      secAxis = secAxis
    )
  }

  return(plotObject)
}
#' add X-scale
#'
#' @inheritParams addXYScale
#'
#' @return The updated `ggplot` object
#' @export
addXScale <- function(plotObject,
                      xScale,
                      xScaleArgs = list()) {
  checkmate::assertChoice(xScale, choices = unlist(AxisScales), null.ok = TRUE)

  if (xScale == AxisScales$discrete) {
    scaleFunction <- scale_x_discrete
  } else {
    scaleFunction <- scale_x_continuous
  }

  if (xScale == AxisScales$log) {
    xScaleArgs[["transform"]] <- "log10"
    if (is.null(xScaleArgs$guide)) {
      xScaleArgs[["guide"]] <- "axis_logticks"
    }
  }

  plotObject <- plotObject +
    do.call(
      what = scaleFunction,
      args = xScaleArgs
    )

  return(plotObject)
}
#' add y-scale
#'
#' @inheritParams addXYScale
#'
#' @return The updated `ggplot` object
#' @export
addYScale <- function(plotObject,
                      yScale,
                      yScaleArgs = list(),
                      secAxis = waiver()) {
  checkmate::assertChoice(yScale, choices = unlist(AxisScales[c("linear", "log")]), null.ok = TRUE)

  if (yScale == AxisScales$log) {
    yScaleArgs[["transform"]] <- "log10"
    if (is.null(yScaleArgs$guide)) {
      yScaleArgs[["guide"]] <- "axis_logticks"
    }
  }

  plotObject <- plotObject +
    do.call(
      what = scale_y_continuous,
      args = c(
        yScaleArgs,
        list(sec.axis = secAxis)
      )
    )

  return(plotObject)
}
