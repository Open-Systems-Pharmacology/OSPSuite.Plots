#' initialize Plot
#'
#' Initialize a `ggplot` object
#' with watermark
#' and set its labels by metaData
#'
#' @param mappedData  MappedData object
#' @param setMapping if TRUE (default) mapping is passed to ggplot, otherwise mapping will be used only to create labels
#'
#' @return A `ggplot` object
#' @export
#'
initializePlot <- function(mappedData = NULL,
                           setMapping = TRUE) {
  # Validation
  checkmate::assertClass(mappedData, classes = "MappedData", null.ok = TRUE)
  checkmate::assertFlag(setMapping)

  mappingToSet <- aes()
  if (setMapping && !is.null(mappedData)) {
    mappingToSet <- mappedData$mapping
  }
  plotObject <- ggplotWithWatermark(data = mappedData$dataForPlot,
                      mapping = mappingToSet)

  #
  shapeValues <- getOspsuite.plots.option(optionKey = OptionKeys$shapeValues)
  if (!is.null(shapeValues)) {
    # shape
    scale_shape_discrete <- function(...) { # nolint use snake_case as it is copied from ggplot
      scale_shape_manual(values = shapeValues)
    }
    assign("scale_shape_discrete", scale_shape_discrete)
  }

  # add labels
  plotObject <- addLabels(plotObject, mappedData)

  return(plotObject)
}


#' Add layer
#'
#' @param mappedData A `MappedData object`
#' @param plotObject A `ggplot` object on which to add the plot layer
#' @param geomAttributes arguments passed on to the ggplot2 geom layer
#' @param geom character used to select appropriate aesthetics
#' @param layerToCall function ggplot2 geom layer
#'
#' @keywords internal
#' @return The updated `ggplot` object
addLayer <- function(mappedData,
                     geomAttributes,
                     geom,
                     plotObject,
                     layerToCall) {
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


#' add LLOQ Layer with lloq lines
#'
#'
#' @param mappedData object of class 'MappedData', with lloq data
#' @param useLinetypeAsAttribute boolean, if TRUE line type is set as attribute, no legend is created
#' @param geomLLOQAttributes additional attributes
#' @param plotObject A `ggplot` object on which to add the plot layer
#' @param layerToCall function ggplot2 geom layer
#'
#' @return updated plot object
#' @export
addLLOQLayer <-
  function(plotObject,
           mappedData,
           layerToCall,
           useLinetypeAsAttribute,
           geomLLOQAttributes) {
    if (!mappedData$hasLLOQMatch) {
      return(plotObject)
    }

    if (useLinetypeAsAttribute) {
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
      filteredMapping <-
        structure(
          utils::modifyList(
            filteredMapping,
            aes(linetype = "LLOQ")
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

#' add X and Y-scale
#'
#' @param plotObject A `ggplot` object on which to add the scale
#' @param secAxis secondary axis arguments for scale_y functions
#' @inheritParams plotTimeProfile
#'
#' @return The updated `ggplot` object
#' @export
addXYScale <- function(plotObject,
                       xscale = NULL,
                       xscale.args = list(),
                       yscale = NULL,
                       yscale.args = list(),
                       secAxis = waiver()) {
  if (!is.null(xscale)) {
    plotObject <- addXscale(plotObject,
                            xscale = xscale,
                            xscale.args = xscale.args
    )
  }

  if (!is.null(yscale)) {
    plotObject <- addYscale(plotObject,
                            yscale = yscale,
                            yscale.args = yscale.args,
                            secAxis = secAxis
    )
  }

  return(plotObject)
}


#' add X-scale
#'
#' @param plotObject A `ggplot` object on which to add the scale
#' @inheritParams plotTimeProfile
#'
#' @return The updated `ggplot` object
#' @export
addXscale <- function(plotObject,
                      xscale,
                      xscale.args = list()) {
  scaleFunctions <- list()
  scaleFunctions[[AxisScales$linear]] <- scale_x_continuous
  scaleFunctions[[AxisScales$log]] <- scale_x_log10
  scaleFunctions[[AxisScales$discrete]] <- scale_x_discrete

  checkmate::assertChoice(xscale, choices = names(scaleFunctions), null.ok = TRUE)

  if (xscale == AxisScales$log &&
      is.null(xscale.args$guide)) {
    xscale.args$guide <- ggplot2::guide_axis_logticks()
  }

  plotObject <- plotObject +
    do.call(
      what = scaleFunctions[[xscale]],
      args = xscale.args
    )

  return(plotObject)
}


#' add y-scale
#'
#' @param plotObject A `ggplot` object on which to add the scale
#' @param secAxis secondary axis arguments for scale_y functions
#' @inheritParams plotTimeProfile
#'
#' @return The updated `ggplot` object
#' @export
addYscale <- function(plotObject,
                      yscale,
                      yscale.args = list(),
                      secAxis = waiver()) {
  scaleFunctions <- list()
  scaleFunctions[[AxisScales$linear]] <- scale_y_continuous
  scaleFunctions[[AxisScales$log]] <- scale_y_log10

  checkmate::assertChoice(yscale, choices = names(scaleFunctions), null.ok = TRUE)

  if (yscale == AxisScales$log &&
      is.null(yscale.args$guide)) {
    yscale.args$guide <- ggplot2::guide_axis_logticks()
  }

  plotObject <- plotObject +
    do.call(
      what = scaleFunctions[[yscale]],
      args = c(
        yscale.args,
        list(sec.axis = secAxis)
      )
    )

  return(plotObject)
}
