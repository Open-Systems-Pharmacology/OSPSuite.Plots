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
  plotObject <- ggplot(
    data = mappedData$dataForPlot,
    mapping = mappingToSet
  ) +
    layerWatermark()

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
          utils::modifyList(x = list(na.rm = TRUE),
                            val = geomAttributes)
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
#' @param useLinetypeAsAttribute boolean, if TRUE lientype is set as attribute, no legend is created
#' @param geomLLOQAttributes additional attributes
#' @param plotObject A `ggplot` object on which to add the plot layer
#' @param layerToCall function ggplot2 geom layer
#'
#' @return updated plotObject
#' @export
addLLOQLayer <-
  function(plotObject,
           mappedData,
           layerToCall,
           useLinetypeAsAttribute,
           geomLLOQAttributes) {
    if (!mappedData$hasLLOQMatch) return(plotObject)

    if (useLinetypeAsAttribute)
      geomLLOQAttributes = utils::modifyList(
        list(linetype = getOspsuite.plots.option(optionKey = OptionKeys$LLOQLineType)),
        geomLLOQAttributes)


    filteredMapping <- mappedData$getAestheticsForGeom(
      geom = "hvline",
      geomAttributes = geomLLOQAttributes
    )

    if (!useLinetypeAsAttribute)
      filteredMapping <-
      structure(utils::modifyList(filteredMapping,
                                  aes(linetype = 'LLOQ')),
                class = "uneval")


    plotObject <- plotObject +
      do.call(
        what = layerToCall,
        args = c(
          list(
            data = mappedData$dataForPlot,
            mapping = filteredMapping
          ),
          utils::modifyList(x = list(na.rm = TRUE),
                            val = geomLLOQAttributes)
        )
      )

    if (!useLinetypeAsAttribute)
      plotObject <- plotObject +
      scale_linetype_manual(values = c(LLOQ  = getOspsuite.plots.option(optionKey = OptionKeys$LLOQLineType)),
                            guide = guide_legend(
                              title = NULL,
                              order = 10,
                            ))

    return(plotObject)
  }



#' Create a watermark layer for a ggplot object.
#'
#' @param label Passed to `buildWatermarkGrob`.
#' @param x Passed to `buildWatermarkGrob`.
#' @param y Passed to `buildWatermarkGrob`.
#' @param angle Passed to `buildWatermarkGrob`.
#'   TRUE/FALSE for explicit control, otherwise use option watermark_enabled
#' @param color Passed to `buildWatermarkGrob`.
#' @param alpha Passed to `buildWatermarkGrob`.
#' @param fontsize Passed to `buildWatermarkGrob`.
#' @param show Determines whether the watermark will be created. Set to
#'
#' @return a ggplot2 layer with the watermark (if show is `TRUE` or (`NULL` and
#'   option 'ospsuite.plots.watermark_enabled' is `TRUE`)), otherwise an empty layer
#' @export
#'
layerWatermark <- function(label = NULL,
                           x = NULL,
                           y = NULL,
                           angle = NULL,
                           color = NULL,
                           alpha = NULL,
                           fontsize = NULL,
                           show = NULL) {
  if (is.null(show)) {
    show <- getOspsuite.plots.option(optionKey = OptionKeys$watermark_enabled)
  }

  if (show) {
    if (is.null(label)) {
      label <- getOspsuite.plots.option(optionKey = OptionKeys$watermark_label)
    }

    formatOptionsSet <- utils::modifyList(
      getDefaultOptions()$ospsuite.plots.watermark_format,
      getOspsuite.plots.option(optionKey = OptionKeys$watermark_format)
    )
    invisible(list2env(formatOptionsSet, envir = environment()))

    watermarkLayer <- annotation_custom(buildWatermarkGrob(
      label = label,
      x = x,
      y = y,
      angle = angle,
      color = color,
      fontsize = fontsize,
      alpha = alpha
    ))
  } else {
    watermarkLayer <- NULL
  }

  return(watermarkLayer)
}

#' Build a `grid::textGrob` to be used as a watermark.
#'
#' @param label The text of the watermark - leave NULL to use the option watermark_label
#' @param x Horizontal location, in 0..1 coordinates
#' @param y Vertical location, in 0..1  coordinates
#' @param angle Angle of the text
#' @param color Color of the test
#' @param alpha opacity of the text
#' @param fontsize size of the text
#'
#' @keywords internal
#' @return a `grid::textGrob` with the watermark
buildWatermarkGrob <- function(label, x = .5, y = .5, angle = 30,
                               color = "grey20",
                               alpha = 0.7,
                               fontsize = 12) {
  # enable additional settings
  extraParameters <- list()
  extraParameters$col <- color
  extraParameters$alpha <- alpha
  extraParameters$fontsize <- fontsize

  gpar <- grid::get.gpar()
  gpar <- utils::modifyList(gpar, extraParameters)


  watermarkGrob <- grid::textGrob(label = label, y = y, x = x, rot = angle, gp = gpar)
  return(watermarkGrob)
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
  checkmate::assertChoice(xscale, choices = c("linear", "log", "discrete"), null.ok = TRUE)

  plotObject <- plotObject +
    switch(xscale,
           "linear" = {
             do.call(
               what = scale_x_continuous,
               args = xscale.args
             )
           },
           "log" = {
             if (is.null(xscale.args$guide)) xscale.args$guide <- ggplot2::guide_axis_logticks()
             do.call(
               what = scale_x_log10,
               args = xscale.args
             )
           },
           "discrete" = {
             do.call(
               what = scale_x_discrete,
               args = xscale.args
             )
           }
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
  checkmate::assertChoice(yscale, choices = c("linear", "log"), null.ok = TRUE)


  plotObject <- plotObject +
    if (yscale == "linear") {
      do.call(
        what = scale_y_continuous,
        args = c(
          yscale.args,
          list(sec.axis = secAxis)
        )
      )
    } else {
      if (is.null(yscale.args$guide)) yscale.args$guide <- ggplot2::guide_axis_logticks()
      do.call(
        what = scale_y_log10,
        args = c(
          yscale.args,
          list(sec.axis = secAxis)
        )
      )
    }

  return(plotObject)
}
