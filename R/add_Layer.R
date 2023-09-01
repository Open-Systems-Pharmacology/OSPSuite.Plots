#' initialize Plot
#'
#' Initialize a `ggplot` object
#' with watermark
#' and set its labels by metaData
#'
#' @param data data passed to ggplot object
#' @param mapping  Default list of aesthetic mappings to use for plot
#' @param metaData A named list of information about the `data` such as the `dimension` and `unit` of its variables.
#' @param setMapping if TRUE (default) mapping is passed to ggplot, otherwise mapping will be used only to create labels
#'
#' @return A `ggplot` object
#' @export
#'
initializePlot <- function(metaData = NULL,
                           mapping = NULL,
                           data = NULL,
                           setMapping = TRUE) {
  # Validation
  checkmate::assertList(metaData, types = "list", null.ok = TRUE)
  checkmate::assertList(mapping, types = "quosure", null.ok = TRUE)
  checkmate::assertFlag(setMapping)

  mappingToSet <- aes()
  if (setMapping && !is.null(mapping)) {
    mappingToSet <- mapping
  }
  plotObject <- ggplot(
    data = data,
    mapping = mappingToSet,
    environment = globalenv()
  ) +
    layerWatermark()


  # add labels
  plotObject <- .addLabels(plotObject, metaData, mapping)


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
  if (!is.null(filteredMapping)) {
    plotObject <- plotObject +
      do.call(
        what = layerToCall,
        args = c(
          list(
            data = mappedData$dataForPlot,
            mapping = filteredMapping,
            na.rm = TRUE
          ),
          geomAttributes
        )
      )
  }


  if (geom == "point" & mappedData$hasLLOQMatch) {
    plotObject <- plotObject +
      scale_alpha_manual(values = getOption(
        x = "ospsuite.plots.LLOQAlphaVector",
        default = getDefaultOptions()[["ospsuite.plots.LLOQAlphaVector"]]
      )) +
      guides(alpha = "none")
  }


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
    show <- getOption("ospsuite.plots.watermark_enabled", getDefaultOptions()$ospsuite.plots.watermark_enabled)
  }

  if (show) {
    if (is.null(label)) {
      label <- getOption("ospsuite.plots.watermark_label", getDefaultOptions()$ospsuite.plots.watermark_label)
    }

    formatOptions_default <- getDefaultOptions()$ospsuite.plots.watermark_format
    formatOptions_set <- getOption("ospsuite.plots.watermark_format", formatOptions_default) # nolint

    for (f in names(formatOptions_default)) {
      if (is.null(get(f))) {
        eval(parse(text = paste0(f, ' = formatOptions_set[["', f, '"]] %||% formatOptions_default[["', f, '"]]')))
      }
    }


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
    watermarkLayer <- geom_blank()
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
#' @return a `grid::textGrob` with the watermark and the additional attribute 'is_watermark' set to `TRUE`
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
  attr(watermarkGrob, "is_watermark") <- TRUE
  return(watermarkGrob)
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
  checkmate::assertChoice(xscale, choices = c("linear", "log"), null.ok = TRUE)

  plotObject <- plotObject +
    if (xscale == "linear") {
      if (is.null(xscale.args$guide)) xscale.args$guide <- "axis_minor"
      do.call(
        what = scale_x_continuous,
        args = xscale.args
      )
    } else {
      if (is.null(xscale.args$guide)) xscale.args$guide <- "axis_logticks"
      do.call(
        what = scale_x_log10,
        args = xscale.args
      )
    }

  return(plotObject)
}


#' add y-scale
#'
#' @param plotObject A `ggplot` object on which to add the scale
#' @param secAxis description
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
      if (is.null(yscale.args$guide)) yscale.args$guide <- "axis_minor"
      do.call(
        what = scale_y_continuous,
        args = c(
          yscale.args,
          list(sec.axis = secAxis)
        )
      )
    } else {
      if (is.null(yscale.args$guide)) yscale.args$guide <- "axis_logticks"
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
