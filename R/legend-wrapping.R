# Legend wrapping --------------------------

#' Wrap long legend entries of a plot
#'
#' @description
#' Applies automatic line wrapping to the legend entries (key labels) of a
#' plot, so that long entries - such as simulation output paths like
#' `Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)` -
#' are broken over several lines instead of shrinking the plot panel or being
#' cut off.
#'
#' The function wraps the labels of all scales that can contribute to the
#' legend and are mapped in the plot: `colour`/`color`, `fill`, `shape` and
#' `linetype`.
#'
#' * Scales already present on the plot (e.g. `scale_colour_osp()` or the LLOQ
#'   `scale_linetype_manual()`) keep their palettes, values and guides - only
#'   their labels are wrapped. Named `labels` and label functions are resolved
#'   first, so the wrapped text is exactly what would have been displayed.
#' * For aesthetics that are mapped but have no scale yet, a scale of the same
#'   type as the one the plot would get at build time is added with wrapped
#'   labels (`scale_colour_osp()` / `scale_fill_osp()` / `scale_shape_osp()`
#'   for `ospsuite.plots` plots, the `ggplot2` defaults otherwise), so the
#'   appearance of the plot is unchanged.
#'
#' Strings without spaces longer than `maxWidth` (e.g. compound output paths)
#' are split using `stringr::str_wrap(width = maxWidth, whitespace_only = FALSE)`, 
#' so that long tokens are broken into smaller chunks.
#'
#' Combine with [legendWidthFromPlotDimensions()] to derive `maxWidth` from
#' the exported plot size, or use the convenience wrapper [wrapLegend()].
#'
#' @param plotObject A `ggplot` object, e.g. created by [plotTimeProfile()],
#'   or a `CombinedPlot` object whose main plot is updated.
#' @param maxWidth Maximum number of characters per legend line.
#'
#' @return The `ggplot` object with wrapped legend labels (the input object is
#'   not modified).
#'
#' @examples
#' df <- data.frame(
#'   time = rep(seq(1, 24), 2),
#'   concentration = c(dexp(1:24, 0.2), dexp(1:24, 0.1)),
#'   output = rep(c(
#'     "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
#'     "Organism|PeripheralVenousBlood|Aciclovir|Muscle (Muscle Interstitial)"
#'   ), each = 24)
#' )
#' p <- plotTimeProfile(
#'   data = df,
#'   mapping = aes(x = time, y = concentration, color = output)
#' )
#' # wrap legend entries at 20 characters per line
#' pWrapped <- wrapLegendLabels(p, maxWidth = 20)
#'
#' # wrap at a width derived from the exported plot dimensions
#' pWrapped <- wrapLegendLabels(
#'   p,
#'   maxWidth = legendWidthFromPlotDimensions(p, plotWidth = 8, unit = "cm")
#' )
#'
#' @export
#' @family legend helpers
wrapLegendLabels <- function(plotObject, maxWidth) {
  if (inherits(plotObject, "CombinedPlot")) {
    # clone so that the input CombinedPlot is not modified
    plotClone <- plotObject$clone()
    plotClone$plotObject <- wrapLegendLabels(
      plotObject$plotObject,
      maxWidth = maxWidth
    )
    return(plotClone)
  }
  checkmate::assertClass(plotObject, "gg")
  checkmate::assertNumber(maxWidth, lower = 1)

  # Work on a copy of the scales so that the input plot is not modified.
  plotObject$scales <- plotObject$scales$clone()

  wrapLabels <- function(existingLabels) {
    # capture the current labels eagerly, before the scale is modified below
    force(existingLabels)
    return(function(breaks) {
      .wrapLegendText(.resolveScaleLabels(existingLabels, breaks), maxWidth)
    })
  }

  discreteAesthetics <- .usedDiscreteAesthetics(plotObject)
  isOspPlot <- "osp_ggplot" %in% class(plotObject)

  for (aesthetic in .mappedLegendAesthetics(plotObject)) {
    existingScale <- plotObject$scales$get_scales(aesthetic)
    if (!is.null(existingScale)) {
      scaleIndex <- which(vapply(
        plotObject$scales$scales,
        function(scale) aesthetic %in% scale$aesthetics,
        logical(1)
      ))
      if (!is.null(existingScale$labels)) {
        plotObject$scales$scales[[scaleIndex]]$labels <- wrapLabels(
          existingScale$labels
        )
      }
      next
    }

    # Discrete scales are the only ones with entry labels; a continuous
    # colour/fill mapping without an explicit scale is left untouched.
    isContinuousColorScale <- aesthetic %in%
      c("colour", "fill") &&
      !aesthetic %in% discreteAesthetics
    if (isContinuousColorScale) {
      next
    }

    plotObject <- plotObject +
      .legendScaleWithWrappedLabels(
        aesthetic = aesthetic,
        labels = wrapLabels(NULL),
        ospStyle = isOspPlot
      )
  }

  return(plotObject)
}

#' Maximum number of legend characters per line for given plot dimensions
#'
#' @description
#' Calculates the maximum number of characters per line that fit into the
#' legend of a plot with the given exported dimensions - the `maxWidth` to be
#' used with [wrapLegendLabels()].
#'
#' The calculation is based on:
#'
#' * the legend position of the plot: legends on the left or right side are
#'   given at most `legendFractionHorizontal` of the total plot width (default 40%),
#'   legends at the top or bottom are given `legendFractionVertical` (default 100%);
#' * the legend font size;
#'
#' @param plotObject A `ggplot` object, e.g. created by [plotTimeProfile()],
#'   or a `CombinedPlot` object whose main plot is used.
#' @param plotWidth Width of the exported plot.
#' If `NULL`, the default export width option `exportWidth` is used.
#' @param unit Unit of `plotWidth`: one of `"cm"`, `"in"`, `"mm"` or `"px"`.
#' If `NULL`, the default export width option `exportUnits` is used.
#' @param dpi Resolution in dots per inch; used to convert `plotWidth` to
#'   physical inches when `unit = "px"`.
#' If `NULL`, the default export DPI option `exportDpi` is used.
#' @param legendFractionHorizontal Maximum fraction of the total plot width available to
#'   legends on the left or right side. Ignored for top/bottom legends.
#' @param legendFractionVertical Maximum fraction of the total plot width available to
#'   legends on the top or bottom side. Ignored for left/right and inside legends.
#'
#' @return A single integer, the maximum number of characters per legend line
#'   (at least 1).
#'
#' @examples
#' df <- data.frame(
#'   time = 1:10,
#'   concentration = (1:10)^2,
#'   output = rep("Organism|PeripheralVenousBlood|Aciclovir|Plasma", 10)
#' )
#' p <- plotTimeProfile(
#'   data = df,
#'   mapping = aes(x = time, y = concentration, color = output)
#' )
#' # legend on the right may use 40% of an 8 cm wide plot
#' legendWidthFromPlotDimensions(p, plotWidth = 8, unit = "cm")
#'
#' # bottom legend may use the full plot width
#' pBottom <- p + theme(legend.position = "bottom")
#' legendWidthFromPlotDimensions(pBottom, plotWidth = 8, unit = "cm")
#'
#' @export
#' @family legend helpers
legendWidthFromPlotDimensions <- function(
  plotObject,
  plotWidth = NULL,
  unit = NULL,
  dpi = NULL,
  legendFractionHorizontal = 0.4,
  legendFractionVertical = 1
) {
  if (inherits(plotObject, "CombinedPlot")) {
    plotObject <- plotObject$plotObject
  }
  checkmate::assertClass(plotObject, "gg")
  plotWidth <- plotWidth %||%
    getOspsuite.plots.option(optionKey = OptionKeys$exportWidth)
  unit <- unit %||%
    getOspsuite.plots.option(optionKey = OptionKeys$exportUnits)
  unit <- tolower(unit)
  dpi <- dpi %||%
    getOspsuite.plots.option(optionKey = OptionKeys$exportDpi)

  checkmate::assertNumber(plotWidth, lower = 0)
  checkmate::assertChoice(unit, choices = c("cm", "in", "mm", "px"))
  checkmate::assertNumber(dpi, lower = 1)
  checkmate::assertNumber(legendFractionHorizontal, lower = 0)
  checkmate::assertNumber(legendFractionVertical, lower = 0)

  # Resolve the theme used for rendering; complete per-plot themes override
  # the global theme, while incomplete themes inherit from it.
  themeOfPlot <- ggplot2::theme_get() + plotObject$theme
  legendPosition <- themeOfPlot$legend.position
  fontSize <- .legendTextFontSize(themeOfPlot)

  legendFraction <- legendFractionHorizontal
  if (legendPosition %in% c("top", "bottom")) {
    legendFraction <- legendFractionVertical
  }

  plotWidthInches <- switch(
    unit,
    cm = plotWidth / 2.54,
    "in" = plotWidth,
    mm = plotWidth / 25.4,
    px = plotWidth / dpi
  )
  availableWidthInches <- plotWidthInches * legendFraction
  avgCharWidth <- 0.55
  characterWidthInches <- avgCharWidth * fontSize / 72
  maxWidth <- floor(availableWidthInches / characterWidthInches)
  maxWidth <- max(as.integer(maxWidth), 1L)

  return(maxWidth)
}

#' Wrap long legend entries based on the plot dimensions
#'
#' @description
#' Convenience wrapper around [wrapLegendLabels()] and
#' [legendWidthFromPlotDimensions()]: derives the maximum number of characters
#' per line from the exported plot dimensions and applies the wrapping to the
#' plot legend in one call.
#'
#' @param plotObject A `ggplot` object, e.g. created by [plotTimeProfile()],
#'   or a `CombinedPlot` object whose main plot is updated.
#' @param plotWidth Width of the exported plot.
#' If `NULL`, the default export width option `exportWidth` is used.
#' @param unit Unit of `plotWidth`: one of `"cm"`, `"in"`, `"mm"` or `"px"`.
#' If `NULL`, the default export width option `exportUnits` is used.
#' @param dpi Resolution in dots per inch; used to convert `plotWidth` to
#'   physical inches when `unit = "px"`.
#' If `NULL`, the default export DPI option `exportDpi` is used.
#' @param ... Additional arguments passed on to
#'   [legendWidthFromPlotDimensions()] (`legendFractionHorizontal`, `legendFractionVertical`).
#'
#' @return The `ggplot` object with wrapped legend labels (the input object is
#'   not modified).
#'
#' @examples
#' df <- data.frame(
#'   time = rep(seq(1, 24), 2),
#'   concentration = c(dexp(1:24, 0.2), dexp(1:24, 0.1)),
#'   output = rep(c(
#'     "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
#'     "Organism|PeripheralVenousBlood|Aciclovir|Muscle (Muscle Interstitial)"
#'   ), each = 24)
#' )
#' p <- plotTimeProfile(
#'   data = df,
#'   mapping = aes(x = time, y = concentration, color = output)
#' )
#' # wrap legend entries to fit a bottom legend on an 8 cm wide plot
#' pWrapped <- wrapLegend(
#'   p + theme(legend.position = "bottom"),
#'   plotWidth = 8,
#'   unit = "cm"
#' )
#'
#' @export
#' @family legend helpers
wrapLegend <- function(
  plotObject,
  plotWidth = NULL,
  unit = NULL,
  dpi = NULL,
  ...
) {
  maxWidth <- legendWidthFromPlotDimensions(
    plotObject,
    plotWidth = plotWidth,
    unit = unit,
    dpi = dpi,
    ...
  )
  return(wrapLegendLabels(plotObject, maxWidth = maxWidth))
}

# Legend wrapping internals --------------------------

#' Aesthetics whose scale labels can appear in a plot legend
#' @keywords internal
.legendAesthetics <- c("colour", "fill", "shape", "linetype")

#' Legend aesthetics that are mapped in the plot or one of its layers
#'
#' @param plotObject A `ggplot` object.
#'
#' @return Character vector with legend aesthetics mapped in the plot.
#' @keywords internal
.mappedLegendAesthetics <- function(plotObject) {
  mappings <- c(
    list(plotObject$mapping),
    lapply(plotObject$layers, `[[`, "mapping")
  )
  aestheticNames <- unique(unlist(lapply(
    mappings,
    function(mapping) ggplot2::standardise_aes_names(names(mapping))
  )))
  return(aestheticNames[
    sub("_ggnewscale_[0-9]+$", "", aestheticNames) %in% .legendAesthetics
  ])
}

#' Resolve the labels displayed by a scale for given breaks
#'
#' Handles the possible `labels` specifications of a scale: a function, a
#' named or unnamed character vector, a waiver or `NULL`.
#'
#' @param existingLabels The `labels` field of a scale (function, character,
#'   waiver or `NULL`).
#' @param breaks The breaks of the scale.
#'
#' @return Character vector of displayed labels for `breaks`.
#' @keywords internal
.resolveScaleLabels <- function(existingLabels, breaks) {
  if (is.function(existingLabels)) {
    return(existingLabels(breaks))
  }
  pos <- attr(breaks, "pos")
  breaks <- as.character(breaks)
  if (is.character(existingLabels)) {
    if (!is.null(names(existingLabels))) {
      mappedLabels <- unname(existingLabels[breaks])
      return(ifelse(is.na(mappedLabels), breaks, mappedLabels))
    }
    if (!is.null(pos)) {
      return(existingLabels[pos])
    }
    if (length(existingLabels) == length(breaks)) {
      return(existingLabels)
    }
  }
  return(breaks)
}

#' Wrap strings to a maximum number of characters per line
#'
#' Words are wrapped with [stringr::str_wrap()] and
#' lines are joined with `"\n"`.
#'
#' @param labels Character vector of labels to wrap.
#' @param maxWidth Maximum number of characters per line.
#'
#' @return Character vector of wrapped labels, `NA` labels are kept.
#' @keywords internal
#' @importFrom stringr str_wrap
.wrapLegendText <- function(labels, maxWidth) {
  if (length(labels) == 0 || is.expression(labels)) {
    return(labels)
  }
  wrappedLabels <- vapply(
    as.character(labels),
    function(label) {
      if (is.na(label)) {
        return(NA_character_)
      }
      labelLines <- stringr::str_wrap(
        label,
        width = maxWidth,
        # Option to split after `|` in long output paths
        whitespace_only = FALSE
      )
      return(paste(labelLines, collapse = "\n"))
    },
    character(1),
    USE.NAMES = FALSE
  )
  return(wrappedLabels)
}

#' Create a scale with wrapped labels for a legend aesthetic
#'
#' The scale type matches the one the plot would get at build time, so adding
#' it does not change the appearance of the plot.
#'
#' @param aesthetic One of `"colour"`, `"fill"`, `"shape"` or `"linetype"`.
#' @param labels A `labels` function for the scale.
#' @param ospStyle If `TRUE`, the OSPSuite scales are used.
#'
#' @return A `ggplot2` scale.
#' @keywords internal
.legendScaleWithWrappedLabels <- function(aesthetic, labels, ospStyle) {
  return(switch(
    aesthetic,
    colour = if (ospStyle) {
      scale_colour_osp(labels = labels)
    } else {
      ggplot2::scale_colour_discrete(labels = labels)
    },
    fill = if (ospStyle) {
      scale_fill_osp(labels = labels)
    } else {
      ggplot2::scale_fill_discrete(labels = labels)
    },
    shape = if (ospStyle) {
      scale_shape_osp(labels = labels)
    } else {
      ggplot2::scale_shape(labels = labels)
    },
    linetype = ggplot2::scale_linetype(labels = labels)
  ))
}

#' Legend text font size in points
#'
#' @param themeOfPlot A complete theme object.
#'
#' @return Numeric font size in points.
#' @keywords internal
.legendTextFontSize <- function(themeOfPlot) {
  legendText <- ggplot2::calc_element("legend.text", themeOfPlot)
  fontSize <- legendText$size
  if (!is.numeric(fontSize)) {
    fontSize <- 0.8 * ggplot2::calc_element("text", themeOfPlot)$size
  }
  return(as.numeric(fontSize))
}
