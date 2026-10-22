#' Create a ggplot with an optional watermark
#'
#' This function creates a ggplot object and adds a watermark if the watermark option is enabled.
#' The watermark can be customized with various options such as position, angle, font size, color, and transparency.
#'
#' @param ... Arguments to be passed to `ggplot()`, such as data and aesthetics. This allows for flexibility in creating different types of plots.
#' @param watermark Optional watermark text for this plot. If `NULL`, the session option is used at print time.
#' @param watermarkFormat Optional list of watermark formatting values (`x`, `y`, `angle`, `fontsize`, `color`, `alpha`).
#'   If provided, it is merged with current watermark format options at print time.
#' @param watermarkEnabled Optional logical flag controlling watermark visibility for this plot.
#'   If `NULL`, the session option is used at print time.
#'
#' @return A ggplot object, which may have the class "ggWatermark" if the watermark is enabled. The object can be printed or further modified as needed.
#'
#' @details
#' If the watermark feature is enabled, the resulting ggplot object will include a watermark overlay when printed.
#' The watermark's properties are determined by options set in the Ospsuite plotting configuration.
#'
#' The following options can be used to customize the watermark:
#' - `watermarkLabel`: Text to be displayed as the watermark.
#' - `watermarkFormat`: A list with the following entries:
#'   - `x`: The x-coordinate for the watermark's position on the plot.
#'   - `y`: The y-coordinate for the watermark's position on the plot.
#'   - `angle`: The angle at which the watermark text is displayed (in degrees).
#'   - `fontsize`: The size of the font for the watermark text.
#'   - `color`: The color of the watermark text, specified in a valid color format (e.g., "red", "#FF0000").
#'   - `alpha`: The transparency level of the watermark text, ranging from 0 (completely transparent) to 1 (completely opaque).
#'
#' @examples
#' # Example usage with watermark enabled (watermark is enabled by default)
#' plotWithWatermark <- ggplotWithWatermark(data = mtcars, aes(x = wt, y = mpg)) +
#'   geom_point()
#' print(plotWithWatermark)
#'
#' # Example usage with watermark disabled
#' setOspsuite.plots.option(optionKey = OptionKeys$watermarkEnabled, value = FALSE)
#' plotWithoutWatermark <- ggplotWithWatermark(data = mtcars, aes(x = wt, y = mpg)) +
#'   geom_point()
#' print(plotWithoutWatermark)
#' # Reset options
#' setOspsuite.plots.option(optionKey = OptionKeys$watermarkEnabled, value = TRUE)
#'
#' # Example usage with customized watermark
#' setOspsuite.plots.option(optionKey = OptionKeys$watermarkLabel, value = "Custom Label")
#' watermarkFormat <- getOspsuite.plots.option(optionKey = OptionKeys$watermarkFormat)
#' watermarkFormat$color <- "red"
#' setOspsuite.plots.option(optionKey = OptionKeys$watermarkFormat, value = watermarkFormat)
#' plotWithCustomizedWatermark <- ggplotWithWatermark(data = mtcars, aes(x = wt, y = mpg)) +
#'   geom_point()
#' print(plotWithCustomizedWatermark)
#' # Reset options
#' setOspsuite.plots.option(
#'   optionKey = OptionKeys$watermarkFormat,
#'   value = getDefaultOptions()[[OptionKeys$watermarkFormat]]
#' )
#' setOspsuite.plots.option(
#'   optionKey = OptionKeys$watermarkLabel,
#'   value = getDefaultOptions()[[OptionKeys$watermarkLabel]]
#' )
#' @family watermark
#' @export
ggplotWithWatermark <- function(
  ...,
  watermark = NULL,
  watermarkFormat = NULL,
  watermarkEnabled = NULL
) {
  checkmate::assertCharacter(watermark, len = 1, null.ok = TRUE)
  checkmate::assertList(watermarkFormat, null.ok = TRUE)
  checkmate::assertFlag(watermarkEnabled, null.ok = TRUE)

  plotObject <- ggplot(...)

  # Set new class to overwrite the print method for the ggplot object
  class(plotObject) <- c("ggWatermark", class(plotObject))
  attr(plotObject, "ospWatermark") <- list(
    watermark = watermark,
    watermarkFormat = watermarkFormat,
    watermarkEnabled = watermarkEnabled
  )

  return(plotObject)
}

#' @keywords internal
#' @noRd
.getStoredWatermarkSettings <- function(plotObject) {
  watermarkSettings <- attr(plotObject, "ospWatermark")

  if (is.null(watermarkSettings)) {
    return(list(
      watermark = NULL,
      watermarkFormat = NULL,
      watermarkEnabled = NULL
    ))
  }

  return(watermarkSettings)
}

#' @keywords internal
#' @noRd
.resolveWatermarkSettings <- function(
  plotObject,
  watermark = NULL,
  watermarkFormat = NULL,
  watermarkEnabled = NULL
) {
  storedSettings <- .getStoredWatermarkSettings(plotObject)

  resolvedWatermarkEnabled <- watermarkEnabled %||%
    storedSettings$watermarkEnabled %||%
    .getWatermarkEnabled()

  if (!resolvedWatermarkEnabled) {
    return(list(
      watermark = NULL,
      watermarkFormat = NULL,
      watermarkEnabled = FALSE
    ))
  }

  resolvedWatermark <- watermark %||%
    storedSettings$watermark %||%
    getOspsuite.plots.option(optionKey = OptionKeys$watermarkLabel)

  defaultWatermarkFormat <- getOspsuite.plots.option(
    optionKey = OptionKeys$watermarkFormat
  )
  userWatermarkFormat <- watermarkFormat %||% storedSettings$watermarkFormat
  resolvedWatermarkFormat <- utils::modifyList(
    defaultWatermarkFormat,
    userWatermarkFormat %||% list()
  )

  return(list(
    watermark = resolvedWatermark,
    watermarkFormat = resolvedWatermarkFormat,
    watermarkEnabled = TRUE
  ))
}

#' @keywords internal
#' @noRd
.validateWatermarkSettings <- function(watermark, watermarkFormat) {
  checkmate::assertCharacter(watermark, len = 1, null.ok = FALSE)
  checkmate::assertList(watermarkFormat, null.ok = FALSE)
  checkmate::assertNumber(watermarkFormat$x, lower = 0, upper = 1)
  checkmate::assertNumber(watermarkFormat$y, lower = 0, upper = 1)
  checkmate::assertNumber(watermarkFormat$angle, lower = 0, upper = 360)
  checkmate::assertNumber(watermarkFormat$fontsize, lower = 0)
  checkmate::assertCharacter(watermarkFormat$color, len = 1)
  checkmate::assertNumber(watermarkFormat$alpha, lower = 0, upper = 1)
}

#' @keywords internal
#' @noRd
.getAnnotationBounds <- function(plotObject) {
  panelParams <- ggplot2::ggplot_build(plotObject)$layout$panel_params[[1]]

  getDataRange <- function(panelAxis, transformedRange) {
    scaleObject <- panelAxis$scale
    if (is.null(scaleObject)) {
      return(transformedRange)
    }

    inverseTransform <- NULL

    # ggplot2 <= 3.x commonly exposes transformation objects via `$trans`.
    if (is.list(scaleObject$trans) && is.function(scaleObject$trans$inverse)) {
      inverseTransform <- scaleObject$trans$inverse
    }

    # ggplot2 >= 4.x may expose transforms via `$transform`.
    if (is.null(inverseTransform) && !is.null(scaleObject$transform)) {
      transformObject <- scaleObject$transform
      if (is.list(transformObject) && is.function(transformObject$inverse)) {
        inverseTransform <- transformObject$inverse
      }

      # Some scales store only a transform closure with no inverse accessor.
      if (is.null(inverseTransform) && is.function(transformObject)) {
        inverseFromAttr <- attr(transformObject, "inverse", exact = TRUE)
        if (is.function(inverseFromAttr)) {
          inverseTransform <- inverseFromAttr
        }
      }
    }

    if (is.null(inverseTransform)) {
      return(transformedRange)
    }

    return(inverseTransform(transformedRange))
  }

  xRange <- panelParams$x.range %||% c(-Inf, Inf)
  yRange <- panelParams$y.range %||% c(-Inf, Inf)

  xDataRange <- getDataRange(panelParams$x, xRange)
  yDataRange <- getDataRange(panelParams$y, yRange)

  return(list(
    xmin = xDataRange[[1]],
    xmax = xDataRange[[2]],
    ymin = yDataRange[[1]],
    ymax = yDataRange[[2]]
  ))
}


#' Helper function to get watermark enabled status.
#' @keywords internal
.getWatermarkEnabled <- function() {
  watermarkStatus <- getOption(
    "ospsuite.plots.watermarkEnabled",
    default = TRUE
  )
  checkmate::assert_flag(watermarkStatus)
  return(watermarkStatus)
}

#' Print method for ggWatermark objects
#'
#' This function customizes the printing of ggplot objects with the class "ggWatermark" by adding a watermark.
#'
#' @param x A ggWatermark object created by `ggplotWithWatermark()`.
#' @param ... Additional arguments to be passed to the print method, allowing for further customization of the output.
#' @return A ggplot object with a watermark drawn on it. The watermark is displayed according to the specified options.
#'
#' @family watermark
#' @export
print.ggWatermark <- function(x, ...) {
  watermarkSettings <- .resolveWatermarkSettings(plotObject = x)
  watermarkEnabled <- watermarkSettings$watermarkEnabled

  if (watermarkEnabled) {
    # Add watermark overlay when watermark is enabled
    print(addWatermark(
      x,
      watermark = watermarkSettings$watermark,
      watermarkFormat = watermarkSettings$watermarkFormat,
      watermarkEnabled = TRUE
    ))
  } else {
    # Remove custom class so default ggplot print method is used.
    class(x) <- setdiff(class(x), "ggWatermark")
    print(x)
  }
}
#' Add a watermark to a ggplot object
#'
#' This function adds a customizable watermark to a ggplot object. The watermark can be configured with various options such as position, angle, font size, color, and transparency.
#'
#' @param plotObject A ggplot object to which the watermark will be added.
#' @param watermark Optional watermark text.
#' @param watermarkFormat Optional watermark format list.
#' @param watermarkEnabled Optional watermark enabled flag.
#'
#' @return A ggplot object with a watermark drawn on it. The watermark is displayed according to the specified options.
#'
#' @examples
#' \dontrun{
#' # Example usage (watermark enabled by default)
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point()
#' p_with_watermark <- addWatermark(p)
#' print(p_with_watermark)
#'
#' # Example of customizing the watermark
#' setOspsuite.plots.option(optionKey = OptionKeys$watermarkLabel, value = "Custom Watermark")
#' watermarkFormat <- getOspsuite.plots.option(optionKey = OptionKeys$watermarkFormat)
#' watermarkFormat$x <- 0.5 # Centered horizontally
#' watermarkFormat$y <- 0.5 # Centered vertically
#' watermarkFormat$angle <- 45 # Rotated 45 degrees
#' watermarkFormat$fontsize <- 6 # Font size 6
#' watermarkFormat$color <- "blue" # Blue color
#' watermarkFormat$alpha <- 0.5 # 50% transparency
#' setOspsuite.plots.option(optionKey = OptionKeys$watermarkFormat, value = watermarkFormat)
#'
#' # Create plot with customized watermark
#' p_custom <- addWatermark(p)
#' print(p_custom)
#' }
#'
#' @export
#'
#' @family watermark
addWatermark <- function(
  plotObject,
  watermark = NULL,
  watermarkFormat = NULL,
  watermarkEnabled = NULL
) {
  # initialize variables to avoid check messages
  x <- y <- label <- NULL

  checkmate::assert_class(plotObject, classes = "gg")
  checkmate::assertCharacter(watermark, len = 1, null.ok = TRUE)
  checkmate::assertList(watermarkFormat, null.ok = TRUE)
  checkmate::assertFlag(watermarkEnabled, null.ok = TRUE)

  watermarkSettings <- .resolveWatermarkSettings(
    plotObject = plotObject,
    watermark = watermark,
    watermarkFormat = watermarkFormat,
    watermarkEnabled = watermarkEnabled
  )

  # if watermark is not enabled return unchanged object
  if (!watermarkSettings$watermarkEnabled) {
    return(plotObject)
  }

  watermarkLabel <- watermarkSettings$watermark
  watermarkOptions <- watermarkSettings$watermarkFormat

  # Validate watermark options
  .validateWatermarkSettings(
    watermark = watermarkLabel,
    watermarkFormat = watermarkOptions
  )

  annotationBounds <- .getAnnotationBounds(plotObject)

  plotWithWatermark <- plotObject +
    annotation_custom(
      grob = grid::textGrob(
        label = watermarkLabel,
        x = grid::unit(watermarkOptions$x, "npc"),
        y = grid::unit(watermarkOptions$y, "npc"),
        just = c("center", "center"),
        rot = watermarkOptions$angle,
        gp = grid::gpar(
          col = watermarkOptions$color,
          alpha = watermarkOptions$alpha,
          fontsize = watermarkOptions$fontsize
        )
      ),
      xmin = annotationBounds$xmin,
      xmax = annotationBounds$xmax,
      ymin = annotationBounds$ymin,
      ymax = annotationBounds$ymax
    )

  # Remove custom class to avoid recursive watermark application on print.
  class(plotWithWatermark) <- setdiff(class(plotWithWatermark), "ggWatermark")

  return(plotWithWatermark)
}


#' Create plot function for ggWatermark.
#'
#' This method allows for the ggWatermark object to be plotted using the standard print method.
#'
#' @param x A ggWatermark object to be printed.
#' @param ... Additional arguments to be passed to the print method.
#'
#' @family watermark
#' @export
plot.ggWatermark <- function(x, ...) {
  print(x, ...)
}
