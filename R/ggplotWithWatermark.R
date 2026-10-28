#' Create a ggplot with an optional watermark
#'
#' This function creates a ggplot object and adds a watermark if enabled.
#' The watermark can be customized with various options such as position, angle, font size, color, and transparency.
#'
#' @param ... Arguments to be passed to `ggplot()`, such as data and aesthetics. This allows for flexibility in creating different types of plots.
#' @param watermark Optional watermark text for this plot. If `NULL`, the session option is used at plot creation.
#'   Set to `FALSE` to disable the watermark for this plot.
#' @param watermarkFormat Optional list of watermark formatting values (`x`, `y`, `angle`, `fontsize`, `color`, `alpha`).
#'   If provided, it is merged with current watermark format options.
#'
#' @return A ggplot object that can be printed or further modified as needed.
#'
#' @details
#' If the watermark feature is enabled, the resulting ggplot object includes
#' a watermark annotation as part of the plot object.
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
#' plotWithoutWatermark <- ggplotWithWatermark(data = mtcars, aes(x = wt, y = mpg)) +
#'   geom_point(),
#'   watermark = FALSE
#' print(plotWithoutWatermark)
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
  watermarkFormat = NULL
) {
  if (!is.null(watermark) && !identical(watermark, FALSE)) {
    checkmate::assertCharacter(watermark, len = 1)
  }
  checkmate::assertList(watermarkFormat, null.ok = TRUE)

  plotObject <- ggplot(...)
  return(addWatermark(
    plotObject = plotObject,
    watermark = watermark,
    watermarkFormat = watermarkFormat
  ))
}

#' Resolve effective watermark settings for a plot.
#'
#' @param watermark Optional watermark label override.
#' @param watermarkFormat Optional watermark format override list.
#'
#' @return List with resolved `watermark`, `watermarkFormat`, and `watermarkEnabled`.
#' @keywords internal
#' @noRd
.resolveWatermarkSettings <- function(
  watermark = NULL,
  watermarkFormat = NULL
) {
  resolvedWatermarkEnabled <- !identical(watermark, FALSE)

  if (is.null(watermark) && resolvedWatermarkEnabled) {
    resolvedWatermarkEnabled <- .getWatermarkEnabled()
  }

  if (!resolvedWatermarkEnabled) {
    return(list(
      watermark = NULL,
      watermarkFormat = NULL,
      watermarkEnabled = FALSE
    ))
  }

  resolvedWatermark <- watermark %||%
    getOspsuite.plots.option(optionKey = OptionKeys$watermarkLabel)

  if (is.null(resolvedWatermark)) {
    return(list(
      watermark = NULL,
      watermarkFormat = NULL,
      watermarkEnabled = FALSE
    ))
  }

  defaultWatermarkFormat <- getOspsuite.plots.option(
    optionKey = OptionKeys$watermarkFormat
  )
  userWatermarkFormat <- watermarkFormat
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

#' Validate watermark label and format values.
#'
#' @param watermark Watermark label string.
#' @param watermarkFormat Watermark format list.
#'
#' @return Invisibly returns `TRUE` when all checks pass.
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

#' Internal geom drawing watermark text in panel NPC coordinates.
#' @keywords internal
#' @noRd
.geomWatermarkNpc <- ggplot2::ggproto(
  "GeomWatermarkNpc",
  ggplot2::Geom,
  required_aes = c(),
  default_aes = ggplot2::aes(),
  draw_key = ggplot2::draw_key_blank,
  draw_panel = function(
    data,
    panel_params,
    coord,
    label,
    x,
    y,
    angle,
    fontsize,
    colour,
    alpha
  ) {
    grid::textGrob(
      label = label,
      x = grid::unit(x, "npc"),
      y = grid::unit(y, "npc"),
      just = c("center", "center"),
      rot = angle,
      gp = grid::gpar(
        col = colour,
        alpha = alpha,
        fontsize = fontsize
      )
    )
  }
)

#' Build a watermark layer from resolved settings.
#'
#' @param watermark Watermark label string.
#' @param watermarkFormat Watermark format list.
#'
#' @return A ggplot2 layer object drawing the watermark.
#' @keywords internal
#' @noRd
.createWatermarkLayer <- function(watermark, watermarkFormat) {
  ggplot2::layer(
    geom = .geomWatermarkNpc,
    stat = "identity",
    position = "identity",
    data = data.frame(.watermark = 1),
    mapping = ggplot2::aes(),
    inherit.aes = FALSE,
    params = list(
      label = watermark,
      x = watermarkFormat$x,
      y = watermarkFormat$y,
      angle = watermarkFormat$angle,
      fontsize = watermarkFormat$fontsize,
      colour = watermarkFormat$color,
      alpha = watermarkFormat$alpha
    )
  )
}


#' Read global watermark-enabled option with validation.
#'
#' @return Logical watermark-enabled flag.
#' @keywords internal
#' @noRd
.getWatermarkEnabled <- function() {
  watermarkStatus <- getOption(
    "ospsuite.plots.watermarkEnabled",
    default = TRUE
  )
  checkmate::assert_flag(watermarkStatus)
  return(watermarkStatus)
}

#' Add a watermark to a ggplot object
#'
#' This function adds a customizable watermark to a ggplot object. The watermark can be configured with various options such as position, angle, font size, color, and transparency.
#'
#' @param plotObject A ggplot object to which the watermark will be added.
#' @param watermark Optional watermark text. If `NULL`, the session option is used.
#'   Set to `FALSE` to disable the watermark for this plot.
#' @param watermarkFormat Optional watermark format list.
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
  watermarkFormat = NULL
) {
  checkmate::assert_class(plotObject, classes = "gg")
  if (!is.null(watermark) && !identical(watermark, FALSE)) {
    checkmate::assertCharacter(watermark, len = 1)
  }
  checkmate::assertList(watermarkFormat, null.ok = TRUE)

  watermarkSettings <- .resolveWatermarkSettings(
    watermark = watermark,
    watermarkFormat = watermarkFormat
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

  plotWithWatermark <- plotObject +
    .createWatermarkLayer(
      watermark = watermarkLabel,
      watermarkFormat = watermarkOptions
    )

  return(plotWithWatermark)
}
