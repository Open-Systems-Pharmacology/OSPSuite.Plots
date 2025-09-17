#' Create a ggplot with an optional watermark
#'
#' This function creates a ggplot object and adds a watermark if the watermark option is enabled.
#' The watermark can be customized with various options such as position, angle, font size, color, and transparency.
#'
#' @param ... Arguments to be passed to `ggplot()`, such as data and aesthetics. This allows for flexibility in creating different types of plots.
#'
#' @return A ggplot object, which may have the class "ggWatermark" if the watermark is enabled. The object can be printed or further modified as needed.
#'
#' @details
#' If the watermark feature is enabled, the resulting ggplot object will include a watermark overlay when printed.
#' The watermark's properties are determined by options set in the Ospsuite plotting configuration.
#'
#' @examples
#' # Example usage with watermark enabled
#' plot_with_watermark <- ggplotWithWatermark(data = mtcars, aes(x = wt, y = mpg)) +
#'   geom_point()
#' print(plot_with_watermark)
#'
#'
#' @export
ggplotWithWatermark <- function(...) {
  plotObject <- ggplot(...)

  # Return original ggplot if watermark is not enabled
  if (!getOspsuite.plots.option(optionKey = OptionKeys$watermark_enabled))
    return(plotObject)

  # Overwrite the print method for the ggplot object
  class(plotObject) <- c("ggWatermark", class(plotObject))

  return(plotObject)
}

#' Print method for ggWatermark objects
#'
#' This function customizes the printing of ggplot objects with the class "ggWatermark" by adding a watermark.
#'
#' @param x A ggWatermark object created by `ggplotWithWatermark()`.
#' @param ... Additional arguments to be passed to the print method, allowing for further customization of the output.
#' @return A ggplot object with a watermark drawn on it. The watermark is displayed according to the specified options.
#'
#' @details
#' The watermark is drawn using the `geom_text` function from `ggplot2`, and its appearance is controlled by options such as position, angle, font size, color, and transparency.
#'
#' @export
print.ggWatermark <- function(x, ...) {
  watermarkLabel <- getOspsuite.plots.option(optionKey = OptionKeys$watermark_label)
  watermarkOptions <- getOspsuite.plots.option(optionKey = OptionKeys$watermark_format)

  print(cowplot::ggdraw(x) +
          geom_text(
            data = data.frame(x = watermarkOptions$x,
                              y = watermarkOptions$y,
                              label = watermarkLabel),
            aes(x, y, label = label),
            hjust = 0.5,
            vjust = 0.5,
            angle = watermarkOptions$angle,
            size = watermarkOptions$fontsize,
            color = watermarkOptions$color,
            alpha = watermarkOptions$alpha,
            inherit.aes = FALSE
          ))
}

#' Create plot function for ggWatermark.
#'
#' This method allows for the ggWatermark object to be plotted using the standard print method.
#'
#' @param x A ggWatermark object to be printed.
#' @param ... Additional arguments to be passed to the print method.
#'
#' @export
plot.ggWatermark <- function(x, ...) {
  print(x,...)
}
