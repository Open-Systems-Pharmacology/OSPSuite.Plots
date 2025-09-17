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
#' The following options can be used to customize the watermark:
#' - `watermark_label`: Text to be displayed as the watermark.
#' - `watermark_format`: A list with the following entries:
#'   - `x`: The x-coordinate for the watermark's position on the plot.
#'   - `y`: The y-coordinate for the watermark's position on the plot.
#'   - `angle`: The angle at which the watermark text is displayed (in degrees).
#'   - `fontsize`: The size of the font for the watermark text.
#'   - `color`: The color of the watermark text, specified in a valid color format (e.g., "red", "#FF0000").
#'   - `alpha`: The transparency level of the watermark text, ranging from 0 (completely transparent) to 1 (completely opaque).
#'
#' @examples
#' # Example usage with watermark enabled
#' plotWithWatermark <- ggplotWithWatermark(data = mtcars, aes(x = wt, y = mpg)) +
#'   geom_point()
#' print(plotWithWatermark)
#'
#' # Example usage with watermark disabled
#' setOspsuite.plots.option(optionKey = OptionKeys$watermark_enabled, value = FALSE)
#' plotWithoutWatermark <- ggplotWithWatermark(data = mtcars, aes(x = wt, y = mpg)) +
#'   geom_point()
#' print(plotWithoutWatermark)
#' # Reset options
#' setOspsuite.plots.option(optionKey = OptionKeys$watermark_enabled, value = TRUE)
#'
#' # Example usage with customized watermark
#' setOspsuite.plots.option(optionKey = OptionKeys$watermark_label, value = 'Custom Label')
#' watermark_format <- getOspsuite.plots.option(optionKey = OptionKeys$watermark_format)
#' watermark_format$color <- 'red'
#' setOspsuite.plots.option(optionKey = OptionKeys$watermark_format, value = watermark_format)
#' plotWithCustomizedWatermark <- ggplotWithWatermark(data = mtcars, aes(x = wt, y = mpg)) +
#'   geom_point()
#' print(plotWithCustomizedWatermark)
#' # Reset options
#' setOspsuite.plots.option(optionKey = OptionKeys$watermark_format,
#'   value = getDefaultOptions()[[OptionKeys$watermark_format]])
#' setOspsuite.plots.option(optionKey = OptionKeys$watermark_label,
#'   value = getDefaultOptions()[[OptionKeys$watermark_label]])
#'
#' @export
ggplotWithWatermark <- function(...) {
  plotObject <- ggplot(...)

  # Return original ggplot if watermark is not enabled
  if (!getOspsuite.plots.option(optionKey = OptionKeys$watermark_enabled))
    return(plotObject)

  # Set new class to overwrite the print method for the ggplot object
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
