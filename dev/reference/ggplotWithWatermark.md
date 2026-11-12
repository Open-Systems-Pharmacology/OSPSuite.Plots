# Create a ggplot with an optional watermark

This function creates a ggplot object and adds a watermark if enabled.
The watermark can be customized with various options such as position,
angle, font size, color, and transparency.

## Usage

``` r
ggplotWithWatermark(..., watermark = NULL, watermarkFormat = NULL)
```

## Arguments

- ...:

  Arguments to be passed to
  [`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html),
  such as data and aesthetics. This allows for flexibility in creating
  different types of plots.

- watermark:

  Optional watermark text for this plot. If `NULL`, the session option
  is used at plot creation. Set to `FALSE` to disable the watermark for
  this plot.

- watermarkFormat:

  Optional list of watermark formatting values (`x`, `y`, `angle`,
  `fontsize`, `color`, `alpha`). If provided, it is merged with current
  watermark format options.

## Value

A ggplot object that can be printed or further modified as needed.

## Details

If the watermark feature is enabled, the resulting ggplot object
includes a watermark annotation as part of the plot object. The
watermark's properties are determined by options set in the Ospsuite
plotting configuration.

The following options can be used to customize the watermark:

- `watermarkLabel`: Text to be displayed as the watermark.

- `watermarkFormat`: A list with the following entries:

  - `x`: The x-coordinate for the watermark's position on the plot.

  - `y`: The y-coordinate for the watermark's position on the plot.

  - `angle`: The angle at which the watermark text is displayed (in
    degrees).

  - `fontsize`: The size of the font for the watermark text.

  - `color`: The color of the watermark text, specified in a valid color
    format (e.g., "red", "#FF0000").

  - `alpha`: The transparency level of the watermark text, ranging from
    0 (completely transparent) to 1 (completely opaque).

## See also

Other watermark:
[`addWatermark()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/addWatermark.md)

## Examples

``` r
# Example usage with watermark enabled (watermark is enabled by default)
plotWithWatermark <- ggplotWithWatermark(data = mtcars, aes(x = wt, y = mpg)) +
  geom_point()
print(plotWithWatermark)


# Example usage with watermark disabled
plotWithoutWatermark <- ggplotWithWatermark(
  data = mtcars,
  aes(x = wt, y = mpg),
  watermark = FALSE
) +
  geom_point()
print(plotWithoutWatermark)


# Example usage with customized watermark
setOspsuite.plots.option(optionKey = OptionKeys$watermarkLabel, value = "Custom Label")
watermarkFormat <- getOspsuite.plots.option(optionKey = OptionKeys$watermarkFormat)
watermarkFormat$color <- "red"
setOspsuite.plots.option(optionKey = OptionKeys$watermarkFormat, value = watermarkFormat)
plotWithCustomizedWatermark <- ggplotWithWatermark(data = mtcars, aes(x = wt, y = mpg)) +
  geom_point()
print(plotWithCustomizedWatermark)

# Reset options
setOspsuite.plots.option(
  optionKey = OptionKeys$watermarkFormat,
  value = getDefaultOptions()[[OptionKeys$watermarkFormat]]
)
setOspsuite.plots.option(
  optionKey = OptionKeys$watermarkLabel,
  value = getDefaultOptions()[[OptionKeys$watermarkLabel]]
)
```
