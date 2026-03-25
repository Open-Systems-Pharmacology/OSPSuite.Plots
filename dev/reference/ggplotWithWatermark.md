# Create a ggplot with an optional watermark

This function creates a ggplot object and adds a watermark if the
watermark option is enabled. The watermark can be customized with
various options such as position, angle, font size, color, and
transparency.

## Usage

``` r
ggplotWithWatermark(...)
```

## Arguments

- ...:

  Arguments to be passed to `ggplot()`, such as data and aesthetics.
  This allows for flexibility in creating different types of plots.

## Value

A ggplot object, which may have the class "ggWatermark" if the watermark
is enabled. The object can be printed or further modified as needed.

## Details

If the watermark feature is enabled, the resulting ggplot object will
include a watermark overlay when printed. The watermark's properties are
determined by options set in the Ospsuite plotting configuration.

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
[`addWatermark()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/addWatermark.md),
[`plot.ggWatermark()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plot.ggWatermark.md),
[`print.ggWatermark()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/print.ggWatermark.md)

## Examples

``` r
# Set watermark option first (required)
options(ospsuite.plots.watermarkEnabled = TRUE)

# Example usage with watermark enabled
plotWithWatermark <- ggplotWithWatermark(data = mtcars, aes(x = wt, y = mpg)) +
  geom_point()
print(plotWithWatermark)


# Example usage with watermark disabled
setOspsuite.plots.option(optionKey = OptionKeys$watermarkEnabled, value = FALSE)
plotWithoutWatermark <- ggplotWithWatermark(data = mtcars, aes(x = wt, y = mpg)) +
  geom_point()
print(plotWithoutWatermark)

# Reset options
setOspsuite.plots.option(optionKey = OptionKeys$watermarkEnabled, value = TRUE)

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
