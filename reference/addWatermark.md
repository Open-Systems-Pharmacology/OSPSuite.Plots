# Add a watermark to a ggplot object

This function adds a customizable watermark to a ggplot object. The
watermark can be configured with various options such as position,
angle, font size, color, and transparency.

## Usage

``` r
addWatermark(plotObject)
```

## Arguments

- plotObject:

  A ggplot object to which the watermark will be added.

## Value

A ggplot object with a watermark drawn on it. The watermark is displayed
according to the specified options.

## See also

Other watermark:
[`ggplotWithWatermark()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/reference/ggplotWithWatermark.md),
[`plot.ggWatermark()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/reference/plot.ggWatermark.md),
[`print.ggWatermark()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/reference/print.ggWatermark.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Set watermark option first (required)
setOspsuite.plots.option(optionKey = OptionKeys$watermarkEnabled, value = TRUE)

# Example usage
p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()
p_with_watermark <- addWatermark(p)
print(p_with_watermark)

# Example of customizing the watermark
setOspsuite.plots.option(optionKey = OptionKeys$watermarkLabel, value = "Custom Watermark")
watermarkFormat <- getOspsuite.plots.option(optionKey = OptionKeys$watermarkFormat)
watermarkFormat$x <- 0.5 # Centered horizontally
watermarkFormat$y <- 0.5 # Centered vertically
watermarkFormat$angle <- 45 # Rotated 45 degrees
watermarkFormat$fontsize <- 6 # Font size 6
watermarkFormat$color <- "blue" # Blue color
watermarkFormat$alpha <- 0.5 # 50% transparency
setOspsuite.plots.option(optionKey = OptionKeys$watermarkFormat, value = watermarkFormat)

# Create plot with customized watermark
p_custom <- addWatermark(p)
print(p_custom)
} # }
```
