# set the default theme

set properties of the default theme for OSPSuite plots. This function
applies a custom theme based on theme_bw() with OSPSuite-specific
styling.

## Usage

``` r
setDefaultTheme()
```

## Value

invisibly return the previous theme so you can easily save it, then
later restore it.

## See also

Other setDefault functions:
[`Shapes`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/reference/Shapes.md),
[`colorMaps`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/reference/colorMaps.md),
[`geomPointUnicode()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/reference/geomPointUnicode.md),
[`getDefaultGeomAttributes()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/reference/getDefaultGeomAttributes.md),
[`getDefaultOptions()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/reference/getDefaultOptions.md),
[`getOspsuite.plots.option()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/reference/getOspsuite.plots.option.md),
[`resetDefaultColorMapDistinct()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/reference/resetDefaultColorMapDistinct.md),
[`resetDefaultShapeDiscrete()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/reference/resetDefaultShapeDiscrete.md),
[`resetDefaultTheme()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/reference/resetDefaultTheme.md),
[`resetDefaults()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/reference/resetDefaults.md),
[`setDefaultColorMapDistinct()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/reference/setDefaultColorMapDistinct.md),
[`setDefaultShapeDiscrete()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/reference/setDefaultShapeDiscrete.md),
[`setDefaults()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/reference/setDefaults.md),
[`setOspsuite.plots.option()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/reference/setOspsuite.plots.option.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Set watermark option first (required)
options(ospsuite.plots.watermarkEnabled = TRUE)
oldTheme <- setDefaultTheme()

# Create a plot with the new theme
p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()
print(p)

# Restore previous theme
resetDefaultTheme(oldTheme)
} # }
```
