# set the default theme

**\[deprecated\]**

set the OSPSuite theme as the global `ggplot2` theme for the whole
session via
[`ggplot2::theme_set()`](https://ggplot2.tidyverse.org/reference/get_theme.html).

`ospsuite.plots` plot functions now apply
[`theme_osp()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/theme_osp.md)
per plot, so this global mutation is no longer needed for them. Use
`plot + theme_osp()` to style an individual (non-`ospsuite.plots`) plot
instead.

## Usage

``` r
setDefaultTheme()
```

## Value

invisibly return the previous theme so you can easily save it, then
later restore it.

## See also

Other setDefault functions:
[`colorMaps`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/colorMaps.md),
[`getDefaultGeomAttributes()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/getDefaultGeomAttributes.md),
[`getDefaultOptions()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/getDefaultOptions.md),
[`getOspsuite.plots.option()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/getOspsuite.plots.option.md),
[`resetDefaultColorMapDistinct()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/resetDefaultColorMapDistinct.md),
[`resetDefaultTheme()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/resetDefaultTheme.md),
[`resetDefaults()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/resetDefaults.md),
[`setDefaultColorMapDistinct()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/setDefaultColorMapDistinct.md),
[`setDefaults()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/setDefaults.md),
[`setOspsuite.plots.option()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/setOspsuite.plots.option.md),
[`theme_osp()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/theme_osp.md)

## Examples

``` r
if (FALSE) { # \dontrun{
oldTheme <- setDefaultTheme()

# Create a plot with the new theme
p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()
print(p)

# Restore previous theme
resetDefaultTheme(oldTheme)
} # }
```
