# sets the defaults for the OSPSuite.plots package

**\[deprecated\]**

Mutates global `ggplot2` state (theme, geom defaults and discrete color
options) for the whole session.

`ospsuite.plots` plot functions now apply the full OSPSuite styling per
plot, so this is no longer needed for them. To style individual
(non-`ospsuite.plots`) plots, compose the per-plot constructors instead:
`plot + theme_osp() + scale_colour_osp() + scale_fill_osp()`.

for detailed information see
`vignette("ospsuite.plots", package = "ospsuite.plots")`

## Usage

``` r
setDefaults(defaultOptions = list(), colorMapList = NULL)
```

## Arguments

- defaultOptions:

  list of options

- colorMapList:

  list of color maps

## Value

list of old settings which can be used to reset defaults with
[`resetDefaults()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/resetDefaults.md)

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
[`setDefaultTheme()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/setDefaultTheme.md),
[`setOspsuite.plots.option()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/setOspsuite.plots.option.md),
[`theme_osp()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/theme_osp.md)
