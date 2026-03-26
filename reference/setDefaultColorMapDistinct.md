# set the default color-map for discrete colors

Sets default color mappings for discrete color and fill aesthetics in
ggplot2. Each color map should be a vector of valid color values (hex
codes, color names, etc.).

## Usage

``` r
setDefaultColorMapDistinct(colorMapList = NULL)
```

## Arguments

- colorMapList:

  list of color-maps to be set

## Value

list with color-maps previously set

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
[`setDefaultShapeDiscrete()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/reference/setDefaultShapeDiscrete.md),
[`setDefaultTheme()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/reference/setDefaultTheme.md),
[`setDefaults()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/reference/setDefaults.md),
[`setOspsuite.plots.option()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/reference/setOspsuite.plots.option.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Set custom color maps
customColors <- list(
  c("#FF0000", "#00FF00", "#0000FF"), # RGB colors
  c("red", "green", "blue") # Named colors
)
oldColors <- setDefaultColorMapDistinct(customColors)

# Use default OSP color maps
setDefaultColorMapDistinct()

# Reset to previous colors
resetDefaultColorMapDistinct(oldColors)
} # }
```
