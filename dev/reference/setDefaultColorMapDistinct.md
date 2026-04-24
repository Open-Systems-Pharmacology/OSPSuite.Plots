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
[`Shapes`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/Shapes.md),
[`colorMaps`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/colorMaps.md),
[`geom_point_osp()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/geom_point_osp.md),
[`getDefaultGeomAttributes()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/getDefaultGeomAttributes.md),
[`getDefaultOptions()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/getDefaultOptions.md),
[`getOspsuite.plots.option()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/getOspsuite.plots.option.md),
[`resetDefaultColorMapDistinct()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/resetDefaultColorMapDistinct.md),
[`resetDefaultShapeDiscrete()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/resetDefaultShapeDiscrete.md),
[`resetDefaultTheme()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/resetDefaultTheme.md),
[`resetDefaults()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/resetDefaults.md),
[`scale_shape_osp()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/scale_shape_osp.md),
[`scale_shape_osp_identity()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/scale_shape_osp_identity.md),
[`scale_shape_osp_manual()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/scale_shape_osp_manual.md),
[`setDefaultShapeDiscrete()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/setDefaultShapeDiscrete.md),
[`setDefaultTheme()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/setDefaultTheme.md),
[`setDefaults()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/setDefaults.md),
[`setOspsuite.plots.option()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/setOspsuite.plots.option.md),
[`stat_qq_osp()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/stat_qq_osp.md)

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
