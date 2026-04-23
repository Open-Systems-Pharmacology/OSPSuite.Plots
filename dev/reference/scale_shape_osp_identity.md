# OSP Shape Identity Scale

Identity scale for when data already contains OSP shape names. Use this
when your shape column contains values from `ospShapeNames` directly
(e.g., "circle", "diamond", "star"). Equivalent to
[`ggplot2::scale_shape_identity()`](https://ggplot2.tidyverse.org/reference/scale_identity.html).

## Usage

``` r
scale_shape_osp_identity(guide = "none", ...)
```

## Arguments

- guide:

  Guide for the legend. Use `"legend"` to show a legend, or `"none"` to
  hide it.

- ...:

  Passed to
  [`ggplot2::scale_shape_manual`](https://ggplot2.tidyverse.org/reference/scale_manual.html).

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
[`scale_shape_osp_manual()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/scale_shape_osp_manual.md),
[`setDefaultColorMapDistinct()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/setDefaultColorMapDistinct.md),
[`setDefaultShapeDiscrete()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/setDefaultShapeDiscrete.md),
[`setDefaultTheme()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/setDefaultTheme.md),
[`setDefaults()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/setDefaults.md),
[`setOspsuite.plots.option()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/setOspsuite.plots.option.md)

## Examples

``` r
library(ggplot2)
df <- data.frame(x = 1:3, y = 1:3, shape = c("circle", "diamond", "star"))
ggplot(df, aes(x, y, shape = shape)) +
  geom_point_osp(size = 4) +
  scale_shape_osp_identity(guide = "legend")
```
