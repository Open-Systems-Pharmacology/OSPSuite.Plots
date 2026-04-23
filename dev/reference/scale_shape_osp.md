# OSP Shape Scale

Discrete shape scale that automatically assigns shapes from
`ospShapeNames` in order based on the number of factor levels.
Equivalent to
[`ggplot2::scale_shape()`](https://ggplot2.tidyverse.org/reference/scale_shape.html).

If there are more levels than available shapes, shapes are recycled and
a warning is issued.

## Usage

``` r
scale_shape_osp(...)
```

## Arguments

- ...:

  Passed to
  [`ggplot2::discrete_scale`](https://ggplot2.tidyverse.org/reference/discrete_scale.html).

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
[`scale_shape_osp_identity()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/scale_shape_osp_identity.md),
[`scale_shape_osp_manual()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/scale_shape_osp_manual.md),
[`setDefaultColorMapDistinct()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/setDefaultColorMapDistinct.md),
[`setDefaultShapeDiscrete()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/setDefaultShapeDiscrete.md),
[`setDefaultTheme()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/setDefaultTheme.md),
[`setDefaults()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/setDefaults.md),
[`setOspsuite.plots.option()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/setOspsuite.plots.option.md)

## Examples

``` r
library(ggplot2)
df <- data.frame(x = 1:3, y = 1:3, group = c("A", "B", "C"))
ggplot(df, aes(x, y, shape = group)) +
  geom_point_osp(size = 4) +
  scale_shape_osp()
```
