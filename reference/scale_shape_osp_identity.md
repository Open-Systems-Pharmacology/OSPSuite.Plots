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

## Value

A ggplot2 scale that can be added to a plot.

## See also

Other shapes:
[`Shapes`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/reference/Shapes.md),
[`ospShapeNames`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/reference/ospShapeNames.md),
[`scale_shape_osp()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/reference/scale_shape_osp.md),
[`scale_shape_osp_manual()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/reference/scale_shape_osp_manual.md)

## Examples

``` r
library(ggplot2)
df <- data.frame(x = 1:3, y = 1:3, shape = c("circle", "diamond", "star"))
ggplot(df, aes(x, y, shape = shape)) +
  geom_point_osp(size = 4) +
  scale_shape_osp_identity(guide = "legend")
```
