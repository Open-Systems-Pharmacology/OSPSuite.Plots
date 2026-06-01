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

## Value

A ggplot2 scale that can be added to a plot.

## See also

Other shapes:
[`Shapes`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/Shapes.md),
[`ospShapeNames`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/ospShapeNames.md),
[`scale_shape_osp_identity()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/scale_shape_osp_identity.md),
[`scale_shape_osp_manual()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/scale_shape_osp_manual.md)

## Examples

``` r
library(ggplot2)
df <- data.frame(x = 1:3, y = 1:3, group = c("A", "B", "C"))
ggplot(df, aes(x, y, shape = group)) +
  geom_point_osp(size = 4) +
  scale_shape_osp()
```
