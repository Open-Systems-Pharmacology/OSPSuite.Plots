# Add Comparison Lines to Plot

This function adds horizontal or diagonal comparison lines to the given
ggplot object.

## Usage

``` r
addComparisonLines(
  plotObject,
  comparisonLineVector,
  yDisplayAsAbsolute,
  geomLineAttributes,
  xyscale
)
```

## Arguments

- comparisonLineVector:

  A vector defining the comparison lines.

- yDisplayAsAbsolute:

  A boolean that defines the direction of comparison lines.

- geomLineAttributes:

  Line attributes, e.g., `color`, `linetype`, passed to
  [`ggplot2::geom_hline`](https://ggplot2.tidyverse.org/reference/geom_abline.html)
  or
  [`ggplot2::geom_abline`](https://ggplot2.tidyverse.org/reference/geom_abline.html).

- xyscale:

  Either "linear" or "log" scale for the X and Y axes.

## Value

The updated `ggplot` object with comparison lines added.
