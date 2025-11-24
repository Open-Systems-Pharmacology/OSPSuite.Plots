# Validate Plot Inputs

This internal function validates the inputs for plotting functions to
ensure proper data format and parameter values.

## Usage

``` r
.validatePlotYXsXInputs(
  data,
  metaData,
  geomPointAttributes,
  geomErrorbarAttributes,
  geomGuestLineAttributes,
  geomComparisonLineAttributes,
  geomLLOQAttributes,
  groupAesthetics,
  comparisonLineVector,
  addRegression,
  addGuestLimits,
  deltaGuest,
  residualScale,
  asSquarePlot,
  xscale,
  xscale.args,
  yscale,
  yscale.args,
  observedDataDirection
)
```

## Arguments

- data:

  A `data.frame` containing the data to plot.

- metaData:

  A named list of information about `data` such as the `dimension` and
  `unit` of its variables.

- geomPointAttributes:

  A `list` with arguments which are passed on to the call
  [`ggplot2::geom_point`](https://ggplot2.tidyverse.org/reference/geom_point.html)

- geomErrorbarAttributes:

  A `list` with arguments which are passed on to the call
  [`ggplot2::geom_errorbar`](https://ggplot2.tidyverse.org/reference/geom_linerange.html)

- geomGuestLineAttributes:

  A `list` of arguments passed to
  [`ggplot2::geom_function`](https://ggplot2.tidyverse.org/reference/geom_function.html)
  to display guest criteria.

- geomComparisonLineAttributes:

  A `list` of arguments passed to `ggplot2::hline` or `ggplot2::abline`
  to display comparison lines.

- geomLLOQAttributes:

  A `list` with arguments which are passed on to the call
  [`ggplot2::geom_hline`](https://ggplot2.tidyverse.org/reference/geom_abline.html)

- groupAesthetics:

  A character vector of aesthetic names used for grouping data points
  when calculating comparison statistics. Data will be grouped by
  combinations of these aesthetics before computing counts and
  proportions within comparison lines. Common grouping aesthetics
  include `"colour"`, `"fill"`, `"shape"`.

- comparisonLineVector:

  A vector defining the comparison lines.

- addRegression:

  A boolean that activates the insertion of a regression line.

- addGuestLimits:

  A boolean that activates the insertion of guest limits.

- deltaGuest:

  Numeric value parameter for the Guest function.

- residualScale:

  Either "linear", "log", or "ratio" scale for residuals.

- asSquarePlot:

  A boolean; if true, the plot is returned as a square plot with aspect
  ratio = 1 and fixed ratios.

- xscale:

  either 'linear' then
  [`ggplot2::scale_x_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
  or 'log' then
  [`ggplot2::scale_x_log10()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
  is used

- xscale.args:

  list of arguments passed to
  [`ggplot2::scale_x_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
  or
  [`ggplot2::scale_x_log10()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)

- yscale:

  either 'linear' then
  [`ggplot2::scale_y_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
  or 'log' then
  [`ggplot2::scale_y_log10()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
  is used

- yscale.args:

  list of arguments passed to
  [`ggplot2::scale_y_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
  or
  [`ggplot2::scale_y_log10()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)

- observedDataDirection:

  Either 'x' or 'y', defining the direction of observed data.

## Value

Invisible NULL if validation is successful; otherwise, an error is
raised.
