# Validates input for `plotTimeProfile` function

Validates input for `plotTimeProfile` function

## Usage

``` r
.validatePlotTimeProfileInputs(
  data,
  observedData,
  plotObject,
  metaData,
  xscale,
  xscale.args,
  yscale,
  yscale.args,
  y2scale,
  y2scale.args,
  geomLineAttributes,
  geomRibbonAttributes,
  geomPointAttributes,
  geomErrorbarAttributes,
  geomLLOQAttributes,
  groupAesthetics,
  mapSimulatedAndObserved
)
```

## Arguments

- data:

  data.frame with simulated data will be displayed as lines with ribbons

- observedData:

  data.frame with observed data will be displayed as points with
  error-bars

- plotObject:

  An optional `ggplot` object on which to add the plot layers

- metaData:

  A named list of information about `data` such as the `dimension` and
  `unit` of its variables.

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

- y2scale:

  either 'linear' the secondary axis is displayed linear, or 'log'
  secondary axis is displayed with log scale

- y2scale.args:

  list of arguments passed to
  [`ggplot2::sec_axis()`](https://ggplot2.tidyverse.org/reference/sec_axis.html),
  trans, break are set by code

- geomLineAttributes:

  A `list` with arguments which are passed on to the call
  [`ggplot2::geom_line`](https://ggplot2.tidyverse.org/reference/geom_path.html)

- geomRibbonAttributes:

  A `list` with arguments which are passed on to the call
  [`ggplot2::geom_ribbon`](https://ggplot2.tidyverse.org/reference/geom_ribbon.html)

- geomPointAttributes:

  A `list` with arguments which are passed on to the call
  [`ggplot2::geom_point`](https://ggplot2.tidyverse.org/reference/geom_point.html)

- geomErrorbarAttributes:

  A `list` with arguments which are passed on to the call
  [`ggplot2::geom_errorbar`](https://ggplot2.tidyverse.org/reference/geom_linerange.html)

- geomLLOQAttributes:

  A `list` with arguments which are passed on to the call
  [`ggplot2::geom_hline`](https://ggplot2.tidyverse.org/reference/geom_abline.html)

- groupAesthetics:

  vector of aesthetics, which are used for columns mapped with
  `groupby`,

- mapSimulatedAndObserved:

  table with columns observed and simulated which maps simulated and
  observed data use of `mapSimulatedAndObserved` triggers reset of
  aesthetic scales after simulation layers
