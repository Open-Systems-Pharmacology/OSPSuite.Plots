# prepares mapped Data object for plotting

prepares mapped Data object for plotting

## Usage

``` r
.getMappedDataForTimeProfiles(
  data,
  mapping,
  observedData,
  observedMapping,
  metaData,
  xScale,
  yScale,
  yScaleArgs,
  y2Scale,
  y2ScaleArgs,
  groupAesthetics,
  mapSimulatedAndObserved
)
```

## Arguments

- data:

  data.frame with simulated data will be displayed as lines with ribbons

- mapping:

  a list of aesthetic mappings to use for plot, additional to
  `{ggplot2}` aesthetics, the aesthetics
  `groupby`,`error`,`error_relative`,`lloq`, `mdv`, `y2axis` are
  available, see vignettes for more details and examples

- observedData:

  data.frame with observed data will be displayed as points with
  error-bars

- observedMapping:

  a list of aesthetic mappings to use for observed data, per default is
  is set to mapping. So if both data sets have the same mapping, use
  only `mapping`, if a different mapping is necessary use `mapping` and
  `observedMapping`

- metaData:

  A named list of information about `data` such as the `dimension` and
  `unit` of its variables.

- xScale:

  either 'linear' then
  [`ggplot2::scale_x_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
  or 'log' then
  [`ggplot2::scale_x_log10()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
  is used

- yScale:

  either 'linear' then
  [`ggplot2::scale_y_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
  or 'log' then
  [`ggplot2::scale_y_log10()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
  is used

- yScaleArgs:

  list of arguments passed to
  [`ggplot2::scale_y_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
  or
  [`ggplot2::scale_y_log10()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)

- y2Scale:

  either 'linear' the secondary axis is displayed linear, or 'log'
  secondary axis is displayed with log scale

- y2ScaleArgs:

  list of arguments passed to
  [`ggplot2::sec_axis()`](https://ggplot2.tidyverse.org/reference/sec_axis.html),
  trans, break are set by code

- groupAesthetics:

  vector of aesthetics, which are used for columns mapped with
  `groupby`,

- mapSimulatedAndObserved:

  table with columns observed and simulated which maps simulated and
  observed data use of `mapSimulatedAndObserved` triggers reset of
  aesthetic scales after simulation layers

## Value

list with entries `simMappedData` and `obsMappedData`
