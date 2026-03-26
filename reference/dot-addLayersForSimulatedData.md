# set line and ribbon layer fro simulated data

set line and ribbon layer fro simulated data

## Usage

``` r
.addLayersForSimulatedData(
  plotObject,
  simMappedData,
  geomRibbonAttributes,
  geomLineAttributes,
  mapSimulatedAndObserved,
  groupAesthetics
)
```

## Arguments

- plotObject:

  An optional `ggplot` object on which to add the plot layers

- simMappedData:

  object of class `MappedDataTimeprofile` for simulated data

- geomRibbonAttributes:

  A `list` with arguments which are passed on to the call
  [`ggplot2::geom_ribbon`](https://ggplot2.tidyverse.org/reference/geom_ribbon.html)

- geomLineAttributes:

  A `list` with arguments which are passed on to the call
  [`ggplot2::geom_line`](https://ggplot2.tidyverse.org/reference/geom_path.html)

- mapSimulatedAndObserved:

  table with columns observed and simulated which maps simulated and
  observed data use of `mapSimulatedAndObserved` triggers reset of
  aesthetic scales after simulation layers

- groupAesthetics:

  vector of aesthetics, which are used for columns mapped with
  `groupby`,

## Value

plot object wit newly added layers
