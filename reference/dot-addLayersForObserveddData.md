# set line and ribbon layer fro simulated data

set line and ribbon layer fro simulated data

## Usage

``` r
.addLayersForObserveddData(
  plotObject,
  obsMappedData,
  geomErrorbarAttributes,
  geomPointAttributes,
  geomLLOQAttributes,
  useLLOQLinetypeAsAttribute,
  mapSimulatedAndObserved,
  groupAesthetics
)
```

## Arguments

- plotObject:

  An optional `ggplot` object on which to add the plot layers

- obsMappedData:

  object of class `MappedDataTimeprofile` for observed data

- geomErrorbarAttributes:

  A `list` with arguments which are passed on to the call
  [`ggplot2::geom_errorbar`](https://ggplot2.tidyverse.org/reference/geom_linerange.html)

- geomPointAttributes:

  A `list` with arguments which are passed on to the call
  [`ggplot2::geom_point`](https://ggplot2.tidyverse.org/reference/geom_point.html)

- geomLLOQAttributes:

  A `list` with arguments which are passed on to the call
  [`ggplot2::geom_hline`](https://ggplot2.tidyverse.org/reference/geom_abline.html)

- useLLOQLinetypeAsAttribute:

  boolean if TRUE line type for LLOQ is set as attribute

- mapSimulatedAndObserved:

  table with columns observed and simulated which maps simulated and
  observed data use of `mapSimulatedAndObserved` triggers reset of
  aesthetic scales after simulation layers

- groupAesthetics:

  vector of aesthetics, which are used for columns mapped with
  `groupby`,
