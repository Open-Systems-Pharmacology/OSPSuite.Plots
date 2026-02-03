# generate time profile plots

Produces time profiles for simulated and observed data.

For the simulated data a `geom_line` and a `geom_ribbon` layer are added
For the observed data a `geom_point` and a `geom_errorbar` layer are
added

For more details and examples see the vignettes:

- `vignette("Time Profile Plots", package = "ospsuite.plots")`

- `vignette("ospsuite.plots", package = "ospsuite.plots")`

## Usage

``` r
plotTimeProfile(
  data = NULL,
  mapping = NULL,
  observedData = NULL,
  observedMapping = mapping,
  metaData = NULL,
  mapSimulatedAndObserved = NULL,
  xScale = AxisScales$linear,
  xScaleArgs = list(limits = c(0, NA)),
  yScale = AxisScales$linear,
  yScaleArgs = list(),
  y2Scale = AxisScales$linear,
  y2ScaleArgs = list(),
  plotObject = NULL,
  geomLineAttributes = getDefaultGeomAttributes("Line"),
  geomRibbonAttributes = getDefaultGeomAttributes("Ribbon"),
  geomPointAttributes = getDefaultGeomAttributes("Point"),
  geomErrorbarAttributes = getDefaultGeomAttributes("Errorbar"),
  geomLLOQAttributes = getDefaultGeomAttributes("LLOQ"),
  groupAesthetics = c("colour", "fill", "shape")
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

- mapSimulatedAndObserved:

  table with columns observed and simulated which maps simulated and
  observed data use of `mapSimulatedAndObserved` triggers reset of
  aesthetic scales after simulation layers

- xScale:

  either 'linear' then
  [`ggplot2::scale_x_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
  or 'log' then
  [`ggplot2::scale_x_log10()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
  is used

- xScaleArgs:

  list of arguments passed to
  [`ggplot2::scale_x_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
  or
  [`ggplot2::scale_x_log10()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)

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

- plotObject:

  An optional `ggplot` object on which to add the plot layers

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

## Value

A `ggplot` object

## See also

Other plot functions:
[`plotBoxWhisker()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotBoxWhisker.md),
[`plotForest()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotForest.md),
[`plotHistogram()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotHistogram.md),
[`plotPredVsObs()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotPredVsObs.md),
[`plotQQ()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotQQ.md),
[`plotRangeDistribution()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotRangeDistribution.md),
[`plotRatioVsCov()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotRatioVsCov.md),
[`plotResVsCov()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotResVsCov.md),
[`plotYVsX()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotYVsX.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Set watermark option first (required)
options(ospsuite.plots.watermark_enabled = TRUE)

# Basic time profile plot with simulated data
plotTimeProfile(
  data = simulationData,
  mapping = aes(x = time, y = concentration, color = compound)
)

# Time profile with both simulated and observed data
plotTimeProfile(
  data = simulationData,
  observedData = observedData,
  mapping = aes(x = time, y = concentration, color = treatment),
  observedMapping = aes(x = time, y = concentration, color = treatment)
)

# Time profile with secondary y-axis
plotTimeProfile(
  data = myData,
  mapping = aes(x = time, y = concentration, y2axis = fraction_unbound)
)
} # }
```
