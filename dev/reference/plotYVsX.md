# Base Plot for Residuals and Predictions vs Covariates

This function creates a base plot for
[`plotResVsCov()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotResVsCov.md),
[`plotRatioVsCov()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotRatioVsCov.md),
and
[`plotPredVsObs()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotPredVsObs.md).

For details and examples, see the vignettes:

- `vignette("Goodness of fit", package = "ospsuite.plots")`

- `vignette("ospsuite.plots", package = "ospsuite.plots")`

## Usage

``` r
plotYVsX(
  data,
  mapping,
  metaData = NULL,
  geomPointAttributes = getDefaultGeomAttributes("Point"),
  geomErrorbarAttributes = getDefaultGeomAttributes("Errorbar"),
  geomGuestLineAttributes = getDefaultGeomAttributes("GuestLine"),
  geomComparisonLineAttributes = getDefaultGeomAttributes("ComparisonLine"),
  geomLLOQAttributes = getDefaultGeomAttributes("LLOQ"),
  groupAesthetics = c("colour", "fill", "shape"),
  comparisonLineVector = NULL,
  addRegression = FALSE,
  addGuestLimits = FALSE,
  deltaGuest = 1,
  labelGuestCriteria = "guest criteria",
  residualScale = NULL,
  asSquarePlot = FALSE,
  xscale = AxisScales$linear,
  xscale.args = list(),
  yscale = AxisScales$log,
  yscale.args = list(),
  observedDataDirection = "y",
  yDisplayAsAbsolute = TRUE
)
```

## Arguments

- data:

  A `data.frame` containing the data to plot.

- mapping:

  A list of aesthetic mappings to use for the plot.

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

- labelGuestCriteria:

  Label used in the legend for guest criteria (default: "guest
  criteria").

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

- yDisplayAsAbsolute:

  A boolean that defines the direction of comparison lines.

## Value

A `ggplot` object representing the plotted data.

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
[`plotTimeProfile()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotTimeProfile.md)
