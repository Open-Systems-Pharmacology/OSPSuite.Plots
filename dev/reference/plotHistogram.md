# Generates Histograms

Produces histograms with optional distribution fit.

For more details and examples see the vignettes:

- `vignette("Histogram Plots", package = "ospsuite.plots")`

- `vignette("ospsuite.plots", package = "ospsuite.plots")`

- `vignette("Goodness of fit", package = "ospsuite.plots")`

## Usage

``` r
plotHistogram(
  data,
  mapping,
  metaData = NULL,
  asBarPlot = NULL,
  geomHistAttributes = getDefaultGeomAttributes("Hist"),
  plotAsFrequency = FALSE,
  xscale = AxisScales$linear,
  xscale.args = list(),
  yscale = AxisScales$linear,
  yscale.args = list(),
  distribution = "none",
  meanFunction = "auto",
  residualScale = ResidualScales$log
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

- metaData:

  A named list of information about `data` such as the `dimension` and
  `unit` of its variables.

- asBarPlot:

  A `logical` indicating if `geom_histogram` should be used (for
  continuous data) or `geom_bar` (for categorical data). If TRUE, the
  variables `distribution`, `meanFunction`, `xscale`, and `xscale.args`
  are ignored.

- geomHistAttributes:

  A `list` of arguments passed to
  [`ggplot2::geom_histogram`](https://ggplot2.tidyverse.org/reference/geom_histogram.html)
  (or `geom_bar` if `asBarPlot` = TRUE).

- plotAsFrequency:

  A `logical` indicating if the histogram displays frequency on the
  y-axis.

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

- distribution:

  Name of the distribution to fit. Available distributions are those in
  the `stats` package (see ?stats::distributions): `norm`, `lnorm`,
  `weibull`, `gamma`, etc. Use `"none"` for no fit (default). Shortcuts:
  `"normal"` (same as `"norm"`), `"lognormal"` (same as `"lnorm"`).

- meanFunction:

  Function selection for the display of a vertical line. Options:
  `'none'`, `'mean'`, `'geomean'`, `'median'`, `'auto'` (default).
  `'auto'` selects `'mean'` for normal distribution, `'geomean'` for
  lognormal, `'median'` for other distributions, and `'none'` when no
  distribution fit.

- residualScale:

  Either "linear", "log", or "ratio" scale for residuals.

## Value

A `ggplot` object.

## See also

Other plot functions:
[`plotBoxWhisker()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotBoxWhisker.md),
[`plotForest()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotForest.md),
[`plotPredVsObs()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotPredVsObs.md),
[`plotQQ()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotQQ.md),
[`plotRangeDistribution()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotRangeDistribution.md),
[`plotRatioVsCov()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotRatioVsCov.md),
[`plotResVsCov()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotResVsCov.md),
[`plotTimeProfile()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotTimeProfile.md),
[`plotYVsX()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotYVsX.md)
