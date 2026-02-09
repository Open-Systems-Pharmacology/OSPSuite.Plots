# generates residual quantile quantile plot

For details and examples see the vignettes:

- `vignette("Goodness of fit", package = "ospsuite.plots")`

- `vignette("ospsuite.plots", package = "ospsuite.plots")`

## Usage

``` r
plotQQ(
  data,
  mapping,
  metaData = NULL,
  xScaleArgs = list(),
  residualScale = ResidualScales$log,
  yScaleArgs = list(),
  geomQQAttributes = list(),
  geomQQLineAttributes = geomQQAttributes,
  groupAesthetics = c("colour", "fill", "shape")
)
```

## Arguments

- data:

  ´data.frame\` with data to plot

- mapping:

  a list of aesthetic mappings to use for plot, additional to
  `{ggplot2}` aesthetics, the aesthetics
  `groupby`,`error`,`error_relative`,`lloq`, `mdv`, `y2axis` are
  available, see vignettes for more details and examples

- metaData:

  A named list of information about `data` such as the `dimension` and
  `unit` of its variables.

- xScaleArgs:

  list of arguments passed to
  [`ggplot2::scale_x_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
  or
  [`ggplot2::scale_x_log10()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)

- residualScale:

  Either `"linear"` or `"log"` for scaling residuals. For linear:
  residuals = predicted - observed. For log: residuals =
  log(predicted) - log(observed). The y-axis scale remains linear in
  both cases.

- yScaleArgs:

  list of arguments passed to
  [`ggplot2::scale_y_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
  or
  [`ggplot2::scale_y_log10()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)

- geomQQAttributes:

  A list of arguments passed to
  [`ggplot2::stat_qq()`](https://ggplot2.tidyverse.org/reference/geom_qq.html).

- geomQQLineAttributes:

  A list of arguments passed to
  [`ggplot2::stat_qq_line()`](https://ggplot2.tidyverse.org/reference/geom_qq.html).

- groupAesthetics:

  A character vector of aesthetic names used for grouping data points in
  the Q-Q plot. Common options include `"colour"`, `"fill"`, `"shape"`,
  `"linetype"`, and `"size"`.

## Value

A `ggplot` object

## See also

Other plot functions:
[`plotBoxWhisker()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotBoxWhisker.md),
[`plotForest()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotForest.md),
[`plotHistogram()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotHistogram.md),
[`plotPredVsObs()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotPredVsObs.md),
[`plotRangeDistribution()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotRangeDistribution.md),
[`plotRatioVsCov()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotRatioVsCov.md),
[`plotResVsCov()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotResVsCov.md),
[`plotTimeProfile()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotTimeProfile.md),
[`plotYVsX()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotYVsX.md)
