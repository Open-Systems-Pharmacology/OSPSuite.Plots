# Generate Box-Whisker Plots

Produces box-and-whisker plots for visualizing the distribution of data.
For more details and examples, see the vignettes:

- `vignette("Box-Whisker Plots", package = "ospsuite.plots")`

- `vignette("ospsuite.plots", package = "ospsuite.plots")`

## Usage

``` r
plotBoxWhisker(
  data,
  mapping,
  metaData = NULL,
  plotObject = NULL,
  percentiles = getOspsuite.plots.option(optionKey = OptionKeys$Percentiles),
  yScale = AxisScales$linear,
  yScaleArgs = list(),
  xScale = "auto",
  xScaleArgs = list(),
  statFun = NULL,
  outliers = FALSE,
  statFunOutlier = NULL,
  geomBoxplotAttributes = getDefaultGeomAttributes("Boxplot"),
  geomPointAttributes = getDefaultGeomAttributes("Boxplot"),
  residualScale = ResidualScales$log
)
```

## Arguments

- data:

  A `data.frame` containing the data to aggregate.

- mapping:

  A list of aesthetic mappings to use for the plot.

- metaData:

  A named list of information about `data` such as the `dimension` and
  `unit` of its variables.

- plotObject:

  An optional `ggplot` object on which to add the plot layers

- percentiles:

  A numeric vector with percentiles used for the box whiskers and boxes,
  e.g., c(0.05, 0.25, 0.5, 0.75, 0.95). Default defined by
  `ospsuite.plots` option.

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

- xScale:

  Either 'linear', 'log', 'discrete', or 'auto' (default). Auto selects
  linear for continuous data and discrete for categorical data.

- xScaleArgs:

  A list of arguments passed to
  [`ggplot2::scale_x_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html),
  [`ggplot2::scale_x_log10()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html),
  or
  [`ggplot2::scale_x_discrete()`](https://ggplot2.tidyverse.org/reference/scale_discrete.html).

- statFun:

  (default NULL) A function to calculate whiskers and box ranges, which
  overwrites the `percentiles` variable if provided.

- outliers:

  Logical indicating whether outliers should be included in the boxplot.
  Outliers are flagged when outside the range from the "25th"
  percentile - 1.5 x IQR to the "75th" percentile + 1.5 x IQR, as
  suggested by McGill et al.

- statFunOutlier:

  (default NULL) A function to calculate outliers, which overwrites the
  default calculation if provided.

- geomBoxplotAttributes:

  A `list` of arguments passed to the `geom_boxplot` call.

- geomPointAttributes:

  A `list` of arguments passed to the
  [`ggplot2::geom_point`](https://ggplot2.tidyverse.org/reference/geom_point.html)
  call.

- residualScale:

  Either "linear", "log", or "ratio" scale for residuals.

## Value

A `ggplot` object representing the box-whisker plot.

## References

McGill, R., Tukey, J. W., & Larsen, W. A. (1978). Variations of box
plots. The American Statistician, 32(1), 12-16.

## See also

Other plot functions:
[`plotForest()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotForest.md),
[`plotHistogram()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotHistogram.md),
[`plotPredVsObs()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotPredVsObs.md),
[`plotQQ()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotQQ.md),
[`plotRangeDistribution()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotRangeDistribution.md),
[`plotRatioVsCov()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotRatioVsCov.md),
[`plotResVsCov()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotResVsCov.md),
[`plotTimeProfile()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotTimeProfile.md),
[`plotYVsX()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotYVsX.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Set watermark option first (required)
options(ospsuite.plots.watermark_enabled = TRUE)

# Basic box-whisker plot
plotBoxWhisker(
  data = myData,
  mapping = aes(x = group, y = value)
)

# Box-whisker plot with custom percentiles
plotBoxWhisker(
  data = myData,
  mapping = aes(x = treatment, y = response),
  percentiles = c(0.1, 0.25, 0.5, 0.75, 0.9)
)

# Box-whisker plot with custom stat function
customStatFun <- function(x) {
  return(quantile(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm = TRUE))
}
plotBoxWhisker(
  data = myData,
  mapping = aes(x = dose_group, y = concentration),
  statFun = customStatFun,
  outliers = TRUE
)
} # }
```
