# Plot Range Plot

Creates a range plot using specified data and mapping, allowing for
different binning strategies and scaling options. This function provides
a flexible way to visualize data distributions over specified ranges
with optional statistical summaries.

## Usage

``` r
plotRangeDistribution(
  data,
  mapping,
  metaData = NULL,
  modeOfBinning = BINNINGMODE$number,
  numberOfBins = 20,
  breaks = NA,
  asStepPlot = FALSE,
  statFun = NULL,
  percentiles = getOspsuite.plots.option(optionKey = OptionKeys$Percentiles)[c(1, 3, 5)],
  yScale = "linear",
  yScaleArgs = list(),
  xScale = "linear",
  xScaleArgs = list(),
  geomRibbonAttributes = getDefaultGeomAttributes("Ribbon"),
  geomLineAttributes = getDefaultGeomAttributes("Line"),
  identifier = "IndividualId"
)
```

## Arguments

- data:

  A data frame containing the data to be plotted. This data should
  include the variables specified in the `mapping` argument.

- mapping:

  A mapping object (created using
  [`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html))
  that defines how variables in `data` are mapped to aesthetics such as
  x and y axes, color, fill, etc.

- metaData:

  Optional metadata to be added to the plot. This can include additional
  information relevant to the data being plotted.

- modeOfBinning:

  A character string specifying the mode of binning. It determines how
  the data will be divided into bins. Options include:

  - `Equal Frequency Binning`

  - \`Equal Width Binning

  - `Custom Binning` Default is `BINNINGMODE$number`.

- numberOfBins:

  An integer specifying the number of bins to use for equal frequency or
  width binning. Default is 20.

- breaks:

  Optional numeric vector specifying custom breaks for binning when
  `modeOfBinning` is set to `Custom Binning`. This allows for precise
  control over how data is grouped.

- asStepPlot:

  A logical indicating whether to create a step plot. If TRUE, the plot
  will display steps between the data points rather than continuous
  lines. Default is FALSE.

- statFun:

  An optional function for statistical summary, which takes a vector of
  y-values and returns a summary (e.g., quantiles). If NULL, defaults to
  calculating quantiles based on the specified `percentiles`.

- percentiles:

  A numeric vector of percentiles to be used in the statistical summary,
  which defines the range of values to be displayed on the plot. Default
  is the 5th, 50th, and 95th percentiles.

- yScale:

  A character string specifying the y-axis scale. Options are "linear"
  or "log". This determines how the y values are displayed on the plot.
  Default is "linear".

- yScaleArgs:

  A list of additional arguments for the y-axis scale, which can be used
  to customize the appearance and behavior of the y-axis.

- xScale:

  A character string specifying the x-axis scale. Options are "linear"
  or "log". This determines how the x values are displayed on the plot.
  Default is "linear".

- xScaleArgs:

  A list of additional arguments for the x-axis scale, which can be used
  to customize the appearance and behavior of the x-axis.

- geomRibbonAttributes:

  A list of attributes for the ribbon geometry in the plot, allowing
  customization of the visual appearance, such as colors and
  transparency.

- geomLineAttributes:

  A list of attributes for the line geometry in the plot, allowing
  customization of line characteristics such as color, size, and type.

- identifier:

  columnName of individual identifiers, default "IndividualId"

## Value

A ggplot object representing the range plot. The returned object can be
further customized or rendered using
[`print()`](https://rdrr.io/r/base/print.html) or similar functions.

## See also

Other plot functions:
[`plotBoxWhisker()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotBoxWhisker.md),
[`plotForest()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotForest.md),
[`plotHistogram()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotHistogram.md),
[`plotPredVsObs()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotPredVsObs.md),
[`plotQQ()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotQQ.md),
[`plotRatioVsCov()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotRatioVsCov.md),
[`plotResVsCov()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotResVsCov.md),
[`plotTimeProfile()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotTimeProfile.md),
[`plotYVsX()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotYVsX.md)
