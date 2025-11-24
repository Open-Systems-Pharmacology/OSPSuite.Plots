# Create a Forest Plot

This function generates a forest plot with optional faceting and a
corresponding table.

## Usage

``` r
plotForest(
  plotData,
  mapping = aes(y = y, x = x, groupby = dataType),
  xLabel,
  yFacetColumns = NULL,
  xFacetColumn = NULL,
  xscale = c("linear", "log"),
  xscale.args = list(),
  groupAesthetics = c("color", "fill", "shape"),
  tableColumns = c("yValues", "yErrorValues"),
  tableLabels = c("M", "Variance"),
  labelWrapWidth = 10,
  digitsToRound = 2,
  digitsToShow = 2,
  withTable = is.null(xFacetColumn),
  geomPointAttributes = getDefaultGeomAttributes("Point"),
  geomErrorbarAttributes = getDefaultGeomAttributes("Errorbar"),
  facetScales = c("free_y", "free")
)
```

## Arguments

- plotData:

  A data.table containing the data to be plotted. Must include columns
  specified in `yFacetColumns`, `xFacetColumn`, `tableColumns`, and
  others.

- mapping:

  A ggplot2 mapping object, typically created with
  [`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html),
  to specify how variables in the data are mapped to visual properties.

- xLabel:

  A string representing the label for the x-axis.

- yFacetColumns:

  A character vector of column names used for faceting on the y-axis.
  Can be NULL or of length up to 2.

- xFacetColumn:

  A character string specifying the column name for the x-axis facet.
  Must be of length 1 or NULL.

- xscale:

  A character string indicating the scale type for the x-axis. Options
  are "linear" or "log".

- xscale.args:

  A list of additional arguments for customizing the x-axis scale.

- groupAesthetics:

  A character vector specifying aesthetics for grouping (e.g., color,
  fill, shape).

- tableColumns:

  A character vector of column names to be included in the table.

- tableLabels:

  A character vector of labels corresponding to `tableColumns`.

- labelWrapWidth:

  A numeric value specifying the width for label wrapping in facets.

- digitsToRound:

  An integer specifying the number of digits to round in the table.

- digitsToShow:

  An integer specifying the number of digits to display in the table.

- withTable:

  A logical flag indicating whether to include the table in the output.
  Defaults to TRUE if `xFacetColumn` is not NULL.

- geomPointAttributes:

  A list of attributes for the point geometry in the plot.

- geomErrorbarAttributes:

  A list of attributes for the error bar geometry in the plot.

- facetScales:

  A character string indicating the scales used for facets. Options are
  "free_y" or "free".

## Value

A combined plot object containing the forest plot and the table (if
applicable).

## See also

Other plot functions:
[`plotBoxWhisker()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotBoxWhisker.md),
[`plotHistogram()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotHistogram.md),
[`plotPredVsObs()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotPredVsObs.md),
[`plotQQ()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotQQ.md),
[`plotRangeDistribution()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotRangeDistribution.md),
[`plotRatioVsCov()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotRatioVsCov.md),
[`plotResVsCov()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotResVsCov.md),
[`plotTimeProfile()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotTimeProfile.md),
[`plotYVsX()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotYVsX.md)
