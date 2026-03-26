# Create the Plot Object

This function generates the main plot object for the forest plot.

## Usage

``` r
createPlotObject(
  plotData,
  mapping,
  xScale,
  xScaleArgs,
  xLabel,
  groupAesthetics,
  yFacetColumns,
  xFacetColumn,
  labelWrapWidth,
  geomPointAttributes,
  geomErrorbarAttributes,
  facetScales = "free_y"
)
```

## Arguments

- plotData:

  A data.table containing the data to be plotted.

- mapping:

  A ggplot mapping object.

- xScale:

  A character string indicating the scale type for the x-axis.

- xScaleArgs:

  A list of additional arguments for customizing the x-axis scale.

- xLabel:

  A string representing the label for the x-axis.

- groupAesthetics:

  A character vector specifying aesthetics for grouping.

- yFacetColumns:

  A character vector of column names used for faceting on the y-axis.

- xFacetColumn:

  A character string specifying the column name for the x-axis facet.

- labelWrapWidth:

  A numeric value specifying the width for label wrapping in facets.

- geomPointAttributes:

  A list of attributes for the point geometry.

- geomErrorbarAttributes:

  A list of attributes for the error bar geometry.

- facetScales:

  A character string indicating the scales used for facets. Defaults to
  "free_y".

## Value

A ggplot object representing the main plot.
