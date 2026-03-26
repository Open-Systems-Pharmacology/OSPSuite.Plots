# Create the Table Object

This function generates the table plot object.

## Usage

``` r
createTableObject(
  tableData,
  mapping,
  digitsToRound,
  digitsToShow,
  yFacetColumns
)
```

## Arguments

- tableData:

  A data.table containing the data to be plotted in the table.

- mapping:

  A ggplot mapping object.

- digitsToRound:

  An integer specifying the number of digits to round in the table.

- digitsToShow:

  An integer specifying the number of digits to display in the table.

- yFacetColumns:

  A character vector of column names used for faceting on the y-axis.

## Value

A ggplot object representing the table.
