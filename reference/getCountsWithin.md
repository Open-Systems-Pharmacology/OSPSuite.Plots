# Count Entries Within Specific Limits

This function counts entries within specific limits defined by the
comparison lines and guest limits.

## Usage

``` r
getCountsWithin(
  data,
  mapping,
  comparisonLineVector = getFoldDistanceList(c(1.5, 2)),
  addGuestLimits = FALSE,
  deltaGuest = 1,
  yDisplayAsAbsolute
)
```

## Arguments

- data:

  A `data.frame` containing the data to plot.

- mapping:

  A list of aesthetic mappings to use for the plot.

- comparisonLineVector:

  A vector defining the comparison lines.

- addGuestLimits:

  A boolean that activates the insertion of guest limits.

- deltaGuest:

  Numeric value parameter for the Guest function.

- yDisplayAsAbsolute:

  A boolean that defines the direction of comparison lines.

## Value

A data table summarizing the counts within the specified limits.
