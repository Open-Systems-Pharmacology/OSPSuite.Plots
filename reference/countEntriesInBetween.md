# Count Entries Between Specified Limits

This function counts the number of entries within specified limits for
the given X and Y columns. It calculates the counts based on the
comparison line vector and guest limits, if applicable.

## Usage

``` r
countEntriesInBetween(
  yColumn,
  xColumn,
  comparisonLineVector,
  deltaGuest,
  addGuestLimits,
  yDisplayAsAbsolute
)
```

## Arguments

- yColumn:

  A numeric vector containing the Y values to count.

- xColumn:

  A numeric vector containing the X values to count.

- comparisonLineVector:

  A list of numeric values defining the comparison limits.

- deltaGuest:

  A numeric value parameter for the Guest function.

- addGuestLimits:

  A boolean indicating whether to include guest limits in the counting.

- yDisplayAsAbsolute:

  A boolean indicating whether to consider absolute values for Y.

## Value

A list containing counts of entries that fall within the specified
limits, including guest criteria if applicable.
