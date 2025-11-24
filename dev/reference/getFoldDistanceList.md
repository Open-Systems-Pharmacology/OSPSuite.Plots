# creates a list with fold Distances

this list is used as input for `plotRatioVsCov`, `plotPredVsObs`

## Usage

``` r
getFoldDistanceList(folds = c(1.5, 2), includeIdentity = TRUE)
```

## Arguments

- folds:

  of folds e.g. c(1.5,2) must be \>1

- includeIdentity:

  A `boolean`, if TRUE (default) line of identity is added

## Value

named list with fold distances
