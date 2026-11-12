# Resolve the labels displayed by a scale for given breaks

Handles the possible `labels` specifications of a scale: a function, a
named or unnamed character vector, a waiver or `NULL`.

## Usage

``` r
.resolveScaleLabels(existingLabels, breaks)
```

## Arguments

- existingLabels:

  The `labels` field of a scale (function, character, waiver or `NULL`).

- breaks:

  The breaks of the scale.

## Value

Character vector of displayed labels for `breaks`.
