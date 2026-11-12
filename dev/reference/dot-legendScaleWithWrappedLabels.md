# Create a scale with wrapped labels for a legend aesthetic

The scale type matches the one the plot would get at build time, so
adding it does not change the appearance of the plot.

## Usage

``` r
.legendScaleWithWrappedLabels(aesthetic, labels, ospStyle)
```

## Arguments

- aesthetic:

  One of `"colour"`, `"fill"`, `"shape"` or `"linetype"`.

- labels:

  A `labels` function for the scale.

- ospStyle:

  If `TRUE`, the OSPSuite scales are used.

## Value

A `ggplot2` scale.
