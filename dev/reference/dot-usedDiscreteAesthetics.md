# Determine which discrete color/fill aesthetics a plot uses

Scans the plot-level and per-layer mappings for `colour`/`fill` and
returns those that resolve to discrete (factor/character/logical) data.
Used to decide whether the OSP discrete scales can be safely applied.

## Usage

``` r
.usedDiscreteAesthetics(plot)
```

## Arguments

- plot:

  a `ggplot` object

## Value

character vector, a subset of `c("colour", "fill")`
