# OSP color palette function

Reproduces the stacked-palette selection of the previous global
`ggplot2.discrete.*` options: `colorMaps$default` for up to 6 groups,
otherwise `colorMaps$ospDefault`.

## Usage

``` r
.ospColorPalette(n)
```

## Arguments

- n:

  number of colors needed

## Value

character vector of colors of length `n`
