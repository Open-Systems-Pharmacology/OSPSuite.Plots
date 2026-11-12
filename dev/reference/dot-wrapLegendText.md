# Wrap strings to a maximum number of characters per line

Words are wrapped with
[`stringr::str_wrap()`](https://stringr.tidyverse.org/reference/str_wrap.html)
and lines are joined with `"\n"`.

## Usage

``` r
.wrapLegendText(labels, maxWidth)
```

## Arguments

- labels:

  Character vector of labels to wrap.

- maxWidth:

  Maximum number of characters per line.

## Value

Character vector of wrapped labels, `NA` labels are kept.
