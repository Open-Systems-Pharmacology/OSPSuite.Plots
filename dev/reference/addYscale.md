# add y-scale

add y-scale

## Usage

``` r
addYscale(plotObject, yscale, yscale.args = list(), secAxis = waiver())
```

## Arguments

- plotObject:

  A `ggplot` object on which to add the scale.

- yscale:

  The y-axis scale type. Available is 'linear', 'log'

- yscale.args:

  A list of arguments for the y-axis scale.

- secAxis:

  Secondary axis arguments for scale_y functions.

## Value

The updated `ggplot` object
