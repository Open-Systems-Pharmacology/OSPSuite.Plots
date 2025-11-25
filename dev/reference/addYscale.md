# add y-scale

add y-scale

## Usage

``` r
addYScale(plotObject, yScale, yScaleArgs = list(), secAxis = waiver())
```

## Arguments

- plotObject:

  A `ggplot` object on which to add the scale.

- yScale:

  The y-axis scale type. Available is 'linear', 'log'

- yScaleArgs:

  A list of arguments for the y-axis scale.

- secAxis:

  Secondary axis arguments for scale_y functions.

## Value

The updated `ggplot` object
