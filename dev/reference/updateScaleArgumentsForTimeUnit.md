# adjust arguments for scale if dimension of scale is time

adds break function with fixed width for breaks depending on unit:

## Usage

``` r
updateScaleArgumentsForTimeUnit(scaleArgs, dimension, unit)
```

## Arguments

- scaleArgs:

  list of arguments for scale to be updated, passed to
  scale_x_continuous or scale_x_log10

- dimension:

  dimension of axis, if not 'time' list will not be updated

- unit:

  A named list of information about the `data` such as the `dimension`
  and `unit` of its variables.

## Value

update list of arguments for scale

## Details

- s: width = 15,

- min: width = 15,

- h: width = 6,

- day(s): width = 7

- week(s): width = 4

- month(s): width = 6

The function uses the following logic to determine the breaks:

- If the range of time values is relatively small (i.e., less than twice
  the width of the breaks), it will use a default set of extended
  breaks.

- If the range of time values is larger, the function will check if it
  is appropriate to use wider breaks. Specifically, it will continue to
  double the width until it finds a width that is suitable, ensuring
  that 10 times the width is still less than the total range of time
  values. This means that the breaks will be spaced far enough apart to
  be meaningful without overcrowding the axis, providing clarity in the
  visualization.

## Examples

``` r
xScaleArgs <- list(limits = c(0, 24))
xScaleArgs <-
  updateScaleArgumentsForTimeUnit(
    scaleArgs = xScaleArgs,
    dimension = "time",
    unit = "h"
  )
addXScale(plotObject = ggplot(), xScale = "linear", xScaleArgs = xScaleArgs)
```
