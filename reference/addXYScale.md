# Add X and Y Scale

Add X and Y scales to a `ggplot` object.

## Usage

``` r
addXYScale(
  plotObject,
  xScale = NULL,
  xScaleArgs = list(),
  yScale = NULL,
  yScaleArgs = list(),
  secAxis = waiver()
)
```

## Arguments

- plotObject:

  A `ggplot` object on which to add the scale.

- xScale:

  The x-axis scale type. Available is 'linear', 'log', 'discrete'

- xScaleArgs:

  A list of arguments for the x-axis scale.

- yScale:

  The y-axis scale type. Available is 'linear', 'log'

- yScaleArgs:

  A list of arguments for the y-axis scale.

- secAxis:

  Secondary axis arguments for scale_y functions.

## Value

The updated `ggplot` object.
