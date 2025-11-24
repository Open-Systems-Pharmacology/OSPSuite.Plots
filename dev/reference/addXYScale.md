# Add X and Y Scale

Add X and Y scales to a `ggplot` object.

## Usage

``` r
addXYScale(
  plotObject,
  xscale = NULL,
  xscale.args = list(),
  yscale = NULL,
  yscale.args = list(),
  secAxis = waiver()
)
```

## Arguments

- plotObject:

  A `ggplot` object on which to add the scale.

- xscale:

  The x-axis scale type. Available is 'linear', 'log', 'discrete'

- xscale.args:

  A list of arguments for the x-axis scale.

- yscale:

  The y-axis scale type. Available is 'linear', 'log'

- yscale.args:

  A list of arguments for the y-axis scale.

- secAxis:

  Secondary axis arguments for scale_y functions.

## Value

The updated `ggplot` object.
