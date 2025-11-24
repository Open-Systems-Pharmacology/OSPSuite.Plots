# set the common axis limits to `MappedData` object and update y2-scale arguments

set the common axis limits to `MappedData` object and update y2-scale
arguments

## Usage

``` r
.addCommonLimitsAndYscaleArguments(
  simMappedData,
  obsMappedData,
  commonLimits,
  requireDualAxis,
  yscale.args,
  y2scale.args
)
```

## Arguments

- simMappedData:

  object of class `MappedDataTimeprofile` for simulated data

- obsMappedData:

  object of class `MappedDataTimeprofile` for observed data

- commonLimits:

  common limits for simulated and observed data

- requireDualAxis:

  boolean if TRUE secondary axis is needed

- yscale.args:

  list with y2scale arguments

- y2scale.args:

  list with arguments for secondary y-axis

## Value

list with `simMappedData` adjusted object of class
`MappedDataTimeprofile` for simulated data `obsMappedData` adjusted
object of class `MappedDataTimeprofile` for observed data `yscale.args`
adjusted `yscale.args` with common limits for primary and secondary
y-axis `secAxis` secondary axis object
