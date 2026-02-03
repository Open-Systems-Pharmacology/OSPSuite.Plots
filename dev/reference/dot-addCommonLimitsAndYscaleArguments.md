# set the common axis limits to `MappedData` object and update y2-scale arguments

set the common axis limits to `MappedData` object and update y2-scale
arguments

## Usage

``` r
.addCommonLimitsAndYScaleArguments(
  simMappedData,
  obsMappedData,
  commonLimits,
  requireDualAxis,
  yScaleArgs,
  y2ScaleArgs
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

- yScaleArgs:

  list with yScale arguments

- y2ScaleArgs:

  list with arguments for secondary y-axis

## Value

list with `simMappedData` adjusted object of class
`MappedDataTimeprofile` for simulated data `obsMappedData` adjusted
object of class `MappedDataTimeprofile` for observed data `yScaleArgs`
adjusted `yScaleArgs` with common limits for primary and secondary
y-axis `secAxis` secondary axis object
