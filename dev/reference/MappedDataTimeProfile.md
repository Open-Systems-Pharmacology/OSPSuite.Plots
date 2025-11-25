# MappedDataTimeProfile

R6 class for mapping variable to data for time profile visualizations.
This class extends MappedData to provide specialized functionality for
time-series plots, including support for secondary y-axes, dual scaling,
and time-specific axis handling.

## Details

This class is specifically designed for pharmacokinetic time profile
plots where data may need to be displayed on dual y-axes with different
scales (linear/log). It handles complex scenarios like mapping simulated
and observed data with different scaling requirements.

## See also

Other MappedData classes:
[`MappedData`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/MappedData.md),
[`MappedDataBoxplot`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/MappedDataBoxplot.md),
[`MappedDataRangeDistribution`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/MappedDataRangeDistribution.md)

## Super class

[`ospsuite.plots::MappedData`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/MappedData.md)
-\> `MappedDataTimeProfile`

## Public fields

- `y2limits`:

  double vector limits of secondary y axis

## Active bindings

- `requireDualAxis`:

  boolean, If TRUE secondary axis is required

- `listOfGroups`:

  character vector of groupings

- `secAxis`:

  sec_axis() object

- `dataForPlot`:

  scaled data used for plotting adjust limits

## Methods

### Public methods

- [`MappedDataTimeProfile$new()`](#method-MappedDataTimeProfile-new)

- [`MappedDataTimeProfile$scaleDataForSecondaryAxis()`](#method-MappedDataTimeProfile-scaleDataForSecondaryAxis)

- [`MappedDataTimeProfile$clone()`](#method-MappedDataTimeProfile-clone)

Inherited methods

- [`ospsuite.plots::MappedData$addMetaData()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/MappedData.html#method-addMetaData)
- [`ospsuite.plots::MappedData$getAestheticsForGeom()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/MappedData.html#method-getAestheticsForGeom)
- [`ospsuite.plots::MappedData$updateScaleArgumentsForTimeUnit()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/MappedData.html#method-updateScaleArgumentsForTimeUnit)

------------------------------------------------------------------------

### Method `new()`

Create a new `MappedDataTimeProfile` object

#### Usage

    MappedDataTimeProfile$new(
      data,
      mapping,
      groupAesthetics = NULL,
      groupOrder = NULL,
      direction = "y",
      isObserved = TRUE,
      xlimits = NULL,
      ylimits = NULL,
      xScale = AxisScales$linear,
      scaleOfPrimaryAxis = AxisScales$linear,
      scaleOfSecondaryAxis = AxisScales$linear,
      y2limits = NULL
    )

#### Arguments

- `data`:

  data.frame used for mapping

- `mapping`:

  list of aesthetic mappings

- `groupAesthetics`:

  vector of aesthetics, which are used for columns mapped with aesthetic
  `groupby` , use of group aesthetics triggers second axis after
  simulation layers

- `groupOrder`:

  labels and order for group aesthetic

- `direction`:

  direction of plot either "x" or "y"

- `isObserved`:

  A `boolean` if TRUE mappings mdv, lloq are evaluated

- `xlimits`:

  limits for x-axis (may be NULL)

- `ylimits`:

  limits for primary axis (may be NULL)

- `xScale`:

  = scale of x-axis

- `scaleOfPrimaryAxis`:

  scale of direction, either "linear" or "log"

- `scaleOfSecondaryAxis`:

  either 'linear' or 'log'

- `y2limits`:

  limits for secondary axis (may be NULL)

#### Returns

A new `MappedDataTimeProfile` object Scale data for secondary axis and
update secAxis transformation

This method handles the complex logic of scaling data between primary
and secondary axes with different scale types (linear/log combinations).

------------------------------------------------------------------------

### Method `scaleDataForSecondaryAxis()`

#### Usage

    MappedDataTimeProfile$scaleDataForSecondaryAxis(
      ylimits = NULL,
      y2limits = NULL,
      y2ScaleArgs = list()
    )

#### Arguments

- `ylimits`:

  limits for primary axis (may be NULL)

- `y2limits`:

  limits for secondary axis (may be NULL)

- `y2ScaleArgs`:

  arguments for secondary axis

#### Returns

updated MappedDataTimeProfile boolean for secondary axis

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MappedDataTimeProfile$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create time profile mapping with secondary axis
timeData <- MappedDataTimeProfile$new(
  data = myDataFrame,
  mapping = aes(x = time, y = concentration, y2axis = fraction_unbound),
  scaleOfPrimaryAxis = "linear",
  scaleOfSecondaryAxis = "log"
)

# Time profile with grouping aesthetics
timeData <- MappedDataTimeProfile$new(
  data = myDataFrame,
  mapping = aes(x = time, y = concentration, color = compound),
  groupAesthetics = c("color", "linetype")
)
} # }
```
