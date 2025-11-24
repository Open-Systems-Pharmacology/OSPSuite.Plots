# MappedDataBoxplot

R6 class for mapping variable to data for boxplot visualizations. This
class extends MappedData to provide specialized mapping functionality
for box-and-whisker plots, including handling of discrete and continuous
x-axis scales and automatic grouping logic.

## See also

Other MappedData classes:
[`MappedData`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/MappedData.md),
[`MappedDataRangeDistribution`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/MappedDataRangeDistribution.md),
[`MappedDataTimeProfile`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/MappedDataTimeProfile.md)

## Super class

[`ospsuite.plots::MappedData`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/MappedData.md)
-\> `MappedDataBoxplot`

## Public fields

- `xscale`:

  scale of x axis

- `xscale.args`:

  arguments for scale of x axis

- `hasXmapping`:

  boolean, if TRUE x is mapped

## Active bindings

- `boxwhiskerMapping`:

  mapping for box whisker plot

## Methods

### Public methods

- [`MappedDataBoxplot$new()`](#method-MappedDataBoxplot-new)

- [`MappedDataBoxplot$doAdjustmentsWithMetaData()`](#method-MappedDataBoxplot-doAdjustmentsWithMetaData)

- [`MappedDataBoxplot$clone()`](#method-MappedDataBoxplot-clone)

Inherited methods

- [`ospsuite.plots::MappedData$addMetaData()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/MappedData.html#method-addMetaData)
- [`ospsuite.plots::MappedData$getAestheticsForGeom()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/MappedData.html#method-getAestheticsForGeom)
- [`ospsuite.plots::MappedData$updateScaleArgumentsForTimeUnit()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/MappedData.html#method-updateScaleArgumentsForTimeUnit)

------------------------------------------------------------------------

### Method `new()`

Create a new `MappedDataBoxplot` object

#### Usage

    MappedDataBoxplot$new(
      data,
      mapping,
      groupAesthetics = NULL,
      direction = "y",
      isObserved = TRUE,
      xlimits = NULL,
      ylimits = NULL,
      xscale = AxisScales$linear,
      yscale = AxisScales$linear,
      residualScale = NULL,
      residualAesthetic = "y"
    )

#### Arguments

- `data`:

  data.frame used for mapping

- `mapping`:

  list of aesthetic mappings

- `groupAesthetics`:

  vector of aesthetics, which are used for columns mapped with aesthetic
  `groupby`

- `direction`:

  direction of plot either "x" or "y"

- `isObserved`:

  A `boolean` if TRUE mappings mdv, lloq, error and error_relative are
  evaluated

- `xlimits`:

  limits for x-axis (may be NULL)

- `ylimits`:

  limits for y-axis (may be NULL)

- `xscale`:

  scale of x-axis either 'linear' or 'log'

- `yscale`:

  scale of y-axis either 'linear' or 'log'

- `residualScale`:

  scale of x residuals

- `residualAesthetic`:

  aesthetic used for mapping residuals

#### Returns

`MappedDataBoxplot` class object use Metadata to adjust binning of
x-axis, and group aesthetic

------------------------------------------------------------------------

### Method `doAdjustmentsWithMetaData()`

#### Usage

    MappedDataBoxplot$doAdjustmentsWithMetaData(
      originalmapping,
      xscale,
      xscale.args
    )

#### Arguments

- `originalmapping`:

  mapping provided by user

- `xscale`:

  either 'linear','log', 'discrete' or 'auto' (default) auto select
  linear for continuous data and discrete for categorical data

- `xscale.args`:

  list of arguments passed to
  [`ggplot2::scale_x_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html),
  [`ggplot2::scale_x_log10()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
  or
  [`ggplot2::scale_x_discrete()`](https://ggplot2.tidyverse.org/reference/scale_discrete.html)

#### Returns

adjusted `MappedDataBoxplot` class object

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MappedDataBoxplot$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create boxplot mapping with continuous x variable
boxplotData <- MappedDataBoxplot$new(
  data = myDataFrame,
  mapping = aes(x = dose, y = concentration),
  xscale = "linear"
)

# Create boxplot mapping with categorical x variable
boxplotData <- MappedDataBoxplot$new(
  data = myDataFrame,
  mapping = aes(x = treatment_group, y = response),
  xscale = "discrete"
)
} # }
```
