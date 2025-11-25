# object to map data for rangeplots

R6 class for mapping variable to `data`

## See also

Other MappedData classes:
[`MappedData`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/MappedData.md),
[`MappedDataBoxplot`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/MappedDataBoxplot.md),
[`MappedDataTimeProfile`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/MappedDataTimeProfile.md)

## Super class

[`ospsuite.plots::MappedData`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/MappedData.md)
-\> `MappedDataRangeDistribution`

## Public fields

- `xScale`:

  scale of x axis

## Active bindings

- `border`:

  borders of the binning.

## Methods

### Public methods

- [`MappedDataRangeDistribution$new()`](#method-MappedDataRangeDistribution-new)

- [`MappedDataRangeDistribution$setBins()`](#method-MappedDataRangeDistribution-setBins)

- [`MappedDataRangeDistribution$setBorderDataTable()`](#method-MappedDataRangeDistribution-setBorderDataTable)

- [`MappedDataRangeDistribution$setXMapping()`](#method-MappedDataRangeDistribution-setXMapping)

- [`MappedDataRangeDistribution$clone()`](#method-MappedDataRangeDistribution-clone)

Inherited methods

- [`ospsuite.plots::MappedData$addMetaData()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/MappedData.html#method-addMetaData)
- [`ospsuite.plots::MappedData$getAestheticsForGeom()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/MappedData.html#method-getAestheticsForGeom)
- [`ospsuite.plots::MappedData$updateScaleArgumentsForTimeUnit()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/MappedData.html#method-updateScaleArgumentsForTimeUnit)

------------------------------------------------------------------------

### Method `new()`

Create a new `MappedDataRangeDistribution` object

#### Usage

    MappedDataRangeDistribution$new(
      data,
      mapping,
      groupAesthetics = NULL,
      direction = "y",
      isObserved = TRUE,
      xlimits = NULL,
      ylimits = NULL,
      xScale = "linear",
      yScale = "linear",
      residualScale = NULL,
      residualAesthetic = "y",
      modeOfBinning = NA,
      numberOfBins = NA,
      breaks = NA
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

- `xScale`:

  scale of x-axis either 'linear' or 'log'

- `yScale`:

  scale of y-axis either 'linear' or 'log'

- `residualScale`:

  scale of x residuals

- `residualAesthetic`:

  aesthetic used for mapping residuals

- `modeOfBinning`:

  method of binning (e.g., 'breaks', 'number', 'interval')

- `numberOfBins`:

  number of bins to use for binning

- `breaks`:

  breaks for binning if `modeOfBinning` is 'breaks'

#### Returns

`MappedDataRangeDistribution` class object Set binning columns

------------------------------------------------------------------------

### Method `setBins()`

This method sets the bins for the data based on the specified mode of
binning.

#### Usage

    MappedDataRangeDistribution$setBins()

#### Returns

The object itself (invisible) Create a data table with bin border
information

------------------------------------------------------------------------

### Method `setBorderDataTable()`

This method sets up a data table containing border information for the
bins. Set x mapping for the plot

#### Usage

    MappedDataRangeDistribution$setBorderDataTable(identifier = "IndividualId")

#### Arguments

- `identifier`:

  Identifier for the data table (default is 'IndividualId')

------------------------------------------------------------------------

### Method `setXMapping()`

This method sets the x mapping for the plot based on the specified
parameters.

#### Usage

    MappedDataRangeDistribution$setXMapping(asStepPlot)

#### Arguments

- `asStepPlot`:

  Logical indicating if the plot should be a step plot.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MappedDataRangeDistribution$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
