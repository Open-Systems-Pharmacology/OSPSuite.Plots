# MappedData

R6 class for mapping variables to `data`

## See also

Other MappedData classes:
[`MappedDataBoxplot`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/MappedDataBoxplot.md),
[`MappedDataRangeDistribution`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/MappedDataRangeDistribution.md),
[`MappedDataTimeProfile`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/MappedDataTimeProfile.md)

## Public fields

- `data`:

  data.frame used for mapping

- `mapping`:

  list of aesthetic mappings

- `dimensions`:

  list with dimensions of mapping

- `units`:

  list with dimensions of mapping

- `columnClasses`:

  list with class of mapped columns

- `xlimits`:

  double vector limits of primary y axis

- `ylimits`:

  double vector limits of primary y axis

- `hasResidualMapping`:

  flag to indicate if residual mapping is used

- `residualLabel`:

  label for residuals

## Active bindings

- `hasLLOQMatch`:

  `boolean` if TRUE data has matched lloq data

- `dataForPlot`:

  returns data used for plotting, may be adjusted in child classes (e.g.
  2 axis in MappedDataTimeProfile) check if aesthetic is available in
  data returns data column for aesthetic adds and update mapping deletes
  data where mdv is 1

  adds new column `isLLOQ.i` and updates boolean `LLOQMatch` adds new
  columns `ymin` and `ymax` if required copy aesthetics `groupby`, but
  only if not explicit set converts Integer columns, which are no
  factors to double adds new column `residuals.i` factorize column for
  group to factor

## Methods

### Public methods

- [`MappedData$new()`](#method-MappedData-new)

- [`MappedData$getAestheticsForGeom()`](#method-MappedData-getAestheticsForGeom)

- [`MappedData$addMetaData()`](#method-MappedData-addMetaData)

- [`MappedData$updateScaleArgumentsForTimeUnit()`](#method-MappedData-updateScaleArgumentsForTimeUnit)

- [`MappedData$clone()`](#method-MappedData-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `MappedData` object

#### Usage

    MappedData$new(
      data,
      mapping,
      xscale,
      yscale,
      groupAesthetics = NULL,
      groupOrder = NULL,
      direction = "y",
      isObserved = TRUE,
      xlimits = NULL,
      ylimits = NULL,
      residualScale = NULL,
      residualAesthetic = "y"
    )

#### Arguments

- `data`:

  data.frame used for mapping

- `mapping`:

  list of aesthetic mappings

- `xscale`:

  scale of x-axis either 'linear' or 'log'

- `yscale`:

  scale of y-axis either 'linear' or 'log'

- `groupAesthetics`:

  vector of aesthetics, which are used for columns mapped with `groupby`

- `groupOrder`:

  labels and order for group aesthetic

- `direction`:

  direction of plot either "x" or "y"

- `isObserved`:

  A `boolean `if TRUE mappings mdv, lloq

- `xlimits`:

  limits for x-axis (may be NULL)

- `ylimits`:

  limits for y-axis (may be NULL)

- `residualScale`:

  scale of x residuals

- `residualAesthetic`:

  aesthetic used for mapping residuals

#### Returns

A new `MappedData` object filter possible aesthetics for a geom, check
if mandatory are available

------------------------------------------------------------------------

### Method `getAestheticsForGeom()`

#### Usage

    MappedData$getAestheticsForGeom(geom, geomAttributes)

#### Arguments

- `geom`:

  type of geometric object

- `geomAttributes`:

  additionally arguments for geom layer, will overwrite aesthetics

#### Returns

list of accepted mappings adds list with dimension, units and column
classes

------------------------------------------------------------------------

### Method `addMetaData()`

#### Usage

    MappedData$addMetaData(metaData)

#### Arguments

- `metaData`:

  A named list of information about `data` such as the `dimension` and
  `unit` of its variables.

#### Returns

updated `MappedData` object check if unit of scale direction i s time
and sets the breaks accordingly

------------------------------------------------------------------------

### Method [`updateScaleArgumentsForTimeUnit()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/updateScaleArgumentsForTimeUnit.md)

#### Usage

    MappedData$updateScaleArgumentsForTimeUnit(scale.args, scaleDirection = "x")

#### Arguments

- `scale.args`:

  additional arguments passed on to scale function

- `scaleDirection`:

  direction of axis either 'x' or 'y'

#### Returns

`scale.args` with adjusted break function

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MappedData$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
