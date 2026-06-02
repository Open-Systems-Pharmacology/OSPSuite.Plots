# class to support `plotHistogram`

R6 class container for functions and properties used in `plotHistogram`

## Public fields

- `plotAsFrequency`:

  `boolean` to decide if data should be displayed as frequency

- `distribution`:

  `character` distribution to fit

- `scaledMeanFun`:

  `function` to display mean

- `isStacked`:

  `boolean` if True, frequency is displayed a per total, otherwise per
  group

- `asBarPlot`:

  `boolean` indicates plot switches to bar plot

## Active bindings

- `getHistMapping`:

  generates mapping for histogram

## Methods

### Public methods

- [`plotHelperHistogram$new()`](#method-plotHelperHistogram-initialize)

- [`plotHelperHistogram$getDistrMapping()`](#method-plotHelperHistogram-getDistrMapping)

- [`plotHelperHistogram$getMeanMapping()`](#method-plotHelperHistogram-getMeanMapping)

- [`plotHelperHistogram$clone()`](#method-plotHelperHistogram-clone)

------------------------------------------------------------------------

### `plotHelperHistogram$new()`

Create a new `plotHelperHistogram` object.

#### Usage

    plotHelperHistogram$new(
      xScale,
      plotAsFrequency,
      asBarPlot,
      geomHistAttributes,
      distribution,
      meanFunction
    )

#### Arguments

- `xScale`:

  scale of x -axis

- `plotAsFrequency`:

  A `boolean` to switch display of y to frequency

- `asBarPlot`:

  A `boolean` to switch from geom_histogram to geom_bar

- `geomHistAttributes`:

  attribute for plotting the histogram

- `distribution`:

  distribution to fit the data

- `meanFunction`:

  function to display mean

#### Returns

A new `plotHelperHistogram` object

------------------------------------------------------------------------

### `plotHelperHistogram$getDistrMapping()`

Generates mapping for distribution.

#### Usage

    plotHelperHistogram$getDistrMapping(mappedData, plotObject)

#### Arguments

- `mappedData`:

  mapped data of Histogram

- `plotObject`:

  ggplot object

#### Returns

mapping

------------------------------------------------------------------------

### `plotHelperHistogram$getMeanMapping()`

Generates mapping for display of mean.

#### Usage

    plotHelperHistogram$getMeanMapping(mappedData)

#### Arguments

- `mappedData`:

  mapped data of Histogram

#### Returns

mapping

------------------------------------------------------------------------

### `plotHelperHistogram$clone()`

The objects of this class are cloneable with this method.

#### Usage

    plotHelperHistogram$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
