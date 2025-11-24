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

  generates mapping for histogram check if variable is a valid
  distribution extract bin width and bin border of ggplot object check
  if input is valid and returns function for vertical line

## Methods

### Public methods

- [`plotHelperHistogram$new()`](#method-plotHelperHistogram-new)

- [`plotHelperHistogram$getDistrMapping()`](#method-plotHelperHistogram-getDistrMapping)

- [`plotHelperHistogram$getMeanMapping()`](#method-plotHelperHistogram-getMeanMapping)

- [`plotHelperHistogram$clone()`](#method-plotHelperHistogram-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `MappedData` object

#### Usage

    plotHelperHistogram$new(
      xscale,
      plotAsFrequency,
      asBarPlot,
      geomHistAttributes,
      distribution,
      meanFunction
    )

#### Arguments

- `xscale`:

  scale of x -axis

- `plotAsFrequency`:

  A `boolean` to switch display of y to frequency

- `asBarPlot`:

  A `boolean` to switch from geom_histogram to geom_bar

- `geomHistAttributes`:

  attribute for plotting the histogram\`

- `distribution`:

  distribution to fit the data

- `meanFunction`:

  function to display mean

#### Returns

A new `plotHelperHistogram` object generates mapping for distribution

------------------------------------------------------------------------

### Method `getDistrMapping()`

#### Usage

    plotHelperHistogram$getDistrMapping(mappedData, plotObject)

#### Arguments

- `mappedData`:

  mapped data of Histogram

- `plotObject`:

  ggplot object

#### Returns

mapping generates mapping for display of mean

------------------------------------------------------------------------

### Method `getMeanMapping()`

#### Usage

    plotHelperHistogram$getMeanMapping(mappedData)

#### Arguments

- `mappedData`:

  mapped data of Histogram

#### Returns

mapping

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    plotHelperHistogram$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
