# Initialize Plot

Initialize a `ggplot` object with a watermark and set its labels by
metaData.

## Usage

``` r
initializePlot(
  mappedData = NULL,
  setMapping = TRUE,
  watermark = NULL,
  watermarkFormat = NULL,
  watermarkEnabled = NULL
)
```

## Arguments

- mappedData:

  A `MappedData` object.

- setMapping:

  A boolean indicating if TRUE (default) mapping is passed to ggplot;
  otherwise, mapping will be used only to create labels.

- watermark:

  Optional watermark text for this plot.

- watermarkFormat:

  Optional list of watermark formatting values.

- watermarkEnabled:

  Optional logical flag controlling watermark visibility for this plot.

## Value

A `ggplot` object.
