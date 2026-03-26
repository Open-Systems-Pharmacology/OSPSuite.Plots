# Initialize Plot

Initialize a `ggplot` object with a watermark and set its labels by
metaData.

## Usage

``` r
initializePlot(mappedData = NULL, setMapping = TRUE)
```

## Arguments

- mappedData:

  A `MappedData` object.

- setMapping:

  A boolean indicating if TRUE (default) mapping is passed to ggplot;
  otherwise, mapping will be used only to create labels.

## Value

A `ggplot` object.
