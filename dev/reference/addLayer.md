# Add Layer

Add a layer to a `ggplot` object.

## Usage

``` r
addLayer(mappedData, geomAttributes, geom, plotObject, layerToCall)
```

## Arguments

- mappedData:

  A `MappedData` object.

- geomAttributes:

  Arguments passed on to the ggplot2 geom layer.

- geom:

  A character string used to select appropriate aesthetics.

- plotObject:

  A `ggplot` object on which to add the plot layer.

- layerToCall:

  A function representing the ggplot2 geom layer.

## Value

The updated `ggplot` object.
