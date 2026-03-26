# Add LLOQ Layer with LLOQ Lines

Add a layer for LLOQ lines to a `ggplot` object.

## Usage

``` r
addLLOQLayer(
  plotObject,
  mappedData,
  layerToCall,
  useLinetypeAsAttribute,
  geomLLOQAttributes
)
```

## Arguments

- plotObject:

  A `ggplot` object on which to add the plot layer.

- mappedData:

  A `MappedData` object with LLOQ data.

- layerToCall:

  A function representing the ggplot2 geom layer.

- useLinetypeAsAttribute:

  A boolean indicating whether to set the line type as an attribute
  (TRUE) or not (FALSE); if TRUE, no legend is created.

- geomLLOQAttributes:

  Additional attributes for the LLOQ layer.

## Value

The updated `ggplot` object.
