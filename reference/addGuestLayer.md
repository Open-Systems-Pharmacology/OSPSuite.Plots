# adds limits calculates by Guest function to ggplot object

adds limits calculates by Guest function to ggplot object

## Usage

``` r
addGuestLayer(
  plotObject,
  deltaGuest,
  labelGuestCriteria,
  yDisplayAsAbsolute,
  geomGuestLineAttributes
)
```

## Arguments

- plotObject:

  object to be updated

- deltaGuest:

  Numeric value parameter for the Guest function.

- yDisplayAsAbsolute:

  A logical value if FALSE the limits are calculated for the ratio
  predicted/observed if TRUE limits are calculated for observed

- geomGuestLineAttributes:

  list of arguments passed to geom_fun

## Value

The updated `ggplot` object
