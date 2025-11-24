# set the default shapes

The scales are set to the option `ospsuite.plots.shapeValues`, which is
the used to set the discrete scale of shapes for all `ospsuite.plots`
function for customized functions add
scale_shape_manual(`values = getOspsuite.plots.option(optionKey = OptionKeys$shapeValues)`)

## Usage

``` r
setDefaultShapeDiscrete(shapeValues = NULL)
```

## Arguments

- shapeValues:

  vector of shape values

## Value

vector with `shapeValues` saved in option `ospsuite.plots.shapeValues`
before function call

## See also

Other setDefault functions:
[`Shapes`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/Shapes.md),
[`colorMaps`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/colorMaps.md),
[`geomPointUnicode()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/geomPointUnicode.md),
[`getDefaultGeomAttributes()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/getDefaultGeomAttributes.md),
[`getDefaultOptions()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/getDefaultOptions.md),
[`getOspsuite.plots.option()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/getOspsuite.plots.option.md),
[`resetDefaultColorMapDistinct()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/resetDefaultColorMapDistinct.md),
[`resetDefaultShapeDiscrete()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/resetDefaultShapeDiscrete.md),
[`resetDefaultTheme()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/resetDefaultTheme.md),
[`resetDefaults()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/resetDefaults.md),
[`setDefaultColorMapDistinct()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/setDefaultColorMapDistinct.md),
[`setDefaultTheme()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/setDefaultTheme.md),
[`setDefaults()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/setDefaults.md),
[`setOspsuite.plots.option()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/setOspsuite.plots.option.md)
