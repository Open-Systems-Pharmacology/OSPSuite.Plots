# sets the defaults for the OSPSuite.plots package

should be started at the beginning at each workflow

## Usage

``` r
setDefaults(
  defaultOptions = list(),
  colorMapList = NULL,
  shapeValues = NULL,
  pointAsUnicode = FALSE
)
```

## Arguments

- defaultOptions:

  list of options

- colorMapList:

  list of color maps

- shapeValues:

  list of Shapes

- pointAsUnicode:

  A `flag` to switch between mode for geom_point, if TRUE points will be
  plotted as unicode labels

## Value

list of old settings which can be used to reset defaults with
[`resetDefaults()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/resetDefaults.md)

## Details

for detailed information see
`vignette("ospsuite.plots", package = "ospsuite.plots")`

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
[`setDefaultShapeDiscrete()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/setDefaultShapeDiscrete.md),
[`setDefaultTheme()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/setDefaultTheme.md),
[`setOspsuite.plots.option()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/setOspsuite.plots.option.md)
