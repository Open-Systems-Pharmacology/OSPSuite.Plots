# Maximum number of legend characters per line for given plot dimensions

Calculates the maximum number of characters per line that fit into the
legend of a plot with the given exported dimensions - the `maxWidth` to
be used with
[`wrapLegendLabels()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/wrapLegendLabels.md).

The calculation is based on:

- the legend position of the plot: legends on the left or right side are
  given at most `legendFractionHorizontal` of the total plot width
  (default 40%), legends at the top or bottom are given
  `legendFractionVertical` (default 100%);

- the legend font size;

## Usage

``` r
legendWidthFromPlotDimensions(
  plotObject,
  plotWidth = NULL,
  unit = NULL,
  dpi = NULL,
  legendFractionHorizontal = 0.4,
  legendFractionVertical = 1
)
```

## Arguments

- plotObject:

  A `ggplot` object, e.g. created by
  [`plotTimeProfile()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotTimeProfile.md),
  or a `CombinedPlot` object whose main plot is used.

- plotWidth:

  Width of the exported plot. If `NULL`, the default export width option
  `exportWidth` is used.

- unit:

  Unit of `plotWidth`: one of `"cm"`, `"in"`, `"mm"` or `"px"`. If
  `NULL`, the default export width option `exportUnits` is used.

- dpi:

  Resolution in dots per inch; used to convert `plotWidth` to physical
  inches when `unit = "px"`. If `NULL`, the default export DPI option
  `exportDpi` is used.

- legendFractionHorizontal:

  Maximum fraction of the total plot width available to legends on the
  left or right side. Ignored for top/bottom legends.

- legendFractionVertical:

  Maximum fraction of the total plot width available to legends on the
  top or bottom side. Ignored for left/right and inside legends.

## Value

A single integer, the maximum number of characters per legend line (at
least 1).

## See also

Other legend helpers:
[`wrapLegend()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/wrapLegend.md),
[`wrapLegendLabels()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/wrapLegendLabels.md)

## Examples

``` r
df <- data.frame(
  time = 1:10,
  concentration = (1:10)^2,
  output = rep("Organism|PeripheralVenousBlood|Aciclovir|Plasma", 10)
)
p <- plotTimeProfile(
  data = df,
  mapping = aes(x = time, y = concentration, color = output)
)
# legend on the right may use 40% of an 8 cm wide plot
legendWidthFromPlotDimensions(p, plotWidth = 8, unit = "cm")
#> [1] 18

# bottom legend may use the full plot width
pBottom <- p + theme(legend.position = "bottom")
legendWidthFromPlotDimensions(pBottom, plotWidth = 8, unit = "cm")
#> [1] 46
```
