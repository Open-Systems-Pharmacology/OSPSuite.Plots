# Wrap long legend entries based on the plot dimensions

Convenience wrapper around
[`wrapLegendLabels()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/wrapLegendLabels.md)
and
[`legendWidthFromPlotDimensions()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/legendWidthFromPlotDimensions.md):
derives the maximum number of characters per line from the exported plot
dimensions and applies the wrapping to the plot legend in one call.

## Usage

``` r
wrapLegend(plotObject, plotWidth = NULL, unit = NULL, dpi = NULL, ...)
```

## Arguments

- plotObject:

  A `ggplot` object, e.g. created by
  [`plotTimeProfile()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotTimeProfile.md),
  or a `CombinedPlot` object whose main plot is updated.

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

- ...:

  Additional arguments passed on to
  [`legendWidthFromPlotDimensions()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/legendWidthFromPlotDimensions.md)
  (`legendFractionHorizontal`, `legendFractionVertical`).

## Value

The `ggplot` object with wrapped legend labels (the input object is not
modified).

## See also

Other legend helpers:
[`legendWidthFromPlotDimensions()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/legendWidthFromPlotDimensions.md),
[`wrapLegendLabels()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/wrapLegendLabels.md)

## Examples

``` r
df <- data.frame(
  time = rep(seq(1, 24), 2),
  concentration = c(dexp(1:24, 0.2), dexp(1:24, 0.1)),
  output = rep(c(
    "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
    "Organism|PeripheralVenousBlood|Aciclovir|Muscle (Muscle Interstitial)"
  ), each = 24)
)
p <- plotTimeProfile(
  data = df,
  mapping = aes(x = time, y = concentration, color = output)
)
# wrap legend entries to fit a bottom legend on an 8 cm wide plot
pWrapped <- wrapLegend(
  p + theme(legend.position = "bottom"),
  plotWidth = 8,
  unit = "cm"
)
```
