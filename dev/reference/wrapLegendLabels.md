# Wrap long legend entries of a plot

Applies automatic line wrapping to the legend entries (key labels) of a
plot, so that long entries - such as simulation output paths like
`Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)` -
are broken over several lines instead of shrinking the plot panel or
being cut off.

The function wraps the labels of all scales that can contribute to the
legend and are mapped in the plot: `colour`/`color`, `fill`, `shape` and
`linetype`.

- Scales already present on the plot (e.g.
  [`scale_colour_osp()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/scale_osp.md)
  or the LLOQ
  [`scale_linetype_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.html))
  keep their palettes, values and guides - only their labels are
  wrapped. Named `labels` and label functions are resolved first, so the
  wrapped text is exactly what would have been displayed.

- For aesthetics that are mapped but have no scale yet, a scale of the
  same type as the one the plot would get at build time is added with
  wrapped labels
  ([`scale_colour_osp()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/scale_osp.md)
  /
  [`scale_fill_osp()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/scale_osp.md)
  /
  [`scale_shape_osp()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/scale_shape_osp.md)
  for `ospsuite.plots` plots, the `ggplot2` defaults otherwise), so the
  appearance of the plot is unchanged.

Strings without spaces longer than `maxWidth` (e.g. compound output
paths) are split using
`stringr::str_wrap(width = maxWidth, whitespace_only = FALSE)`, so that
long tokens are broken into smaller chunks.

Combine with
[`legendWidthFromPlotDimensions()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/legendWidthFromPlotDimensions.md)
to derive `maxWidth` from the exported plot size, or use the convenience
wrapper
[`wrapLegend()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/wrapLegend.md).

## Usage

``` r
wrapLegendLabels(plotObject, maxWidth)
```

## Arguments

- plotObject:

  A `ggplot` object, e.g. created by
  [`plotTimeProfile()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotTimeProfile.md),
  or a `CombinedPlot` object whose main plot is updated.

- maxWidth:

  Maximum number of characters per legend line.

## Value

The `ggplot` object with wrapped legend labels (the input object is not
modified).

## See also

Other legend helpers:
[`legendWidthFromPlotDimensions()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/legendWidthFromPlotDimensions.md),
[`wrapLegend()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/wrapLegend.md)

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
# wrap legend entries at 20 characters per line
pWrapped <- wrapLegendLabels(p, maxWidth = 20)

# wrap at a width derived from the exported plot dimensions
pWrapped <- wrapLegendLabels(
  p,
  maxWidth = legendWidthFromPlotDimensions(p, plotWidth = 8, unit = "cm")
)
```
