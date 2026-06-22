# Overview

## 1. Introduction

The main purpose of the `ospsuite.plots` library is to provide
standardized plots typically used in the context of PBPK modeling. The
library supports plot generation for the packages `OSPSuiteR` and
`OSPSuite.ReportingEngine`.

The library is based on `ggplot2` functionality and also utilizes the
`ggh4x` package.

## 2. Styling ospsuite.plots Plots

Every `ospsuite.plots` plot function already produces a fully styled
plot, so you do not need to configure anything to get the
`ospsuite.plots` look. When you *do* want to change something, there are
two distinct mechanisms, and it helps to know which one to reach for:

- **OSP theme and custom scales** ([Section
  2.1](#osp-theme-and-custom-scales)) shape the *frame and the mapped
  aesthetics* of a single plot, by adding them like any other `ggplot2`
  component:
  [`theme_osp()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/theme_osp.md)
  for the theme (background, grid, legend, titles), and
  [`scale_colour_osp()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/scale_osp.md)
  /
  [`scale_fill_osp()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/scale_osp.md)
  /
  [`scale_shape_osp()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/scale_shape_osp.md)
  for the discrete `ospsuite.plots` color and shape palettes. They
  return ordinary `ggplot2` objects and never change global state, so
  they also let you bring the `ospsuite.plots` look to a plot you built
  yourself with plain `ggplot2`.
- **Session options** ([Section 2.2](#session-options)) set *defaults
  read at plot-function call time*: the geom aesthetics of the drawn
  marks (fill alpha, line width, error-bar caps, …), statistical
  behaviour (percentiles, histogram bins), and side features (watermark,
  export format). Set them once and every later `ospsuite.plots` plot in
  the session picks them up; or pass the matching `geom*Attributes`
  argument to override just one call.

In short: reach for a **constructor** to restyle the look of *one plot*
(especially a hand-built `ggplot2` plot); set an **option** to change
the *default marks or behaviour* across your analysis.

Examples within this vignette are plotted using the following test data:

``` r

testData <- exampleDataCovariates |>
  dplyr::filter(SetID == "DataSet1") |>
  dplyr::select(c("ID", "Age", "Obs", "Pred", "Sex"))
```

### 2.1 OSP Theme and Custom Scales

The OSP theme and scales are added to a plot like any other `ggplot2`
component and return ordinary `ggplot2` objects, so they do not change
global state. The `ospsuite.plots` plot functions apply them internally;
you add them yourself only when styling a plain `ggplot2` plot.

The same plot with the default `ggplot2` layout (A) and with the
`ospsuite.plots` constructors applied (B):

``` r

basePlot <- ggplot(testData, aes(x = Obs, y = Pred, color = Sex)) +
  geom_point()

# A: default ggplot2 layout
basePlot + labs(title = "A: default ggplot2")

# B: ospsuite.plots constructors added to the same plot
basePlot +
  theme_osp() +
  scale_colour_osp() +
  labs(title = "B: ospsuite.plots (theme_osp + scale_colour_osp)")
```

![Two scatter plots of the same data. Plot A uses the default ggplot2
layout with the grey theme and default colors. Plot B applies the
ospsuite.plots constructors theme_osp() and scale_colour_osp(), showing
the white theme and ospsuite.plots
colors.](ospsuite-plots_files/figure-html/default-layout-comparison-1.png)![Two
scatter plots of the same data. Plot A uses the default ggplot2 layout
with the grey theme and default colors. Plot B applies the
ospsuite.plots constructors theme_osp() and scale_colour_osp(), showing
the white theme and ospsuite.plots
colors.](ospsuite-plots_files/figure-html/default-layout-comparison-2.png)

#### 2.1.1 Theme: `theme_osp()`

[`theme_osp()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/theme_osp.md)
is a regular `ggplot2` theme, built on
[`theme_bw()`](https://ggplot2.tidyverse.org/reference/ggtheme.html),
that styles the **frame** of a plot (panel background, grid, legend
placement, title and subtitle); it controls everything that is not the
plotted data. Because it is an ordinary theme object, it behaves exactly
like the built-in `theme_*()` themes: use it as the starting point and
layer your own choices on top with a
[`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) call
(the later
[`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) wins for
the elements it sets). It also accepts the usual
[`theme_bw()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)
arguments such as `base_size`.

``` r

# start from the OSP theme, then apply custom choices on top (these win)
ggplot(mpg, aes(x = displ, y = hwy, color = class)) +
  geom_point() +
  labs(
    title = "Highway mileage vs engine size",
    subtitle = "ggplot2 mpg dataset, coloured by car class",
    x = "engine displacement (L)",
    y = "highway miles per gallon"
  ) +
  theme_osp() +
  theme(
    legend.position = "left",
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0),
    panel.grid.major = element_line(linetype = "dotted")
  )
```

![Scatter plot of the ggplot2 mpg dataset (engine size versus highway
mileage, coloured by car class) styled by starting from theme_osp() and
overriding it: the legend is moved to the left, the title and subtitle
are left-aligned and the major grid is
dotted.](ospsuite-plots_files/figure-html/theme_osp-1.png)

#### 2.1.2 Color: `scale_colour_osp()` and `scale_fill_osp()`

[`scale_colour_osp()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/scale_osp.md)
and
[`scale_fill_osp()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/scale_osp.md)
apply the `ospsuite.plots` discrete color palette to the mapped
`colour`/`fill` aesthetics (`colorMaps$default` for up to 6 groups,
`colorMaps$ospDefault` beyond; see
[`?colorMaps`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/colorMaps.md)).
As in `ggplot2`,
[`scale_color_osp()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/scale_osp.md)
is an alias of
[`scale_colour_osp()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/scale_osp.md).
As ordinary scales, they are overridden by any other `scale_colour_*()`
/ `scale_fill_*()` you add.

The palette used beyond six groups (`colorMaps$ospDefault`):

![Color swatch grid of the ospsuite.plots ospDefault palette, the colors
used by scale_fill_osp() and scale_colour_osp() beyond six
groups.](ospsuite-plots_files/figure-html/scale-fill-osp-1.png)

#### 2.1.3 Shape: `scale_shape_osp()`

All point layers produced by `ospsuite.plots` use
[`geom_point_osp()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/geom_point_osp.md),
which draws from the OSP shape palette defined in `ospShapeNames`. When
you build your own plots and want them to combine consistently with
`ospsuite.plots` outputs, prefer
[`geom_point_osp()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/geom_point_osp.md)
over the raw
[`ggplot2::geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html)
(raw
[`geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html)
keeps using ggplot2’s native pch shapes and is unaffected). Shape
ordering is controlled per plot:

- [`scale_shape_osp()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/scale_shape_osp.md)
  assigns shapes automatically from `ospShapeNames`.
- `scale_shape_osp_manual(values = c(level1 = "circle", level2 = "diamond", ...))`
  sets an explicit mapping.
- [`scale_shape_osp_identity()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/scale_shape_osp_identity.md)
  is the right choice when the data already contains shape names.

The available shapes:

![Chart displaying all default OSP shape types available in
ospsuite.plots. Shows various point shapes arranged in a grid with
labels, demonstrating the visual appearance of each shape
option.](ospsuite-plots_files/figure-html/default-shapes-1.png)

#### 2.1.4 Applying the constructors globally (deprecated)

To style many unrelated (non-`ospsuite.plots`) plots in a session at
once,
[`setDefaultTheme()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/setDefaultTheme.md)
sets
[`theme_osp()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/theme_osp.md)
as the global `ggplot2` theme via
[`theme_set()`](https://ggplot2.tidyverse.org/reference/get_theme.html),
and
[`setDefaults()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/setDefaults.md)
additionally mutates the global geom defaults and discrete color
options. Both are **deprecated**: `ospsuite.plots` plots no longer need
them, and the per-plot
[`theme_osp()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/theme_osp.md)
and scales above are the recommended path. They still work and return
the previous settings so you can restore them with
[`resetDefaultTheme()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/resetDefaultTheme.md)
/
[`resetDefaults()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/resetDefaults.md).

``` r

# Deprecated: style every plot in the session, including non-ospsuite.plots ones
oldTheme <- ospsuite.plots::setDefaultTheme()

# Restore the previous global theme
ospsuite.plots::resetDefaultTheme(oldTheme)
```

### 2.2 Session Options

Session options are read by the `ospsuite.plots` plot functions at call
time (falling back to the package defaults when an option is unset), so
setting one changes the default for every later plot in the session. Set
them with `options(ospsuite.plots.<key> = value)` and read them with
`getOption("ospsuite.plots.<key>")`; the examples below use this form.
The helpers `setOspsuite.plots.option(optionKey, value)` and
`getOspsuite.plots.option(optionKey)` do the same using the keys from
the `OptionKeys` enumeration, and
[`getDefaultOptions()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/getDefaultOptions.md)
returns the full default list.

``` r

# inspect the default options
ospsuite.plots::getDefaultOptions()
```

The options fall into three groups:

1.  **Geom default aesthetics** ([Section
    2.2.1](#geom-default-aesthetics)): how the drawn marks look (fill
    alpha, line width, error-bar caps, bar outline, …).
2.  **Statistical and plotting behaviour** ([Section
    2.2.2](#statistical-and-plotting-behaviour)): what is computed or
    drawn, not how it looks (percentiles, histogram bins, LLOQ
    handling).
3.  **Side features** ([Section
    2.2.3](#side-features-watermark-and-export)): the watermark and the
    export format.

#### 2.2.1 Geom Default Aesthetics

All plot functions have `geom*Attributes` input variables that are
passed on to the corresponding `ggplot2` layer; their defaults are
session options. This is how the `ospsuite.plots` look of the drawn
marks (such as the fill alpha of ribbons and boxes, the line width, or
the error-bar cap width) is applied per layer, rather than by mutating
the global `ggplot2` geom defaults. Override one for a single plot by
passing the argument
(e.g. `plotRangeDistribution(..., geomRibbonAttributes = list(alpha = 0.3))`),
or for the session by setting the option.

| functions | Line | Ribbon | Point | Errorbar | LLOQ | Hist | Boxplot | ComparisonLine | GuestLine |
|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|
| plotTimeProfile() | geomLineAttributes | geomRibbonAttributes | geomPointAttributes | geomErrorbarAttributes | geomLLOQAttributes |  |  |  |  |
| plotResVsCov() |  |  | geomPointAttributes | geomErrorbarAttributes | geomLLOQAttributes |  |  | geomComparisonLineAttributes | geomGuestLineAttributes |
| plotRatioVsCov() |  |  | geomPointAttributes | geomErrorbarAttributes | geomLLOQAttributes |  |  | geomComparisonLineAttributes | geomGuestLineAttributes |
| plotPredVsObs() |  |  | geomPointAttributes | geomErrorbarAttributes | geomLLOQAttributes |  |  | geomComparisonLineAttributes | geomGuestLineAttributes |
| plotHistogram() |  |  |  |  |  | geomHistAttributes |  |  |  |
| plotBoxWhisker() |  |  |  |  |  |  | geomBoxplotAttributes, geomPointAttributes |  |  |

Usage of options for geom attributes {.table}

The default fill alpha and line width are shared across these attributes
through the `alpha` option (default `0.5`); setting it adjusts the
transparency of every filled mark:

``` r

# Make all filled marks more transparent for the session
options(ospsuite.plots.alpha = 0.3)
```

#### 2.2.2 Statistical and Plotting Behaviour

These options change *what* a plot computes or draws, not how the marks
look.

- `percentiles`: five quantiles (lower whisker, lower box, median, upper
  box, upper whisker) used by
  [`plotBoxWhisker()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotBoxWhisker.md)
  (default `c(0.05, 0.25, 0.5, 0.75, 0.95)`).
- `defaultPercentiles`: three quantiles used by
  [`plotRangeDistribution()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotRangeDistribution.md)
  (default `c(0.05, 0.5, 0.95)`).
- `geomHistAttributes$bins`: the default number of histogram bins.
- `lloqAlphaVector` / `lloqLineType`: the transparency of points below
  the LLOQ and the line type of the LLOQ line.

``` r

# Change box-whisker percentiles for the session
options(ospsuite.plots.percentiles = c(0.1, 0.25, 0.5, 0.75, 0.9))

# Change range-plot percentiles for the session
options(ospsuite.plots.defaultPercentiles = c(0.1, 0.5, 0.9))
```

#### 2.2.3 Side Features: Watermark and Export

##### Watermark

All plots in this package are created with
[`ggplotWithWatermark()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/ggplotWithWatermark.md)
instead of
[`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html), which
adds a watermark on print. The watermark is **enabled by default**;
setting the option to `NULL` is treated the same as the default, so it
still appears. You only need these options to customize or remove it.

``` r

# Disable watermarks globally (e.g. in your .Rprofile, editable via usethis::edit_r_profile())
options(ospsuite.plots.watermarkEnabled = FALSE)
```

**Attention!** If you combine plots, e.g. with
[`cowplot::plot_grid`](https://wilkelab.org/cowplot/reference/plot_grid.html),
the default print function without watermark is called. In that case add
the watermark with `addWatermark(plotObject)` before printing.

The watermark is controlled by three options:

- `watermarkEnabled` (default `TRUE`): switch the watermark on or off.
- `watermarkLabel` (default `"preliminary analysis"`): the text.
- `watermarkFormat` (default
  `list(x = 0.5, y = 0.5, color = "lightgrey", angle = 30, fontsize = 12, alpha = 0.7)`):
  the appearance.

``` r

# A: default watermark
ggplotWithWatermark() + labs(title = "A: default watermark")
```

![Plot with the default ospsuite.plots watermark: light grey text
reading 'preliminary analysis' at a 30-degree angle in the centre of the
panel.](ospsuite-plots_files/figure-html/default-watermark-1.png)

``` r

# B: customized watermark
options(ospsuite.plots.watermarkLabel = "CONFIDENTIAL")
options(
  ospsuite.plots.watermarkFormat = list(
    x = 0.5,
    y = 0.5,
    color = "purple",
    angle = 45,
    fontsize = 18,
    alpha = 0.35
  )
)

ggplotWithWatermark() + labs(title = "B: customized watermark")
```

![Plot demonstrating a customized watermark: a large, bold,
semi-transparent purple 'CONFIDENTIAL' label rotated 45 degrees across
the centre of the
panel.](ospsuite-plots_files/figure-html/customize-watermark-1.png)

``` r

# C: no watermark
options(ospsuite.plots.watermarkEnabled = FALSE)

ggplotWithWatermark() + labs(title = "C: no watermark")
```

![Plot with the watermark disabled: a clean blank coordinate system with
no watermark text
overlay.](ospsuite-plots_files/figure-html/disable-watermark-1.png)

##### Export

[`exportPlot()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/exportPlot.md)
(see [Section 5](#plot-export)) uses these options as defaults, passed
on to
[`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html):

- `exportWidth`: width of the exported file
- `exportUnits`: units for width and height
- `exportDevice`: device for plot export
- `exportDpi`: plot resolution

## 3. Plot Functions

All plot functions listed below call internally the function
[`initializePlot()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/initializePlot.md).
This function constructs labels from the metadata and adds a watermark
layer. It can also be used to create a customized ggplot.

### 3.1 `plotTimeProfile()`

[Time Profile
Plots](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/articles/plot-time-profile.md)

### 3.2 `plotBoxWhisker()`

[Box Whisker
Plots](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/articles/box-whisker-vignette.md)

### 3.3 `plotHistogram()`

[Histogram
Plots](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/articles/histogram.md)

### 3.4 `plotPredVsObs()`

[Goodness of
Fit](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/articles/Goodness_of_fit.md)

### 3.5 `plotResVsCov()`

[Goodness of
Fit](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/articles/Goodness_of_fit.md)

### 3.6 `plotRatioVsCov()`

[Goodness of
Fit](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/articles/Goodness_of_fit.md)

### 3.7 `plotQQ()`

[Goodness of
Fit](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/articles/Goodness_of_fit.md)

## 4. Additional Aesthetics

This package provides some additional aesthetics.

For more details, see the examples within the vignettes for the
respective functions.

- `groupby`: Shortcut to use different aesthetics to group. All
  functions where this aesthetic is used have also a variable
  `groupAesthetics`. The mapping `groupby` is copied to all aesthetics
  listed within this variable and to the aesthetic `group`.
- `lloq`: Mapped to a column with values indicating the lower limit of
  quantification, adds horizontal (or vertical) lines to the plot. All
  observed values below the “lloq” are plotted with a lighter alpha. As
  values are compared row by row, it is possible to have more than one
  LLOQ.
- `error`: Mapped to a column with additive error (e.g., standard
  deviation); error bars are plotted. This is a shortcut to map `ymin`
  and `ymax` directly (`ymin = y - error` and `ymax = y + error`).
  Additionally, if `yScale` is set, `ymin` values below 0 are set to
  `y`.
- `error_relative`: Mapped to a column with relative error (e.g.,
  geometric standard deviation); error bars are plotted. This is a
  shortcut to map `ymin` and `ymax` directly
  (`ymin = y / error_relative` and `ymax = y * error_relative`).
- `y2axis`: Creates a plot with 2 y axes. It is used to map to a column
  with a logical value. Values where this column has a TRUE entry will
  be displayed with a secondary axis.
- `mdv`: Mapped to a logical column. Rows where this column has entries
  set to TRUE are not plotted (MDV = missing data value, taken from
  NONMEM notation).
- `observed` / `predicted`: For the function
  [`plotPredVsObs()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotPredVsObs.md),
  `observed` is mapped to `x` and `predicted` is mapped to `y`.

Residuals and ratios must be pre-calculated before passing them to the
plotting functions. If you are using the `{ospsuite}` package, use
`ospsuite::addResidualColumn()` to add a residual column to your data.

See `vignette("Goodness of Fit", package = "ospsuite.plots")` for
examples.

| functions | `groupby` | `lloq` | `error` | `error_relative` | `y2axis` | `mdv` | `observed` / `predicted` |
|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|
| plotTimeProfile() | X | X | X | X | X | X |  |
| plotHistogram() | X |  |  |  |  | X |  |
| plotPredVsObs() | X | X | X | X |  | X | X |
| plotResVsCov() | X |  | X | X |  | X |  |
| plotRatioVsCov() | X |  | X | X |  | X |  |
| plotQQ() | X |  |  |  |  | X |  |
| plotBoxWhisker() |  |  |  |  |  | X |  |

Applicability of additional aesthetics in functions {.table
style="width:100%;"}

## 5. Plot Export

This section demonstrates how to use the `exportPlot` function to save
`ggplot` objects to files, adjusting the width and height of the
exported plots as necessary. This function is part of the
`ospsuite.plots` package and simplifies the process of exporting plots
for various purposes, such as publication or presentation.

### 5.1 Basic Usage

To export a plot, you need a ggplot object. Here’s a basic example:

Create a simple ggplot object:

``` r

plotObject <- ospsuite.plots::plotHistogram(
  data = testData,
  mapping = aes(x = Age)
)
```

Exporting the Plot: Using the `exportPlot` function, you can easily save
this plot to a file:

Replace `"path/to/save"` with the actual directory path where you want
to save the plot, and adjust the width and height parameters as needed.

``` r

exportPlot(
  plotObject = plotObject,
  filepath = "path/to/save",
  filename = "myplot",
  width = 10,
  height = 8
)
```

### 5.2 Advanced Usage

#### Adjusting Plot Dimensions Based on Content

The `exportPlot` function can automatically adjust the plot dimensions
based on its content, such as the presence of a legend or the aspect
ratio.

Assuming `plotObject` is your ggplot object:

``` r

exportPlot(
  plotObject = plotObject,
  filepath = "path/to/save",
  filename = "adjusted_plot.png"
)
```

In this case, you don’t need to specify the width and height explicitly;
the function calculates them for you. The default width value saved in
the `ospsuite.plots` option `exportWidth` is used. If an aspect ratio is
defined in the theme of the plot, height will be adjusted accordingly;
otherwise, the function exports square figures.
