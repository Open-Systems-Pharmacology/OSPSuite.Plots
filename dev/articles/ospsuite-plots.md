# Overview

    #> Loading required package: ggplot2

## 1. Introduction

### 1.1 Objectives of ospsuite.plots

The main purpose of the `ospsuite.plots` library is to provide
standardized plots typically used in the context of PBPK modeling. The
library supports plot generation for the packages `OSPSuiteR` and
`OSPSuite.ReportingEngine`.

The library is based on `ggplot2` functionality and also utilizes the
`ggh4x` package.

## 2. Default Settings for Layout

`ospsuite.plots` provides default settings for the layout, including
theme, geometric aesthetics, colors, and shapes for distinct scales.

Examples within this vignette are plotted using the following test data:

``` r
testData <- exampleDataCovariates %>%
  dplyr::filter(SetID == "DataSet1") %>%
  dplyr::select(c("ID", "Age", "Obs", "Pred", "Sex"))

knitr::kable(head(testData), digits = 3)
```

|  ID | Age |  Obs | Pred | Sex  |
|----:|----:|-----:|-----:|:-----|
|   1 |  48 | 4.00 | 2.90 | Male |
|   2 |  36 | 4.40 | 5.75 | Male |
|   3 |  52 | 2.80 | 2.70 | Male |
|   4 |  47 | 3.75 | 3.05 | Male |
|   5 |   0 | 1.95 | 5.25 | Male |
|   6 |  48 | 2.45 | 5.30 | Male |

### 2.1 Plots with and without Default Layout

#### 2.1.1 Default ggplot Layout

- A plot created using the `ospsuite.plots` function
- B customized plot

``` r
# ospsuite.plots function
ospsuite.plots::plotHistogram(data = testData, mapping = aes(x = Age)) + labs(tag = "A")

# Customized plot
ggplot(data = testData, mapping = aes(x = Obs, y = Pred, color = Sex, shape = Sex)) +
  geom_point() +
  theme(legend.position = "top") +
  labs(tag = "B")
```

![Two plots comparing default ggplot styling (A) versus ospsuite.plots
styling (B). Plot A shows a histogram with default ggplot formatting.
Plot B shows a scatter plot with customized colors, shapes, and legend
positioning demonstrating enhanced styling
capabilities.](ospsuite-plots_files/figure-html/default-layout-comparison-1.png)![Two
plots comparing default ggplot styling (A) versus ospsuite.plots styling
(B). Plot A shows a histogram with default ggplot formatting. Plot B
shows a scatter plot with customized colors, shapes, and legend
positioning demonstrating enhanced styling
capabilities.](ospsuite-plots_files/figure-html/default-layout-comparison-2.png)

#### 2.1.2 Set ospsuite.plots Layout

To set the default layout, we use the same logic as in
[`ggplot2::theme_set()`](https://ggplot2.tidyverse.org/reference/get_theme.html).
The previous settings are returned invisibly, so you can easily save
them and restore them later.

[`setDefaults()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/setDefaults.md)
sets the theme, discrete color palette, shapes, and various options. All
objects can also be set separately as described below.

``` r
# Set default layout and save previous layout in variable oldDefaults
oldDefaults <- ospsuite.plots::setDefaults(defaultOptions = list(), colorMapList = NULL, shapeValues = NULL, pointAsUnicode = FALSE)

# ospsuite.plots function
ospsuite.plots::plotHistogram(data = testData, mapping = aes(x = Age)) + labs(tag = "A")

# Customized plot
ggplot(data = testData, mapping = aes(x = Obs, y = Pred, color = Sex, fill = Sex, shape = Sex)) +
  geom_point() +
  theme(legend.position = "top") +
  labs(tag = "B")
```

![Two plots showing the effect of setting ospsuite.plots defaults. Plot
A shows a histogram with ospsuite.plots styling applied. Plot B shows a
scatter plot that now automatically adopts the ospsuite.plots theme,
colors, shapes, and formatting after setDefaults() is
called.](ospsuite-plots_files/figure-html/set-ospsuite-defaults-1.png)![Two
plots showing the effect of setting ospsuite.plots defaults. Plot A
shows a histogram with ospsuite.plots styling applied. Plot B shows a
scatter plot that now automatically adopts the ospsuite.plots theme,
colors, shapes, and formatting after setDefaults() is
called.](ospsuite-plots_files/figure-html/set-ospsuite-defaults-2.png)

#### 2.1.3 Reset to Previously Saved Layout

``` r
# Reset to previously saved layout options
ospsuite.plots::resetDefaults(oldDefaults = oldDefaults)

# ospsuite.plots function
ospsuite.plots::plotHistogram(data = testData, mapping = aes(x = Age)) + labs(tag = "A")

# Customized plot
ggplot(data = testData, mapping = aes(x = Obs, y = Pred, color = Sex, shape = Sex)) +
  geom_point() +
  theme(legend.position = "top") +
  labs(tag = "B")
```

![Two plots demonstrating resetting to previously saved layout defaults.
Plot A shows the histogram reverting to original default styling after
resetDefaults() is called. Plot B shows the scatter plot also returning
to the original ggplot defaults, confirming that the layout reset worked
correctly.](ospsuite-plots_files/figure-html/reset-to-previous-defaults-1.png)![Two
plots demonstrating resetting to previously saved layout defaults. Plot
A shows the histogram reverting to original default styling after
resetDefaults() is called. Plot B shows the scatter plot also returning
to the original ggplot defaults, confirming that the layout reset worked
correctly.](ospsuite-plots_files/figure-html/reset-to-previous-defaults-2.png)

### 2.2 Default Theme

Functions to set the `ospsuite.plots` default theme only are
[`setDefaultTheme()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/setDefaultTheme.md)
and
[`resetDefaultTheme()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/resetDefaultTheme.md).
These functions are called by
[`setDefaults()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/setDefaults.md)
and
[`resetDefaults()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/resetDefaults.md).

``` r
# Set ospsuite.plots Default Theme
oldTheme <- ospsuite.plots::setDefaultTheme()

# Customize theme using ggplot functionalities
theme_update(legend.position = "top")
theme_update(legend.title = element_blank())

# Reset to the previously saved theme
ospsuite.plots::resetDefaultTheme(oldTheme)
```

### 2.3 Default Color

Functions to set the `ospsuite.plots` default color only are
[`setDefaultColorMapDistinct()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/setDefaultColorMapDistinct.md)
and
[`resetDefaultColorMapDistinct()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/resetDefaultColorMapDistinct.md).
These functions are called by
[`setDefaults()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/setDefaults.md)
and
[`resetDefaults()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/resetDefaults.md).

Colors are set to discrete and ordinal scales for `fill` and `colour`.

The package provides some color palettes in the object `colorMaps` (see
[`?colorMaps`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/colorMaps.md)).

The example below shows plots with:

- A plot with default settings for up to 6 different colors
- B plot with default settings for more than 6 different colors
- C customized settings for all following plots using the
  `colorMaps[["grays"]]`
- D customized plot: set gray scale for this plot only using function
  [`scale_fill_grey()`](https://ggplot2.tidyverse.org/reference/scale_grey.html)
- E customized settings for all following plots using
  `ggsci::pal_lancet()(9)`
- F customized plots: set gray scale for this plot only using function
  [`ggsci::scale_color_lancet()`](https://nanx.me/ggsci/reference/scale_lancet.html)

``` r
# Set ospsuite.plots Default Color
oldColors <- ospsuite.plots::setDefaultColorMapDistinct()

ggplot() +
  geom_tile(aes(x = rep(seq(1, 3), 2), y = rep(seq(1, 2), each = 3), fill = as.factor(seq(1, 6)))) +
  labs(title = "Default settings for up to 6 different colors", tag = "A") +
  theme(legend.position = "none", axis.title = element_blank())
```

![Tile plot demonstrating default color settings for up to 6 different
colors. The plot shows a 3x2 grid of colored tiles, each representing a
different discrete color from the default ospsuite.plots color palette
for small categorical
datasets.](ospsuite-plots_files/figure-html/default-color-settings-6colors-1.png)

``` r
ggplot() +
  geom_tile(aes(x = c(rep(seq(1, 7), 7), 1, 2), y = c(rep(seq(1, 7), each = 7), 8, 8), fill = as.factor(seq(1, 51)))) +
  labs(title = "Default settings for more than 6 different colors") +
  theme(legend.position = "none", axis.title = element_blank())
```

![Tile plot demonstrating default color settings for more than 6
different colors. The plot shows a 7x7 grid plus 2 additional tiles (51
total), illustrating how ospsuite.plots handles large categorical
datasets by cycling through and extending the color
palette.](ospsuite-plots_files/figure-html/default-color-settings-many-colors-1.png)

``` r
# Customize colors: set to gray colors
ospsuite.plots::setDefaultColorMapDistinct(colorMaps[["grays"]])

ggplot() +
  geom_tile(aes(x = rep(seq(1, 3), 3), y = rep(seq(1, 3), each = 3), fill = as.factor(seq(1, 9)))) +
  theme(legend.position = "none", axis.title = element_blank()) +
  labs(title = "colorMaps gray", tag = "C")
```

![Tile plot demonstrating customized gray color palette. The plot shows
a 3x3 grid of tiles using grayscale colors from the ospsuite.plots
colorMaps gray palette, illustrating how to apply monochromatic color
schemes for accessibility or publication
requirements.](ospsuite-plots_files/figure-html/customize-colors-gray-1.png)

``` r
ggplot() +
  geom_tile(aes(x = rep(seq(1, 3), 3), y = rep(seq(1, 3), each = 3), fill = as.factor(seq(1, 9)))) +
  theme(legend.position = "none", axis.title = element_blank()) +
  scale_fill_grey() +
  labs(title = "scale_fill_grey", tag = "D")
```

![Tile plot using ggplot2's built-in scale_fill_grey() function. The
plot shows a 3x3 grid of tiles demonstrating how to override
ospsuite.plots defaults with standard ggplot2 gray scale functions for
individual
plots.](ospsuite-plots_files/figure-html/ggplot-scale-fill-grey-1.png)

``` r
# Set to color palettes inspired by plots in Lancet journals
ospsuite.plots::setDefaultColorMapDistinct(ggsci::pal_lancet()(9))

ggplot() +
  geom_tile(aes(x = rep(seq(1, 3), 3), y = rep(seq(1, 3), each = 3), fill = as.factor(seq(1, 9)))) +
  theme(legend.position = "none", axis.title = element_blank()) +
  labs(title = "ggsci::pal_lancet", tag = "E")
```

![Tile plot using ggsci Lancet journal color palette. The plot shows a
3x3 grid of tiles demonstrating how to set ospsuite.plots defaults to
use professional journal color schemes inspired by The Lancet
publication
style.](ospsuite-plots_files/figure-html/ggsci-lancet-colors-1.png)

``` r
# Set to color palettes inspired by plots in Lancet journals
ospsuite.plots::setDefaultColorMapDistinct(ggsci::pal_lancet()(9))

ggplot() +
  geom_tile(aes(x = rep(seq(1, 3), 3), y = rep(seq(1, 3), each = 3), fill = as.factor(seq(1, 9)))) +
  theme(legend.position = "none", axis.title = element_blank()) +
  ggsci::scale_color_lancet() +
  labs(title = "ggsci::scale_color_lancet", tag = "F")
```

![Tile plot using ggsci Lancet color palette with scale override. The
plot shows a 3x3 grid of tiles demonstrating how to use
ggsci::scale_color_lancet() to override the default color mapping for
individual plots while maintaining the Lancet color
scheme.](ospsuite-plots_files/figure-html/ggsci-color-lancet-override-1.png)

#### Reset to Previously Saved Color Map

``` r
# Reset to the previously saved color map
ospsuite.plots::resetDefaultColorMapDistinct(oldColorMaps = oldColors)
```

### 2.4 Default Shapes

Functions to set the `ospsuite.plots` default shapes only are
`setDefaultShapeDistinct(shapeValues)` and
`resetDefaultShapeDistinct(oldShapeValue)`. These functions are called
by
[`setDefaults()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/setDefaults.md)
and
[`resetDefaults()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/resetDefaults.md).
The scales are set to the option `ospsuite.plots.shapeValues`, which is
then used to set the discrete scale of shapes for all `ospsuite.plots`
functions. For customized functions, add
`scale_shape_manual(values = getOspsuite.plots.option(optionKey = OptionKeys$ospsuite.plots.shapeValues))`.

### 2.5 Default Options

[`getDefaultOptions()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/getDefaultOptions.md)
returns a list of options used in this package. These options are set by
the function
[`setDefaults()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/setDefaults.md)
via the variable `defaultOptions`.

``` r
ospsuite.plots::setDefaults(defaultOptions = ospsuite.plots::getDefaultOptions())
```

The names of all options defined by this package start with the package
name `ospsuite.plots` as a prefix and a suffix. The suffixes are listed
in the enumeration `OptionKeys`. There are two helper functions
(`setOspsuite.plots.option` and `getOspsuite.plots.option`) to set and
get these options.

### 2.5.1 Options to Customize Watermark

All plots in this packages are created with the function
[`ggplotWithWatermark()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/ggplotWithWatermark.md)
instead of
[`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html). This
function creates a normal `ggplot` object only if printed, a watermark
is added.

Attention! If you combine plots e.g. with `cowplot:plot_grid` the
default print function without watermark is called. In this case you
have to add the watermark with the function `addwatermark(plotObject)`
before the print.

The watermark can by customized by this options:

- Switch the watermark on and off (option key = `watermark_enabled`,
  default = `TRUE`)
- Select the label (option key = `watermark_label`, default =
  “preliminary analysis”)
- Customize format (option key = `watermark_format`, default =
  `list(x = 0.5, y = 0.5, color = "grey20", angle = 30, fontsize = 12, alpha = 0.7)`)

#### Examples to Customize Watermark

- A: Change format of watermark
- B: Disable watermark
- C: Reset to default

``` r
# Change format and label of watermark
setOspsuite.plots.option(optionKey = OptionKeys$watermark_format, value = list(x = 0.2, y = 0.6, color = "red", angle = 90, fontsize = 24, alpha = 0.2))
setOspsuite.plots.option(optionKey = OptionKeys$watermark_label, value = "NEW")

# Initialize plot
ggplotWithWatermark() + labs(title = "Changed Watermark", tag = "A")
```

![Plot demonstrating customized watermark formatting. The plot shows a
blank coordinate system with a red watermark rotated 90 degrees,
positioned at specific coordinates with increased font size and reduced
transparency, labeled 'NEW' instead of the default
text.](ospsuite-plots_files/figure-html/customize-watermark-1.png)

``` r
# Disable watermark
setOspsuite.plots.option(optionKey = OptionKeys$watermark_enabled, value = FALSE)

# Initialize plot
ggplotWithWatermark() + labs(title = "No Watermark", tag = "B")
```

![Plot demonstrating disabled watermark functionality. The plot shows a
clean blank coordinate system with no watermark text overlay,
illustrating how the watermark_enabled option can be set to FALSE for
clean publication-ready
plots.](ospsuite-plots_files/figure-html/disable-watermark-1.png)

``` r
# Reset to default
setOspsuite.plots.option(optionKey = OptionKeys$watermark_format, value = NULL)
setOspsuite.plots.option(optionKey = OptionKeys$watermark_label, value = NULL)
setOspsuite.plots.option(optionKey = OptionKeys$watermark_enabled, value = NULL)

# Initialize plot
ggplotWithWatermark() + labs(title = "Default Watermark", tag = "C")
```

![Plot demonstrating reset watermark to default settings. The plot shows
a blank coordinate system with the default watermark restored - gray
text reading 'preliminary analysis' at 30-degree angle in the center
with standard transparency and font
size.](ospsuite-plots_files/figure-html/reset-watermark-default-1.png)

### 2.5.2 Options to Set the Defaults for Geom Layer Attributes

All plot functions have input variables `geom*Attributes` which are
passed as variables to the corresponding ggplot layer. The defaults of
the input variables can be set by options.

|     functions     |        Line        |        Ribbon        |        Point        |        Errorbar        |        LLOQ        |        Hist        |                  Boxplot                   |        ComparisonLine        |        GuestLine        |
|:-----------------:|:------------------:|:--------------------:|:-------------------:|:----------------------:|:------------------:|:------------------:|:------------------------------------------:|:----------------------------:|:-----------------------:|
| plotTimeProfile() | geomLineAttributes | geomRibbonAttributes | geomPointAttributes | geomErrorbarAttributes | geomLLOQAttributes |                    |                                            |                              |                         |
|  plotResVsCov()   |                    |                      | geomPointAttributes | geomErrorbarAttributes | geomLLOQAttributes |                    |                                            | geomComparisonLineAttributes | geomGuestLineAttributes |
| plotRatioVsCov()  |                    |                      | geomPointAttributes | geomErrorbarAttributes | geomLLOQAttributes |                    |                                            | geomComparisonLineAttributes | geomGuestLineAttributes |
|  plotPredVsObs()  |                    |                      | geomPointAttributes | geomErrorbarAttributes | geomLLOQAttributes |                    |                                            | geomComparisonLineAttributes | geomGuestLineAttributes |
|  plotHistogram()  |                    |                      |                     |                        |                    | geomHistAttributes |                                            |                              |                         |
| plotBoxWhisker()  |                    |                      |                     |                        |                    |                    | geomBoxplotAttributes, geomPointAttributes |                              |                         |

Usage of options for geom attributes

With default options:

- `LineAttributes = list()`
- `Ribbon = list(color = NA)`
- `PointAttributes = list()`
- `ErrorbarAttributes = list(width = 0)`
- `LLOQAttributes = list()`
- `ComparisonLineAttributes = list(linetype = "dashed")`
- `GuestLineAttributes = list(linetype = "dashed")`
- `BoxplotAttributes = list(position = position_dodge(width = 1), color = "black")`
- `HistAttributes = list(bins = 10, position = ggplot2::position_nudge())`

### 2.5.3 Options to Set Defaults for Aesthetics

Options to set the face alpha of ribbons filled points and options to
set the filled points for values below and above LLOQ.

``` r
# Default alpha = 0.5
getOspsuite.plots.option(optionKey = "Alpha")

# Alpha of LLOQ values
c("TRUE" = 0.3, "FALSE" = 1)
getOspsuite.plots.option(optionKey = "LLOQAlphaVector")

# Linetype LLOQ comparison lines
"dashed"
getOspsuite.plots.option(optionKey = "LLOQLineType")
```

### 2.5.4 Options to Define Export Format

There are options to define the export format using the function
`exportPlot` (see details below):

- `export.width`: Width of the exported file
- `export.units`: Units for width and height
- `export.device`: Device for plot export
- `export.dpi`: Plot resolution

The latter options are used directly as input for
[`ggplot2::ggsave`](https://ggplot2.tidyverse.org/reference/ggsave.html),
so check the help for available values.

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
  `observed` is mapped to `x` and `predicted` is mapped to `y`. For all
  other functions, residuals are calculated if both aesthetics are
  mapped. The variable `residualScale` defines the calculation method:
  - `residualScale = "log"`: $log(observed) - log(predicted)$,
  - `residualScale = "linear"`: $observed - predicted$,
  - `residualScale = "ratio"`: $observed/predicted$.

See `vignette("Goodness of Fit", package = "ospsuite.plots")` for
examples.

|     functions     | `groupby` | `lloq` | `error` | `error_relative` | `y2axis` | `mdv` | `observed` / `predicted` |
|:-----------------:|:---------:|:------:|:-------:|:----------------:|:--------:|:-----:|:------------------------:|
| plotTimeProfile() |     X     |   X    |    X    |        X         |    X     |   X   |                          |
|  plotHistogram()  |     X     |        |         |                  |          |   X   |            X             |
|  plotPredVsObs()  |     X     |   X    |    X    |        X         |          |   X   |            X             |
|  plotResVsCov()   |     X     |        |    X    |        X         |          |   X   |            X             |
| plotRatioVsCov()  |     X     |        |    X    |        X         |          |   X   |            X             |
|     plotQQ()      |     X     |        |         |                  |          |   X   |            X             |
| plotBoxWhisker()  |           |        |         |                  |          |   X   |            X             |

Applicability of additional aesthetics in functions

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
plotObject <- ospsuite.plots::plotHistogram(data = testData, mapping = aes(x = Age))
```

Exporting the Plot: Using the `exportPlot` function, you can easily save
this plot to a file:

Replace “path/to/save” with the actual directory path where you want to
save the plot, and adjust the width and height parameters as needed.

``` r
exportPlot(plotObject = plotObject, filepath = "path/to/save", filename = "myplot", width = 10, height = 8)
```

### 5.2 Advanced Usage

#### Adjusting Plot Dimensions Based on Content

The `exportPlot` function can automatically adjust the plot dimensions
based on its content, such as the presence of a legend or the aspect
ratio.

Assuming `plotObject` is your ggplot object:

``` r
exportPlot(plotObject = plotObject, filepath = "path/to/save", filename = "adjusted_plot.png")
```

In this case, you don’t need to specify the width and height explicitly;
the function calculates them for you. The default width value saved in
the `ospsuite.plots` option `export.width` is used. If an aspect ratio
is defined in the theme of the plot, height will be adjusted
accordingly; otherwise, the function exports square figures.

## 6. Shapes

### 6.1 Default Shapes

``` r
shapeNames <- c("circle", paste("circle", c("open", "filled", "cross", "plus", "small")), "bullet", "square", paste("square", c("open", "filled", "cross", "plus", "triangle")), "diamond", paste("diamond", c("open", "filled", "plus")), "triangle", paste("triangle", c("open", "filled", "square")), paste("triangle down", c("open", "filled")), "plus", "cross", "asterisk")

shapes <- data.frame(shapeNames = shapeNames, x = c(1:7, 1:6, 1:3, 5, 1:3, 6, 2:3, 1:3), y = -rep(1:6, c(7, 6, 4, 4, 2, 3)))

ggplot(shapes, aes(x, y)) +
  geom_point(aes(shape = shapeNames), color = "blue", fill = "red", size = 5, stroke = 1) +
  geom_text(aes(label = shapeNames), nudge_y = -0.3, size = 3.5) +
  scale_shape_identity() +
  theme_void()
```

![Chart displaying all default shape types available in ospsuite.plots.
Shows various point shapes including circles, squares, diamonds,
triangles in different styles (open, filled, cross, plus) arranged in a
grid with labels, demonstrating the visual appearance of each shape
option.](ospsuite-plots_files/figure-html/default-shapes-1.png)

### 6.2 Use Unicode Symbols with `{showtext}`

Attention: The use of [showtext](https://github.com/yixuan/showtext) has
side effects - a customized `geom_Point` function `geomPointUnicode` has
to be used.

``` r
shapes <- data.frame(shapeNames = names(Shapes), shape_symbols = unlist(unname(Shapes)), x = rep(c(1:5), 8), y = rep(-c(1:8), each = 5))

showtext::showtext_auto()
ggplot(shapes, aes(x, y)) +
  geomPointUnicode(aes(shape = shape_symbols), color = "blue", fill = "red", size = 5) +
  geom_text(aes(label = shapeNames), nudge_y = -0.3, size = 3.5) +
  scale_shape_identity() +
  theme_void()
showtext::showtext_auto(enable = "off")
```

![Chart displaying Unicode symbol shapes available in ospsuite.plots
with showtext package. Shows various Unicode symbols arranged in a grid
with their corresponding shape names, demonstrating enhanced typography
options for point symbols in
plots.](ospsuite-plots_files/figure-html/unicode-shapes-1.png)

### 6.3 Switch Between Modes

To switch to the Unicode mode, call `setDefaults` with the input
variable `pointAsUnicode = TRUE`. To switch back, use
`setDefaults(pointAsUnicode = FALSE)` or `resetDefaults(oldDefaults)`.
However, it is recommended to produce all plots of one workflow either
with Unicode mode or without. Otherwise, the plots may have different
fonts.

``` r
oldDefaults <- ospsuite.plots::setDefaults(pointAsUnicode = TRUE)

dt <- data.frame(x = c(1, 2, 1, 2), y = c(1, 1, 2, 2), species = c("pig", "dog", "mouse", "rat"))

plotObject <- plotYVsX(data = dt, mapping = aes(x = x, y = y, groupby = species), xScale = "linear", xScaleArgs = list(limits = c(0.5, 2.5)), yScale = "linear", yScaleArgs = list(limits = c(0.5, 2.5)))

plot(plotObject)
```

![Scatter plot demonstrating Unicode point symbols in ospsuite.plots.
Shows four data points representing different species (pig, dog, mouse,
rat) using Unicode symbols instead of standard ggplot shapes, with each
species having a distinct symbol and
color.](ospsuite-plots_files/figure-html/unicode-mode-demo-1.png)

#### Use Non-Default Icons

``` r
plot(plotObject +
  scale_shape_manual(values = c(pig = "pig", dog = "dog", mouse = "mouse", rat = "rat")))
```

![Scatter plot demonstrating the use of non-default icons in
ospsuite.plots. The plot displays four data points representing
different species (pig, dog, mouse, rat) using custom shapes for each
species instead of standard ggplot shapes, showcasing the ability to
personalize plot aesthetics with unique
icons.](ospsuite-plots_files/figure-html/export-non-default-icons-1.png)

\`\`\`
