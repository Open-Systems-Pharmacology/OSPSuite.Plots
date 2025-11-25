# Package index

## Plot Functions

Functions for creating plots

- [`plotBoxWhisker()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotBoxWhisker.md)
  : Generate Box-Whisker Plots
- [`plotForest()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotForest.md)
  : Create a Forest Plot
- [`plotHistogram()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotHistogram.md)
  : Generates Histograms
- [`plotPredVsObs()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotPredVsObs.md)
  : Generate Predicted vs Observed Plots
- [`plotQQ()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotQQ.md)
  : generates residual quantile quantile plot
- [`plotRangeDistribution()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotRangeDistribution.md)
  : Plot Range Plot
- [`plotRatioVsCov()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotRatioVsCov.md)
  : Generate Plots of Ratios vs Covariate
- [`plotResVsCov()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotResVsCov.md)
  : Generate Residual Plots vs Covariate
- [`plotTimeProfile()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotTimeProfile.md)
  : generate time profile plots
- [`plotYVsX()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotYVsX.md)
  : Base Plot for Residuals and Predictions vs Covariates

## Update/initialize plots

initialize plot or add to existing plots

- [`initializePlot()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/initializePlot.md)
  : Initialize Plot
- [`addXScale()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/addXscale.md)
  : add X-scale
- [`addYScale()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/addYscale.md)
  : add y-scale
- [`addLLOQLayer()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/addLLOQLayer.md)
  : Add LLOQ Layer with LLOQ Lines
- [`addXYScale()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/addXYScale.md)
  : Add X and Y Scale

## Watermark

Functions to set watermark

- [`addWatermark()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/addWatermark.md)
  : Add a watermark to a ggplot object
- [`ggplotWithWatermark()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/ggplotWithWatermark.md)
  : Create a ggplot with an optional watermark
- [`plot(`*`<ggWatermark>`*`)`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plot.ggWatermark.md)
  : Create plot function for ggWatermark.
- [`print(`*`<ggWatermark>`*`)`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/print.ggWatermark.md)
  : Print method for ggWatermark objects

## Plot export

Functions to export plot

- [`exportPlot()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/exportPlot.md)
  : Export a ggplot object to a file

## Default settings

Functions to set and reset Settings for plot layout

- [`Shapes`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/Shapes.md)
  : Shapes

- [`colorMaps`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/colorMaps.md)
  : Color maps

- [`geomPointUnicode()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/geomPointUnicode.md)
  : layer to point unicode as shapes

- [`getDefaultGeomAttributes()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/getDefaultGeomAttributes.md)
  :

  get the defaults for the geom attributes used as defaults in plot
  functions see `vignette("ospsuite.plots", package = "ospsuite.plots")`
  how to change defaults

- [`getDefaultOptions()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/getDefaultOptions.md)
  : get list of default options

- [`getOspsuite.plots.option()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/getOspsuite.plots.option.md)
  : returns an option value for a option defined by the package
  OSPSuite.plots

- [`resetDefaultColorMapDistinct()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/resetDefaultColorMapDistinct.md)
  : reset the default color map for discrete colors

- [`resetDefaultShapeDiscrete()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/resetDefaultShapeDiscrete.md)
  : resets the scale for discrete shapes to ggplot default

- [`resetDefaultTheme()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/resetDefaultTheme.md)
  : reset the default theme

- [`resetDefaults()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/resetDefaults.md)
  : restore to previously stored settings

- [`setDefaultColorMapDistinct()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/setDefaultColorMapDistinct.md)
  : set the default color-map for discrete colors

- [`setDefaultShapeDiscrete()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/setDefaultShapeDiscrete.md)
  : set the default shapes

- [`setDefaultTheme()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/setDefaultTheme.md)
  : set the default theme

- [`setDefaults()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/setDefaults.md)
  : sets the defaults for the OSPSuite.plots package

- [`setOspsuite.plots.option()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/setOspsuite.plots.option.md)
  : Set OSPSuite plots option with a given key and value.

## DataMapping Classes

Classes combining data and Mapping

- [`MappedData`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/MappedData.md)
  : MappedData
- [`MappedDataBoxplot`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/MappedDataBoxplot.md)
  : MappedDataBoxplot
- [`MappedDataRangeDistribution`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/MappedDataRangeDistribution.md)
  : object to map data for rangeplots
- [`MappedDataTimeProfile`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/MappedDataTimeProfile.md)
  : MappedDataTimeProfile

## Constants

Enumeration constants for OSPSuite.plots options and scaling.

- [`AxisScales`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/AxisScales.md)
  : enumeration keys for OSPSuite.plots scaling options for axis
  scalings
- [`OptionKeys`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/OptionKeys.md)
  : enumeration keys for OSPSuite.plots options
- [`ResidualScales`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/ResidualScales.md)
  : enumeration keys for OSPSuite.plots scaling options for residual
  calculations
- [`BINNINGMODE`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/BINNINGMODE.md)
  : enumeration keys for mode of Binning

## Data

Data used within the vignette examples

- [`exampleDataCovariates`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/exampleDataCovariates.md)
  : Example Covariates Data
- [`exampleDataTimeProfile`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/exampleDataTimeProfile.md)
  : Example Time Profile Data

## auxiliary functions

Classes combining data and Mapping

- [`metaData2DataFrame()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/metaData2DataFrame.md)
  : converts metaData List to a data frame row names specify properties
- [`CombinedPlot`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/CombinedPlot.md)
  : CombinedPlot
- [`constructLabelWithUnit()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/constructLabelWithUnit.md)
  : Construct a Label with Unit
- [`getFoldDistanceList()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/getFoldDistanceList.md)
  : creates a list with fold Distances
- [`updateScaleArgumentsForTimeUnit()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/updateScaleArgumentsForTimeUnit.md)
  : adjust arguments for scale if dimension of scale is time
