# Export a ggplot object to a file

This function exports a ggplot object to a specified file with
customizable options.

## Usage

``` r
exportPlot(
  plotObject,
  filepath,
  filename,
  width = NULL,
  height = NULL,
  device = NULL,
  ...
)
```

## Arguments

- plotObject:

  A ggplot object to be exported.

- filepath:

  A character string specifying the directory to save the plot.

- filename:

  A character string specifying the name of the file (without path).

- width:

  A numeric value specifying the width of the plot. If NULL, the default
  option is used.

- height:

  A numeric value specifying the height of the plot. If NULL, it is
  calculated based on the plot dimensions.

- device:

  Export device, if NULL (default) the device set by
  ospsuite.plots.export.device is used.

- ...:

  Additional arguments passed to `ggsave`.

## Value

NULL, the function saves the plot to the specified file.

## Details

The height of the plot is calculated if it is not provided by the user.
The calculation takes into account:

- The aspect ratio of the plot, which is derived from the theme
  settings.

- The number of rows and columns in the plot layout.

- The dimensions of plot components such as axes, legends, and margins.
  The function ensures that the height is adjusted to maintain the
  correct aspect ratio based on the specified width.

Options available for plot export with default values:

- `ospsuite.plots.export.width`: Width of the exported plot (default =
  16).

- `ospsuite.plots.export.units`: Units of the exported plot (default =
  "cm").

- `ospsuite.plots.export.device`: File format of the exported plot
  (default = "png").

- `ospsuite.plots.export.dpi`: Resolution of the exported plot (default
  = 300).

For more details and examples see the vignettes:

- `vignette("ospsuite.plots", package = "ospsuite.plots")`

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic usage
p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()
exportPlot(
  plotObject = p,
  filepath = tempdir(),
  filename = "my_plot.png"
)

# Export with custom dimensions and device
exportPlot(
  plotObject = p,
  filepath = "./output",
  filename = "scatter_plot",
  width = 12,
  height = 8,
  device = "pdf"
)

# Export with special characters in filename (will be cleaned)
exportPlot(
  plotObject = p,
  filepath = tempdir(),
  filename = "concentration in µg/L: results"
)
} # }
```
