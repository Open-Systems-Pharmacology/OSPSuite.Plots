# Print method for ggWatermark objects

This function customizes the printing of ggplot objects with the class
"ggWatermark" by adding a watermark.

## Usage

``` r
# S3 method for class 'ggWatermark'
print(x, ...)
```

## Arguments

- x:

  A ggWatermark object created by
  [`ggplotWithWatermark()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/ggplotWithWatermark.md).

- ...:

  Additional arguments to be passed to the print method, allowing for
  further customization of the output.

## Value

A ggplot object with a watermark drawn on it. The watermark is displayed
according to the specified options.

## See also

Other watermark:
[`addWatermark()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/addWatermark.md),
[`ggplotWithWatermark()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/ggplotWithWatermark.md),
[`plot.ggWatermark()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plot.ggWatermark.md)
