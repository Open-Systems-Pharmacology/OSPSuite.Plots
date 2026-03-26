# initializes plot object and set scaling

initializes plot object and set scaling

## Usage

``` r
.initialplotObjectForTimeProfile(
  simMappedData,
  obsMappedData,
  plotObject,
  xScale,
  xScaleArgs,
  yScale,
  yScaleArgs,
  y2ScaleArgs,
  secAxis
)
```

## Arguments

- simMappedData:

  object of class `MappedDataTimeprofile` for simulated data

- obsMappedData:

  object of class `MappedDataTimeprofile` for observed data

- plotObject:

  An optional `ggplot` object on which to add the plot layers

- xScale:

  either 'linear' then
  [`ggplot2::scale_x_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
  or 'log' then
  [`ggplot2::scale_x_log10()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
  is used

- xScaleArgs:

  list of arguments passed to
  [`ggplot2::scale_x_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
  or
  [`ggplot2::scale_x_log10()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)

- yScale:

  either 'linear' then
  [`ggplot2::scale_y_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
  or 'log' then
  [`ggplot2::scale_y_log10()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
  is used

- yScaleArgs:

  list of arguments passed to
  [`ggplot2::scale_y_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
  or
  [`ggplot2::scale_y_log10()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)

- y2ScaleArgs:

  list of arguments passed to
  [`ggplot2::sec_axis()`](https://ggplot2.tidyverse.org/reference/sec_axis.html),
  trans, break are set by code

## Value

plot object
