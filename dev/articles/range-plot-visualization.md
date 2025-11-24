# Range Plot Visualization

## 1. Introduction

This vignette documents and illustrates workflows for creating range
plots using the function `plotRangeDistribution` from the
`ospsuite.plots` package. Range plots are useful for visualizing data
distributions over specified ranges, allowing for different binning
strategies and statistical summaries.

### 1.1 Setup

This vignette uses the
[ospsuite.plots](https://www.open-systems-pharmacology.org/OSPSuite.Plots/)
and [tidyr](https://tidyr.tidyverse.org) libraries. We will also utilize
the `ggplot2` package for plotting.

``` r
library(ospsuite.plots)
#> Loading required package: ggplot2
library(tidyr)
library(data.table)
library(ggplot2)

# Set Defaults
oldDefaults <- setDefaults()
```

### 1.2 Example Data

This vignette uses a simulated dataset to demonstrate the functionality
of the `plotRangeDistribution` function. The dataset includes individual
identifiers, the age of the individual, a numeric variable representing
measurements, and a categorical variable indicating group membership.

``` r
# Simulating example data
set.seed(123)
n <- 1000
exampleData <- data.table(
  IndividualId = 1:n,
  Age = runif(n = n, min = 2, max = 18),
  Group = sample(c("A", "B"), n, replace = TRUE)
)
exampleData[, value := rnorm(n) + ifelse(Group == "A", Age, 10)]

metaData <- list(Age = list(
  dimension = "Age",
  unit = "year(s)"
))

# Display the first few rows of the example data
head(exampleData)
```

IndividualId Age Group value 1: 1 6.601240 A 5.605442 2: 2 14.612882 B
8.960045 3: 3 8.543631 B 9.982020 4: 4 16.128278 A 15.996103 5: 5
17.047477 B 7.450657 6: 6 2.728904 A 3.769477

### 2. Binning Methods

The `plotRangeDistribution` function supports the following binning
methods:

- **Equal Frequency Binning**: Divides the data into bins that contain
  approximately the same number of observations.
- **Equal Width Binning**: Divides the data into bins of equal width.
- **Custom Binning**: Allows the user to specify custom breaks for
  binning.

### 3. Continuous vs. Step Function Plot Types

The `plotRangeDistribution` function allows for two types of plots:
**continuous** and **step function**.

- **Continuous Plot**: This type of plot displays a smooth line
  connecting the statistical summaries. It is useful for visualizing
  trends in the data over the specified range and provides a clear
  representation of the overall distribution.

- **Step Function Plot**: This type of plot presents the data as steps
  between points rather than a continuous line. This is particularly
  useful when the data has discrete changes and allows for a clearer
  representation of the underlying data points. It emphasizes the
  differences between adjacent values and can help highlight specific
  changes in the data distribution.

### 4. Generating Range Plots

#### 4.1 Basic Range Plot

In this example, we will create a basic range plot to visualize the
distribution of the `Value` variable across different groups.

``` r
plotObject <- plotRangeDistribution(
  data = exampleData,
  mapping = aes(x = Age, y = value, groupby = Group),
  modeOfBinning = BINNINGMODE$number,
  numberOfBins = 20,
  statFun = NULL,
  percentiles = c(0.05, 0.5, 0.95),
  metaData = metaData
)

print(plotObject)
```

![A range plot showing the distribution of values across groups A and
B.](range-plot-visualization_files/figure-html/basic-range-plot-1.png)

##### Explanation:

- `data`: The example dataset prepared in the previous section.
- `mapping`: Specifies the aesthetics for the plot, including the x and
  y axes.
- `modeOfBinning`: Specifies the mode of binning for the data.
- `statFun`: The statistical function used to summarize the data.

#### 4.2 Range Plot with Custom Binning

In this example, we will create a range plot using custom binning
breaks.

``` r
customBreaks <- c(2, 6, 12, 18)

plotObject <- plotRangeDistribution(
  data = exampleData,
  mapping = aes(x = Age, y = value, groupby = Group),
  modeOfBinning = BINNINGMODE$breaks,
  breaks = customBreaks,
  statFun = NULL,
  percentiles = c(0.05, 0.5, 0.95),
  metaData = metaData
)

print(plotObject)
```

![A range plot showing the distribution of values with custom
binning.](range-plot-visualization_files/figure-html/custom-binning-range-plot-1.png)

##### Explanation:

- `breaks`: Specifies custom breaks for binning, allowing for precise
  control over how data is grouped.

#### 4.3 Example of Custom Aggregation Function

In this example, we will define a custom aggregation function that
calculates the mean and standard deviation for the `Value` variable and
use it in the range plot.

``` r
customStatFun <- function(y) {
  return(c(ymin = mean(y) - sd(y), y = mean(y), ymax = mean(y) + sd(y)))
}

plotObject <- plotRangeDistribution(
  data = exampleData,
  metaData = metaData,
  mapping = aes(x = Age, y = value, groupby = Group),
  modeOfBinning = BINNINGMODE$number,
  numberOfBins = 20,
  statFun = customStatFun,
  percentiles = c(0.05, 0.5, 0.95)
)

print(plotObject)
```

![A range plot showing the distribution of values using a custom
aggregation
function.](range-plot-visualization_files/figure-html/custom-aggregation-function-1.png)

##### Explanation:

- `custom_stat_fun`: A user-defined function that calculates the mean
  and standard deviation for the y-values.

#### 4.4 Range Plot with Step Plot Option

In this example, we will create a range plot with the step plot option
enabled.

``` r
plotObject <- plotRangeDistribution(
  data = exampleData,
  mapping = aes(x = Age, y = value, groupby = Group),
  metaData = metaData,
  modeOfBinning = BINNINGMODE$number,
  numberOfBins = 20,
  asStepPlot = TRUE,
  statFun = NULL,
  percentiles = c(0.05, 0.5, 0.95)
)

print(plotObject)
```

![A step range plot showing the distribution of values across groups A
and
B.](range-plot-visualization_files/figure-html/step-plot-range-plot-1.png)

##### Explanation:

- `asStepPlot`: By setting this to `TRUE`, the plot will display steps
  between data points rather than continuous lines.
