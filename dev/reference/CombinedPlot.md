# CombinedPlot

This class represents a combined plot object that includes a plot and an
optional table. It provides methods to get and set the plot and table
objects, as well as to print the combined output.

## Active bindings

- `plotObject`:

  A ggplot object representing the main plot.

- `tableObject`:

  A ggplot object representing the table.

- `relWidths`:

  A numeric vector of length 2 specifying the relative widths of the
  plot and table.

## Methods

### Public methods

- [`CombinedPlot$new()`](#method-CombinedPlot-initialize)

- [`CombinedPlot$combined()`](#method-CombinedPlot-combined)

- [`CombinedPlot$print()`](#method-CombinedPlot-print)

- [`CombinedPlot$clone()`](#method-CombinedPlot-clone)

------------------------------------------------------------------------

### `CombinedPlot$new()`

Create a new `CombinedPlot` object.

#### Usage

    CombinedPlot$new(plotObject = ggplot(), tableObject = NULL)

#### Arguments

- `plotObject`:

  A ggplot object for the main plot.

- `tableObject`:

  A ggplot object for the table.

#### Returns

A new `CombinedPlot` object.

------------------------------------------------------------------------

### `CombinedPlot$combined()`

Combine the combined plot and table. This method combines the plot and
table into a single output and displays it.

#### Usage

    CombinedPlot$combined()

#### Returns

A ggplot object representing the combined plot and table

------------------------------------------------------------------------

### `CombinedPlot$print()`

Print the combined plot and table. This method overrides the default
print function to display the combined output.

#### Usage

    CombinedPlot$print()

#### Returns

Invisibly returns the combined ggplot object

------------------------------------------------------------------------

### `CombinedPlot$clone()`

The objects of this class are cloneable with this method.

#### Usage

    CombinedPlot$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{

# Create a new CombinedPlot instance
combinedPlotInstance <- CombinedPlot$new(plotObject = myPlotObject, tableObject = myTableObject)

# Print the combined plot and table
print(combinedPlotInstance)
# or simply
combinedPlotInstance
} # }
```
