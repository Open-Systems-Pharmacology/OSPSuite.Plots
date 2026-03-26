# Construct a Label with Unit

This function constructs a label by appending a unit in square brackets
if both the label and unit are provided. If the unit is empty or NULL,
only the label is returned.

## Usage

``` r
constructLabelWithUnit(label, unit)
```

## Arguments

- label:

  A character string representing the label. It should not be NULL.

- unit:

  A character string representing the unit. It can be NULL or an empty
  string.

## Value

A character string that combines the label and the unit, formatted as
"label unit", or just the label if the unit is empty or NULL.

## Examples

``` r
constructLabelWithUnit("Temperature", "Celsius") # Returns "Temperature [Celsius]"
#> [1] "Temperature [Celsius]"
constructLabelWithUnit("Length", "") # Returns "Length"
#> [1] "Length"
constructLabelWithUnit(NULL, "kg") # Returns NULL
#> NULL
```
