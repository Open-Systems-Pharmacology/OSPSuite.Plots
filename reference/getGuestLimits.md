# Calculate Limits for DDI Ratio according to Guest et al.

This function calculates the limits according to Guest et al. for the
DDI ratio based on the provided parameters.

## Usage

``` r
getGuestLimits(x, deltaGuest = 1, yDisplayAsAbsolute = FALSE, asLower = TRUE)
```

## Arguments

- x:

  A numeric vector representing the observed values.

- deltaGuest:

  Numeric value parameter for the Guest function.

- yDisplayAsAbsolute:

  A logical value if FALSE the limits are calculated for the ratio
  predicted/observed if TRUE limits are calculated for observed

- asLower:

  A logical value indicating whether to calculate lower limits (default
  is TRUE).

## Value

A numeric vector representing the calculated limits for the DDI ratio.

## References

<https://pubmed.ncbi.nlm.nih.gov/21036951>
