# ospsuite.plots (development version)

## Breaking Changes

* The `ospsuite.plots.watermark_enabled` option no longer has a default value. Users must now explicitly set this option before using any plotting functions. This ensures conscious decision-making about watermark usage.
  
  To use the package, add one of the following to your `.Rprofile`:
  ```r
  # Enable watermarks
  options(ospsuite.plots.watermark_enabled = TRUE)
  
  # Or disable watermarks
  options(ospsuite.plots.watermark_enabled = FALSE)
  ```
  
  You can edit your `.Rprofile` with `usethis::edit_r_profile()`.

## Changes

* Added startup message when `ospsuite.plots.watermark_enabled` is not set, providing instructions on how to configure it
* `addWatermark()` and `print.ggWatermark()` now throw an error if the watermark option is not set
* Added comprehensive test coverage for watermark option validation
* Updated all vignettes and examples to explicitly set the watermark option
* Added `computeResiduals()` function to provide a consistent method for calculating residuals across the Open Systems Pharmacology ecosystem. This function uses the same calculation logic as the internal residual calculations in plotting functions like `plotResVsCov()` and `plotRatioVsCov()`. This aligns residual calculations between `ospsuite.plots` and `ospsuite` packages. The function name was chosen to avoid confusion with `ospsuite::calculateResiduals()`. (#1713)

# ospsuite.plots 1.0.1

  - Initial Release of beta version


