# Changelog

## ospsuite.plots (development version)

### Breaking Changes

- Residuals are now calculated as `predicted - observed` (linear) and
  `log(predicted) - log(observed)` (log scale) to be consistent with
  PK-Sim (#71).
- `ospsuite.plots.watermark_enabled` option must now be set explicitly
  before using any plotting functions (#68).

### Minor improvements and bug fixes

- Fixed
  [`plotYVsX()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotYVsX.md)
  LLOQ layer using wrong geom when `observedDataDirection = "y"`. The
  LLOQ line now correctly uses `geom_hline` for y-direction and
  `geom_vline` for x-direction (#78).
- Added `lloqOnBothAxes` parameter to
  [`plotYVsX()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotYVsX.md)
  to optionally draw LLOQ lines on both axes (#78).
- Fixed duplicate legend in
  [`plotTimeProfile()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotTimeProfile.md)
  when mixing observed and simulated data. Shape and fill guides are now
  suppressed only when they were autoexpanded from `groupby`, preserving
  explicit user-defined mappings (#76).

## ospsuite.plots 1.0.1

- Initial Release of beta version
