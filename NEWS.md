# ospsuite.plots (development version)

- The default width of error bars is now set to ggplot2's default (0.5), showing horizontal whiskers. The previous default was `width = 0`, which suppressed whiskers (#107).

# ospsuite.plots 1.1.0

## Breaking Changes

- Residuals are now calculated as `predicted - observed` (linear) and `log(predicted) - log(observed)` (log scale) to be consistent with PK-Sim (#71).
- `ospsuite.plots.watermarkEnabled` option must now be set explicitly before using any plotting functions (#68).
- All `OptionKeys` standardized to camelCase (#102).

## Minor improvements and bug fixes

- Added `defaultPercentiles` option key (`OptionKeys$defaultPercentiles`, default `c(0.05, 0.5, 0.95)`) as the canonical default for plot functions that use three percentiles. `plotRangeDistribution()` now uses this key instead of indexing into `OptionKeys$Percentiles` (#101).
- Fixed `plotYVsX()` LLOQ layer using wrong geom when `observedDataDirection = "y"`. The LLOQ line now correctly uses `geom_hline` for y-direction and `geom_vline` for x-direction (#78).
- Added `lloqOnBothAxes` parameter to `plotYVsX()` to optionally draw LLOQ lines on both axes (#78).
- Fixed duplicate legend in `plotTimeProfile()` when mixing observed and simulated data. Shape and fill guides are now suppressed only when they were autoexpanded from `groupby`, preserving explicit user-defined mappings (#76).

# ospsuite.plots 1.0.1

- Initial Release of beta version
