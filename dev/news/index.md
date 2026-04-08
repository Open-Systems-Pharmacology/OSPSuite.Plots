# Changelog

## ospsuite.plots (development version)

- Changed default out-of-bounds handling for continuous axes. Ribbons
  and error bars that extend beyond axis limits are now clipped at the
  panel boundary rather than being silently dropped.
- The default width of error bars is now set to `width = 0.2`, showing
  horizontal whiskers. The previous default was `width = 0`, which
  suppressed whiskers (#107).

## ospsuite.plots 1.1.0

### Breaking Changes

- Residuals are now calculated as `predicted - observed` (linear) and
  `log(predicted) - log(observed)` (log scale) to be consistent with
  PK-Sim (#71).
- `ospsuite.plots.watermarkEnabled` option must now be set explicitly
  before using any plotting functions (#68).
- All `OptionKeys` standardized to camelCase (#102).

### Minor improvements and bug fixes

- Added `defaultPercentiles` option key
  (`OptionKeys$defaultPercentiles`, default `c(0.05, 0.5, 0.95)`) as the
  canonical default for plot functions that use three percentiles.
  [`plotRangeDistribution()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/dev/reference/plotRangeDistribution.md)
  now uses this key instead of indexing into `OptionKeys$Percentiles`
  (#101).
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
