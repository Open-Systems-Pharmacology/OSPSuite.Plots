# Changelog

## ospsuite.plots 1.2.0

### Breaking Changes

- Replaced showtext/Unicode-based shape rendering with native grid-based
  [`geom_point_osp()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/reference/geom_point_osp.md).
  The `Shapes` list now contains 22 geometric shapes instead of 44
  shapes including emojis. Removed shapes: `male`, `female`, `man`,
  `woman`, `baby`, `mouse`, `cat`, `rat`, `rabbit`, `dog`, `pig`,
  `sheep`, `cow`, `monkey`, `human`, `pill`, `syringe`, `hazard`, and
  emoji variants. Code using these removed shapes will need to be
  updated to use available geometric shapes from `ospShapeNames` (#10).
- Removed residual calculation from the package. The `ResidualScales`
  export is gone, `residualScale` is deprecated, and mapping both
  `observed` and `predicted` is no longer supported. Callers must
  pre-compute residuals before plotting (#74).

### New Features

- Added
  [`geom_point_osp()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/reference/geom_point_osp.md)
  for custom scientific plot shapes without font dependencies. The geom
  automatically applies
  [`scale_shape_osp()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/reference/scale_shape_osp.md)
  when added to a plot (#10).
- Added shape scale functions following ggplot2 conventions:
  - [`scale_shape_osp()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/reference/scale_shape_osp.md):
    auto-assigns shapes from `ospShapeNames` in order
  - `scale_shape_osp_manual(values = ...)`: explicit mapping of factor
    levels to shapes
  - [`scale_shape_osp_identity()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/reference/scale_shape_osp_identity.md):
    for data already containing shape names
- When more groups exist than available shapes (21, excluding “blank”),
  shapes are recycled with a warning.
- Added `ospShapeNames` vector listing all available shape names.

### Minor improvements and bug fixes

- Changed default out-of-bounds handling for continuous axes. Ribbons
  and error bars that extend beyond axis limits are now clipped at the
  panel boundary rather than being silently dropped (#107).
- Error bars now use the new
  [`geom_errorbar_osp()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/reference/geom_errorbar_osp.md),
  whose cap `width` is given in millimetres and stays visually
  consistent regardless of the data range or axis scale. The default cap
  width is `width = 2`.
  [`plotTimeProfile()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/reference/plotTimeProfile.md),
  [`plotYVsX()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/reference/plotYVsX.md)
  (and its wrappers), and
  [`plotForest()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/reference/plotForest.md)
  all draw their error bars with this geom (#123).
- Removed `showtext` and `sysfonts` from package dependencies (#10).
- `ospsuite.plots.watermarkEnabled` is now `TRUE` by default and set
  automatically at package load (#119). The startup message and runtime
  error from version 1.1.0 are gone. To disable, set
  `options(ospsuite.plots.watermarkEnabled = FALSE)` in your
  `.Rprofile`.
- Axis scale parameters now accept `"lin"` as a shorthand for `"linear"`
  (#111).

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
  [`plotRangeDistribution()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/reference/plotRangeDistribution.md)
  now uses this key instead of indexing into `OptionKeys$Percentiles`
  (#101).
- Fixed
  [`plotYVsX()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/reference/plotYVsX.md)
  LLOQ layer using wrong geom when `observedDataDirection = "y"`. The
  LLOQ line now correctly uses `geom_hline` for y-direction and
  `geom_vline` for x-direction (#78).
- Added `lloqOnBothAxes` parameter to
  [`plotYVsX()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/reference/plotYVsX.md)
  to optionally draw LLOQ lines on both axes (#78).
- Fixed duplicate legend in
  [`plotTimeProfile()`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/reference/plotTimeProfile.md)
  when mixing observed and simulated data. Shape and fill guides are now
  suppressed only when they were autoexpanded from `groupby`, preserving
  explicit user-defined mappings (#76).

## ospsuite.plots 1.0.1

- Initial Release of beta version
