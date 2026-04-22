# ospsuite.plots (development version)

## Breaking Changes

- Replaced showtext/Unicode-based shape rendering with native grid-based `geom_point_osp()`. The `Shapes` list now contains 22 geometric shapes instead of 44 shapes including emojis. Removed shapes: `male`, `female`, `man`, `woman`, `baby`, `mouse`, `cat`, `rat`, `rabbit`, `dog`, `pig`, `sheep`, `cow`, `monkey`, `human`, `pill`, `syringe`, `hazard`, and emoji variants. Code using these removed shapes will need to be updated to use available geometric shapes from `ospShapeNames`.

## New Features
  
- Added `geom_point_osp()` for custom scientific plot shapes without font dependencies. The geom automatically applies `scale_shape_osp()` when added to a plot.
- Added shape scale functions following ggplot2 conventions:
  - `scale_shape_osp()`: auto-assigns shapes from `ospShapeNames` in order
  - `scale_shape_osp_manual(values = ...)`: explicit mapping of factor levels to shapes
  - `scale_shape_osp_identity()`: for data already containing shape names
- When more groups exist than available shapes (21, excluding "blank"), shapes are recycled with a warning.
- Added `ospShapeNames` vector listing all available shape names.

## Minor improvements and bug fixes

- Changed default out-of-bounds handling for continuous axes. Ribbons and error bars that extend beyond axis limits are now clipped at the panel boundary rather than being silently dropped. 
- The default width of error bars is now set to `width = 0.2`, showing horizontal whiskers. The previous default was `width = 0`, which suppressed whiskers (#107).
- Removed `showtext` and `sysfonts` from package dependencies. 

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
