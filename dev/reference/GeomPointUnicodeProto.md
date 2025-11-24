# Geom to point unicode as shapes

Define a Geom using
[`ggplot2::ggproto()`](https://ggplot2.tidyverse.org/reference/ggproto.html)
and based on GeomPoint. The Geom internally uses `textGrob` instead of
`pointsGrob` so that fonts leverage for drawing shapes. Custom ggproto
for Unicode Point Shapes

A custom ggproto object that extends GeomPoint to use Unicode characters
as plot symbols instead of standard R plotting symbols. This allows for
more diverse and visually appealing point shapes in ggplot2 graphics.

## Usage

``` r
GeomPointUnicodeProto
```

## Format

An object of class `GeomPointUnicodeProto` (inherits from `GeomPoint`,
`Geom`, `ggproto`, `gg`) of length 4.

## Details

This geom uses Unicode characters to render points, providing access to
a wider variety of shapes than standard R plotting symbols. The shapes
are rendered as text using grid::textGrob, which allows for better
scaling and appearance.

Key features:

- Uses Unicode characters for point rendering

- Supports color, fill, size, and alpha aesthetics

- Default shape is a filled square (\u2588)

- Compatible with all standard ggplot2 aesthetics and scales

## Shape Validation

Shape codes are validated to ensure they are valid Unicode points.
Invalid codes will fall back to default shapes or generate warnings.

The `grid` and `scales` packages are supposed to be required by
`ggplot2`. So there should not be any issue as installing `ggplot2`
should install those 2 packages.
