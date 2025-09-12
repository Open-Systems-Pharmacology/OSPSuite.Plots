#' @title enumeration keys for OSPSuite.plots options
#' @export
OptionKeys <- enum(c( # nolint
  gsub("ospsuite.plots.", "", names(getDefaultOptions())),
  "shapeValues"
))

#' @title enumeration keys for OSPSuite.plots scaling options for axis scalings
#' @export
AxisScales <- enum(c( # nolint
  linear = "linear",
  log = "log",
  discrete = "discrete"
))

#' @title enumeration keys for OSPSuite.plots scaling options for residual calculations
#' @export
ResidualScales <- enum(c( # nolint
  linear = "linear",
  log = "log",
  ratio = "ratio"
))
