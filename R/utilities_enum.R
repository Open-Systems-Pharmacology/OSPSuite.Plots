#' @title enumeration keys for OSPSuite.plots options
#' @export
OptionKeys <- enum(c( # nolint
  gsub("ospsuite.plots.", "", names(getDefaultOptions())),
  "shapeValues"
))
