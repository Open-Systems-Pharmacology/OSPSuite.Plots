#' @title enumeration keys for ospsuite.plots options
#' @export
OptionKeys <- enum(c(   # nolint
  gsub("ospsuite.plots.", "", names(getDefaultOptions())),
  "shapeValues"
))
