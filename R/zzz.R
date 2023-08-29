.onLoad <- function(libname, pkgname) {
  # If available use show text to load font and display shapes
  if (requireNamespace("showtext", quietly = TRUE)) {
    # sysfonts is required by showtext
    # thus automatically installed when installing showtext
    sysfonts::font_add(
      family = "Symbola",
      regular = system.file("extdata", "Symbola.ttf", package = "ospsuite.plots")
    )
    sysfonts::font_add(
      family = "NotoSans",
      regular = system.file("extdata", "NotoSans-Regular.ttf", package = "ospsuite.plots")
    )

    # # Request usage of showtext with new font
    #  showtext::showtext_auto()
    #
    #  # cowplot needs device to use fonts
    #  cowplot::set_null_device("agg")
  }

  return(invisible())
}
