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
  }

  # Check if watermark option is set and warn if not
  if (is.null(getOption("ospsuite.plots.watermark_enabled"))) {
    packageStartupMessage(
      "The option 'ospsuite.plots.watermark_enabled' is not set.\n",
      "To enable watermarks, add the following to your .Rprofile:\n",
      "  options(ospsuite.plots.watermark_enabled = TRUE)\n",
      "To disable watermarks, add:\n",
      "  options(ospsuite.plots.watermark_enabled = FALSE)\n",
      "You can edit your .Rprofile with usethis::edit_r_profile()"
    )
  }

  return(invisible())
}
