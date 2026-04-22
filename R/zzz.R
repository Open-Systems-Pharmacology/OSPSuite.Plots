.onLoad <- function(libname, pkgname) {
  # Check if watermark option is set and warn if not
  if (is.null(getOption("ospsuite.plots.watermarkEnabled"))) {
    packageStartupMessage(
      "The option 'ospsuite.plots.watermarkEnabled' is not set.\n",
      "To enable watermarks, add the following to your .Rprofile:\n",
      "  options(ospsuite.plots.watermarkEnabled = TRUE)\n",
      "To disable watermarks, add:\n",
      "  options(ospsuite.plots.watermarkEnabled = FALSE)\n",
      "You can edit your .Rprofile with usethis::edit_r_profile()"
    )
  }

  return(invisible())
}
