.onLoad <- function(libname, pkgname) {
  # Set default options only if not already defined (e.g. in .Rprofile)
  if (is.null(getOption("ospsuite.plots.watermarkEnabled"))) {
    options(ospsuite.plots.watermarkEnabled = TRUE)
  }

  return(invisible())
}
