# Default Theme -------------

#' @title OSPSuite plot theme
#' @description A `ggplot2` theme with OSPSuite-specific styling, based on
#'   [ggplot2::theme_bw()]. Apply it to a single plot with `plot + themeOspsuite()`
#'   without altering the global `ggplot2` state.
#'
#' @param base_size base font size, given in pts (passed on to [ggplot2::theme_bw()]).
#' @param ... further arguments passed on to [ggplot2::theme_bw()]
#'   (for example `base_family`).
#'
#' @return a complete `ggplot2` theme object.
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   themeOspsuite()
#'
#' @export
#' @family setDefault functions
themeOspsuite <- function(base_size = 11, ...) {
  # Following ggplot2's guidance for theme functions: build on a complete
  # theme with `%+replace%` and fully specify every element that is touched,
  # since `%+replace%` discards unspecified element properties (unlike `+`).
  halfLine <- base_size / 2

  theme_bw(base_size = base_size, ...) %+replace%
    theme(
      legend.position = "right",
      legend.direction = "vertical",
      legend.justification = 0.5,
      plot.title = element_text(
        size = rel(1.2),
        hjust = 0.5,
        vjust = 0.5,
        margin = margin(b = halfLine)
      ),
      plot.subtitle = element_text(
        hjust = 0.5,
        vjust = 0.5,
        margin = margin(b = halfLine)
      ),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(color = NA, fill = NA),
      complete = TRUE
    )
}


#' @title set the default theme
#' @description set the OSPSuite theme as the global `ggplot2` theme for the
#'   whole session via [ggplot2::theme_set()]. This is an opt-in convenience;
#'   `ospsuite.plots` plot functions already apply [themeOspsuite()] per plot,
#'   so calling this is only needed to style unrelated plots in the session.
#'
#' @return invisibly return the previous theme so you can easily save it, then later restore it.
#' @examples
#' \dontrun{
#' oldTheme <- setDefaultTheme()
#'
#' # Create a plot with the new theme
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point()
#' print(p)
#'
#' # Restore previous theme
#' resetDefaultTheme(oldTheme)
#' }
#'
#' @export
#' @family setDefault functions
setDefaultTheme <- function() {
  return(invisible(ggplot2::theme_set(themeOspsuite())))
}


#' reset the default theme
#'
#' wrapper for `ggplot2::theme_set(oldTheme)`
#'
#' @param oldTheme theme to set
#'
#' @export
#'
#' @family setDefault functions
resetDefaultTheme <- function(oldTheme) {
  ggplot2::theme_set(oldTheme)
}


# Default ColorMap Discrete -------------

#' @title Color maps
#'
#' @description
#'
#' List with some color maps for `Theme` object.
#'
#' The `ospDefault` color map is based on colors in `default_igv` qualitative
#' color palette from `{ggsci}` package.
#'
#' @family setDefault functions
#' @export
colorMaps <- list(
  # nolint: object_name_linter
  default = c("#0078D7", "#D83B01", "#107C10", "#A80000", "#002050", "#B4009E"),
  grays = paste("gray", seq(0, 100, 10), sep = ""),
  prism = c(
    "#FF0000",
    "#FF7F00",
    "#FFFF00",
    "#00FF00",
    "#0000FF",
    "#4B0082",
    "#8F00FF"
  ),
  blot = c("blue", "magenta", "cyan", "green", "yellow", "red"),
  temperature = c(
    "#262A76",
    "#234990",
    "#2F8AC3",
    "#26B0D2",
    "#FFC1CB",
    "#EB559",
    "#AE3535",
    "8E1F20"
  ),
  ospDefault = c(
    "#5050FFFF",
    "#CE3D32FF",
    "#749B58FF",
    "#F0E685FF",
    "#466983FF",
    "#BA6338FF",
    "#5DB1DDFF",
    "#802268FF",
    "#6BD76BFF",
    "#D595A7FF",
    "#924822FF",
    "#837B8DFF",
    "#C75127FF",
    "#D58F5CFF",
    "#7A65A5FF",
    "#E4AF69FF",
    "#3B1B53FF",
    "#CDDEB7FF",
    "#612A79FF",
    "#AE1F63FF",
    "#E7C76FFF",
    "#5A655EFF",
    "#CC9900FF",
    "#99CC00FF",
    "#A9A9A9FF",
    "#CC9900FF",
    "#99CC00FF",
    "#33CC00FF",
    "#00CC33FF",
    "#00CC99FF",
    "#0099CCFF",
    "#0A47FFFF",
    "#4775FFFF",
    "#FFC20AFF",
    "#FFD147FF",
    "#990033FF",
    "#991A00FF",
    "#996600FF",
    "#809900FF",
    "#339900FF",
    "#00991AFF",
    "#009966FF",
    "#008099FF",
    "#003399FF",
    "#1A0099FF",
    "#660099FF",
    "#990080FF",
    "#D60047FF",
    "#FF1463FF",
    "#00D68FFF",
    "#14FFB1FF"
  )
)


#' @param colorMapList list of color-maps to be set
#'
#' @title set the default color-map for discrete colors
#' @description Sets default color mappings for discrete color and fill aesthetics in ggplot2.
#'   Each color map should be a vector of valid color values (hex codes, color names, etc.).
#'
#' @return list with color-maps previously set
#' @examples
#' \dontrun{
#' # Set custom color maps
#' customColors <- list(
#'   c("#FF0000", "#00FF00", "#0000FF"), # RGB colors
#'   c("red", "green", "blue") # Named colors
#' )
#' oldColors <- setDefaultColorMapDistinct(customColors)
#'
#' # Use default OSP color maps
#' setDefaultColorMapDistinct()
#'
#' # Reset to previous colors
#' resetDefaultColorMapDistinct(oldColors)
#' }
#'
#' @family setDefault functions
#' @export
#'
setDefaultColorMapDistinct <- function(colorMapList = NULL) {
  # Convert character vector to a list if it's not NULL
  if (!is.null(colorMapList) && is.character(colorMapList)) {
    colorMapList <- list(colorMapList)
  }
  # Validate colorMapList structure
  validateColors <- function(colorVector, varName) {
    for (color in colorVector) {
      if (any(is.na(grDevices::col2rgb(color, alpha = FALSE)))) {
        stop(messages$errorInvalidColor(color, varName))
      }
    }
  }
  if (!is.null(colorMapList)) {
    checkmate::assertList(colorMapList, min.len = 1)
    # Validate that each element is a character vector of colors
    for (i in seq_along(colorMapList)) {
      checkmate::assertCharacter(
        colorMapList[[i]],
        min.len = 1,
        .var.name = paste0("colorMapList[[", i, "]]")
      )
      # Validate colors using the helper function
      validateColors(colorMapList[[i]], paste0("colorMapList[[", i, "]]"))
    }
  }

  if (is.null(colorMapList)) {
    colorMapList <- list(
      colorMaps[["default"]],
      colorMaps[["ospDefault"]]
    )
  }

  optionNames <- c(
    "ggplot2.discrete.colour",
    "ggplot2.discrete.fill"
  )

  oldColorOptions <- list()
  newColorOptions <- list()
  for (optionName in optionNames) {
    oldColorOptions[[optionName]] <-
      getOption(optionName)
    newColorOptions[[optionName]] <-
      colorMapList
  }

  options(newColorOptions)

  return(invisible(oldColorOptions))
}


#' @title reset the default color map for discrete colors
#'
#' @param oldColorMaps list of color maps previously set
#'
#' @export
#' @family setDefault functions
#'
resetDefaultColorMapDistinct <- function(oldColorMaps) {
  checkmate::assertList(oldColorMaps, names = "named")

  options(ggplot2.discrete.colour = oldColorMaps[["ggplot2.discrete.colour"]])
  options(ggplot2.discrete.fill = oldColorMaps[["ggplot2.discrete.fill"]])
  options(ggplot2.ordinal.colour = oldColorMaps[["ggplot2.ordinal.colour"]])
  options(ggplot2.ordinal.fill = oldColorMaps[["ggplot2.ordinal.fill"]])

  return(invisible(NULL))
}


# Options ----------------

#' get list of default options
#'
#' @family setDefault functions
#' @return names list with default options
#' @export
#'
getDefaultOptions <- function() {
  optionList <- list(
    # watermark
    ospsuite.plots.watermarkEnabled = TRUE,
    ospsuite.plots.watermarkLabel = "preliminary analysis",
    ospsuite.plots.watermarkFormat = list(
      x = 0.5,
      y = 0.5,
      color = "lightgrey",
      angle = 30,
      fontsize = 12,
      alpha = 0.7
    ),
    # geom attributes
    ospsuite.plots.geomLineAttributes = list(),
    ospsuite.plots.geomRibbonAttributes = list(color = NA),
    ospsuite.plots.geomPointAttributes = list(),
    ospsuite.plots.geomErrorbarAttributes = list(width = 2),
    ospsuite.plots.geomLLOQAttributes = list(),
    ospsuite.plots.geomComparisonLineAttributes = list(linetype = "dashed"),
    ospsuite.plots.geomGuestLineAttributes = list(linetype = "dashed"),
    ospsuite.plots.geomBoxplotAttributes = list(
      position = position_dodge(width = 1),
      color = "black"
    ),
    ospsuite.plots.geomHistAttributes = list(
      bins = 10,
      position = ggplot2::position_nudge()
    ),
    # default alpha
    ospsuite.plots.alpha = 0.5,
    # alpha of LLOQ values
    ospsuite.plots.lloqAlphaVector = c("TRUE" = 0.3, "FALSE" = 1),
    # linetype of LLOQ
    ospsuite.plots.lloqLineType = "dashed",
    # percentiles
    ospsuite.plots.percentiles = c(0.05, 0.25, 0.5, 0.75, 0.95),
    # default percentiles (subset of percentiles used as default in plot functions)
    ospsuite.plots.defaultPercentiles = c(0.05, 0.5, 0.95),
    # used for plot export
    ospsuite.plots.exportWidth = 16,
    ospsuite.plots.exportUnits = "cm",
    ospsuite.plots.exportDevice = "png",
    ospsuite.plots.exportDpi = 300
  )

  return(optionList)
}

# Geoms ----------

#' get the defaults for the geom attributes used as defaults in plot functions
#' see \code{vignette("ospsuite.plots", package = "ospsuite.plots")} how to change defaults
#'
#' @param geom  type of geom to return attributes
#'
#' @family setDefault functions
#' @return list with default attributes
#' @export
#'
getDefaultGeomAttributes <- function(geom) {
  choices <- names(getDefaultOptions())
  choices <- gsub(
    "Attributes",
    "",
    gsub(
      "ospsuite.plots.geom",
      "",
      choices[grep("ospsuite.plots.geom", choices)]
    )
  )
  checkmate::assertChoice(geom, choices = choices)

  optionName <- paste0("ospsuite.plots.geom", geom, "Attributes")

  defaults <- getOption(optionName, default = getDefaultOptions()[[optionName]])

  return(defaults)
}

#' returns an option value for a option defined by the package OSPSuite.plots
#'
#'
#' @param optionKey identifier of option
#'
#' @return option value
#' @export
#' @family setDefault functions
#'
#' @examples
#' \dontrun{
#' getOspsuite.plots.option(optionKey = OptionKeys$watermarkEnabled)
#' }
# fmt: skip
getOspsuite.plots.option <- function(optionKey) { # nolint: object_name_linter
  checkmate::assert_choice(optionKey, choices = names(OptionKeys))

  return(getOption(
    paste0("ospsuite.plots.", optionKey),
    getDefaultOptions()[[paste0("ospsuite.plots.", optionKey)]]
  ))
}


#' Set OSPSuite plots option with a given key and value.
#'
#' @param optionKey The key for the option.
#' @param value The value for the option.
#'
#' @return NULL
#' @export
#' @family setDefault functions
#'
#' @examples
#' \dontrun{
#' setOspsuite.plots.option(optionKey = OptionKeys$watermarkEnabled, value = TRUE)
#' }
# fmt: skip
setOspsuite.plots.option <- function(optionKey, value) { # nolint: object_name_linter
  checkmate::assert_choice(optionKey, choices = names(OptionKeys))

  if (is.null(value)) {
    eval(parse(
      text = paste0(
        "options(ospsuite.plots.",
        optionKey,
        " = NULL)"
      )
    ))
  } else {
    newOption <- list()
    newOption[[paste0("ospsuite.plots.", optionKey)]] <- value
    options(newOption)
  }

  return(invisible(NULL))
}

# Wrapper for all defaults  ----------------------

#' sets the defaults for the OSPSuite.plots package
#'
#' should be started at the beginning at each workflow
#'
#' for detailed information see
#' \code{vignette("ospsuite.plots", package = "ospsuite.plots")}
#'
#'
#' @param colorMapList list of color maps
#' @param defaultOptions list of options
#'
#' @return list of old settings which can be used to reset defaults with `resetDefaults()`
#'
#' @family setDefault functions
#' @export
setDefaults <- function(
  defaultOptions = list(),
  colorMapList = NULL
) {
  checkmate::assertList(colorMapList, null.ok = TRUE)
  checkmate::assertList(defaultOptions, null.ok = TRUE)

  # initialize return value
  oldDefaults <- list()

  # append default options with user input
  defaultOptions <- utils::modifyList(
    getDefaultOptions(),
    defaultOptions
  )

  # options
  oldDefaults$options <- lapply(names(getDefaultOptions()), getOption)
  names(oldDefaults$options) <- names(getDefaultOptions())

  options(defaultOptions)

  defaultAlpha <- getOption(
    "ospsuite.plots.alpha",
    getDefaultOptions()[["ospsuite.plots.alpha"]]
  )

  # get old settings of defaults for geoms
  nsenv <- asNamespace("ggplot2")

  oldDefaults[["geomPoint"]] <- get("GeomPoint", envir = nsenv)$default_aes
  oldDefaults[["geomBoxplot"]] <- get("GeomBoxplot", envir = nsenv)$default_aes
  oldDefaults[["geomBar"]] <- get("GeomBar", envir = nsenv)$default_aes
  oldDefaults[["geomRibbon"]] <- get("GeomRibbon", envir = nsenv)$default_aes
  oldDefaults[["geomLine"]] <- get("GeomLine", envir = nsenv)$default_aes

  # set theme, color and shapes

  oldDefaults[["theme"]] <- setDefaultTheme()
  oldDefaults[["colorMaps"]] <- setDefaultColorMapDistinct(
    colorMapList = colorMapList
  )

  # Set geom_point to use shape 1 (open circle) for backwards compatibility
  # when raw geom_point() is used. Our custom shape names from ospShapeNames
  # only work with geom_point_osp.
  update_geom_defaults(
    "point",
    list(shape = 1)
  )

  # set geoms
  update_geom_defaults(
    "boxplot",
    list(
      fill = getOption("ggplot2.discrete.fill")[[1]][[1]],
      alpha = defaultAlpha
    )
  )
  update_geom_defaults(
    "bar",
    list(
      fill = getOption("ggplot2.discrete.fill")[[1]][[1]],
      alpha = defaultAlpha,
      color = "black"
    )
  )
  update_geom_defaults(
    "ribbon",
    list(
      fill = getOption("ggplot2.discrete.fill")[[1]][[1]],
      alpha = defaultAlpha
    )
  )
  update_geom_defaults(
    "line",
    list(
      linewidth = 1.25,
      fill = getOption("ggplot2.discrete.fill")[[1]][[1]]
    )
  )

  return(invisible(oldDefaults))
}


#' restore to previously stored settings
#'
#' @param oldDefaults `list`with previously stored settings
#'
#' @family setDefault functions
#' @export
#'
resetDefaults <- function(oldDefaults) {
  ggplot2::theme_set(oldDefaults$theme)

  options(oldDefaults$options)

  if (!is.null(oldDefaults$geomBar)) {
    update_geom_defaults("bar", oldDefaults$geomBar)
  }
  if (!is.null(oldDefaults$geomBoxplot)) {
    update_geom_defaults("boxplot", oldDefaults$geomBoxplot)
  }
  if (!is.null(oldDefaults$geomPoint)) {
    update_geom_defaults("point", oldDefaults$geomPoint)
  }
  if (!is.null(oldDefaults$geomRibbon)) {
    update_geom_defaults("ribbon", oldDefaults$geomRibbon)
  }
  if (!is.null(oldDefaults$geomLine)) {
    update_geom_defaults("line", oldDefaults$geomLine)
  }

  resetDefaultColorMapDistinct(oldColorMaps = oldDefaults$colorMaps)
}
