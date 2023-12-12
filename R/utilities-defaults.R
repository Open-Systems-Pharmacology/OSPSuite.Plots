# Default Theme -------------

#' @title set the default theme
#' @description set properties of the default theme
#'
#' @return  invisibly return the previous theme so you can easily save it, then later restore it.
#'
#' @export
#' @family setDefault functions
setDefaultTheme <- function() {
  themeNew <-
    ggplot2::theme(
      # rect elemnts
      rect = element_rect(
        fill = "white",
        colour = "black",
        linewidth = 0.5
      ),
      panel.background = element_rect(linetype = "blank"),
      plot.background = element_rect(linetype = "blank"),
      legend.background = element_rect(
        colour = "white",
        linetype = "blank"
      ),
      legend.key = element_rect(
        colour = "white",
        linetype = "blank"
      ),

      # line elements
      line = element_line(colour = "grey20"),
      axis.line = element_line(
        linewidth = 0.5,
        linetype = "solid"
      ),
      panel.grid.major = element_line(
        linewidth = 0.5,
        linetype = "blank"
      ),
      panel.grid.minor = element_line(
        linewidth = 0.25,
        linetype = "blank"
      ),
      # text elements
      text = element_text(
        family = "",
        face = "plain",
        colour = "grey20",
        hjust = 0.5,
        vjust = 0.5
      ),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 10),
      plot.title = element_text(size = 12),
      plot.subtitle = element_text(size = 10),
      plot.caption = element_text(size = 8),
      legend.text = element_text(size = 10),
      strip.text = element_text(size = 10),
      # legend position
      legend.position = "right",
      legend.direction = "vertical",
      legend.justification = 0.5
    )
  oldTheme <- ggplot2::theme_set(themeNew)

  return(invisible(oldTheme))
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
ColorMaps <- list(
  default = c("#0078D7", "#D83B01", "#107C10", "#A80000", "#002050", "#B4009E"),
  grays = paste("gray", seq(0, 100, 10), sep = ""),
  prism = c("#FF0000", "#FF7F00", "#FFFF00", "#00FF00", "#0000FF", "#4B0082", "#8F00FF"),
  blot = c("blue", "magenta", "cyan", "green", "yellow", "red"),
  temperature = c("#262A76", "#234990", "#2F8AC3", "#26B0D2", "#FFC1CB", "#EB559", "#AE3535", "8E1F20"),
  ospDefault = c(
    "#5050FFFF", "#CE3D32FF", "#749B58FF", "#F0E685FF",
    "#466983FF", "#BA6338FF", "#5DB1DDFF", "#802268FF", "#6BD76BFF",
    "#D595A7FF", "#924822FF", "#837B8DFF", "#C75127FF", "#D58F5CFF",
    "#7A65A5FF", "#E4AF69FF", "#3B1B53FF", "#CDDEB7FF", "#612A79FF",
    "#AE1F63FF", "#E7C76FFF", "#5A655EFF", "#CC9900FF", "#99CC00FF",
    "#A9A9A9FF", "#CC9900FF", "#99CC00FF", "#33CC00FF", "#00CC33FF",
    "#00CC99FF", "#0099CCFF", "#0A47FFFF", "#4775FFFF", "#FFC20AFF",
    "#FFD147FF", "#990033FF", "#991A00FF", "#996600FF", "#809900FF",
    "#339900FF", "#00991AFF", "#009966FF", "#008099FF", "#003399FF",
    "#1A0099FF", "#660099FF", "#990080FF", "#D60047FF", "#FF1463FF",
    "#00D68FFF", "#14FFB1FF"
  )
)


#' @param ColorMapList list of ColorMaps to be set
#'
#' @title set the default colormap for discrete colors
#'
#' @return list with Colormaps previously set
#'
#' @family setDefault functions
#' @export
#'
setDefaultColorMapDistinct <- function(ColorMapList = NULL) {
  if (is.null(ColorMapList)) {
    ColorMapList <- list(
      ColorMaps[["default"]],
      ColorMaps[["ospDefault"]]
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
      ColorMapList
  }

  options(newColorOptions)



  return(invisible(oldColorOptions))
}


#' @param oldColorMaps
#'
#' @title reset the default colormap for discrete colors
#'
#' @param oldColorMaps list of colormaps previously set
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


# Default Shapes --------------

#' set the default shapes
#'
#' The scales are set to the option  `ospsuite.plots.shapeValues`, which is the used to
#' set the discrete scale of shapes for all {ospsuite.plots} function
#' for customized functions add scale_shape_manual(`values = getOption('ospsuite.plots.shapeValues')`)
#'
#' @param shapeValues vector of shape values
#'
#' @export
#' @return vector with `shapeValues` saved in option `ospsuite.plots.shapeValues` before function call
#'
#' @family setDefault functions
setDefaultShapeDiscrete <- function(shapeValues = NULL) {
  if (is.null(shapeValues)) {
    shapeValues <- unlist(Shapes)

    if (getOption("ospsuite.plots.GeomPointUnicode")) {
      shapeValues <- unlist(unname(Shapes))
    } else {
      shapeValues <-
        c(
          "circle filled", "diamond filled", "triangle filled", "square filled", "triangle down filled",
          "plus", "cross", "asterisk",
          "circle cross", "square cross",
          "circle plus", "square plus", "diamond plus",
          "square triangle"
        )
    }
  }
  options(list(ospsuite.plots.shapeValues = shapeValues))

  return(invisible(getOption("ospsuite.plots.shapeValues")))
}


#' resets the scale for discrete shapes to ggplot default
#'
#' @param oldShapeValues shape values to set
#'
#' @family setDefault functions
#' @export
resetDefaultShapeDiscrete <- function(oldShapeValues = NULL) {
  options(list(ospsuite.plots.shapeValues = oldShapeValues))
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
    ospsuite.plots.watermark_enabled = TRUE,
    ospsuite.plots.watermark_label = "preliminary analysis",
    ospsuite.plots.watermark_format = list(
      x = 0.5,
      y = 0.5,
      color = "grey20",
      angle = 30,
      fontsize = 12,
      alpha = 0.7
    ),
    # geom attributes
    ospsuite.plots.geomLineAttributes = list(),
    ospsuite.plots.geomRibbonAttributes = list(color = NA),
    ospsuite.plots.geomPointAttributes = list(),
    ospsuite.plots.geomErrorbarAttributes = list(width = 0),
    ospsuite.plots.geomLLOQAttributes = list(linetype = "dotted"),
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
    ospsuite.plots.geomBarAttributes = list(
      position = ggplot2::position_nudge()
    ),
    # default alpha
    ospsuite.plots.Alpha = 0.5,
    # alpha of LLOQ values
    ospsuite.plots.LLOQAlphaVector = c("TRUE" = 0.3, "FALSE" = 1),
    # percentiles
    ospsuite.plots.Percentiles = c(0.05, 0.25, 0.5, 0.75, 0.95),
    # Type of geom_point
    ospsuite.plots.GeomPointUnicode = FALSE
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
    "Attributes", "",
    gsub(
      "ospsuite.plots.geom", "",
      choices[grep("ospsuite.plots.geom", choices)]
    )
  )
  checkmate::assertChoice(geom, choices = choices)

  optionName <- paste0("ospsuite.plots.geom", geom, "Attributes")

  defaults <- getOption(optionName, default = getDefaultOptions()[[optionName]])

  return(defaults)
}


# Wrapper for all defaults  ----------------------

#' sets the defaults for Ospsuite.plot
#'
#' should be started at the beginning at each workflow
#'
#' for detailed information see
#' \code{vignette("ospsuite.plots", package = "ospsuite.plots")}
#'
#'
#' @param ColorMapList list of color maps
#' @param defaultOptions list of options
#' @param shapeValues list of Shapes
#' @param PointAsUnicode A `flag` to switch between mode for geom_point, if TRUE points will be plotted as unicode labels
#'
#' @return list of old settings which can be used to reset defaults with `resetDefaults()`
#'
#' @family setDefault functions
#' @export
setDefaults <- function(defaultOptions = list(),
                        ColorMapList = NULL,
                        shapeValues = NULL,
                        PointAsUnicode = FALSE) {
  checkmate::assertList(ColorMapList, null.ok = TRUE)
  checkmate::assertList(shapeValues, null.ok = TRUE)
  checkmate::assertList(defaultOptions, null.ok = TRUE)
  checkmate::assertFlag(PointAsUnicode, null.ok = TRUE)

  # initialize return value
  oldDefaults <- list()

  # append default options with user input
  defaultOptions <- utils::modifyList(
    getDefaultOptions(),
    defaultOptions
  )

  # switch between UniCodeMode and ggplot defaults
  PointAsUnicode <- PointAsUnicode |
    defaultOptions$ospsuite.plots.GeomPointUnicode
  if (PointAsUnicode) {
    defaultOptions <- utils::modifyList(
      defaultOptions,
      list(ospsuite.plots.GeomPointUnicode = TRUE)
    )

    if (!("size" %in% defaultOptions$ospsuite.plots.geomPointAttributes)) {
      defaultOptions$ospsuite.plots.geomPointAttributes <-
        utils::modifyList(
          defaultOptions$ospsuite.plots.geomPointAttributes,
          list(size = 4)
        )
    }

    showtext::showtext_auto()
  } else {
    if (getOption(
      x = "ospsuite.plots.GeomPointUnicode",
      default = getDefaultOptions()[["ospsuite.plots.GeomPointUnicode"]]
    )) {
      showtext::showtext_auto(enable = "off")
    }
  }
  oldDefaults$PointAsUnicode <-
    getOption(
      x = "ospsuite.plots.GeomPointUnicode",
      default = getDefaultOptions()[["ospsuite.plots.GeomPointUnicode"]]
    )

  # options
  oldDefaults$options <- lapply(names(getDefaultOptions()), getOption)
  names(oldDefaults$options) <- names(getDefaultOptions())

  options(defaultOptions)

  defaultAlpha <- getOption("ospsuite.plots.Alpha", getDefaultOptions()[["ospsuite.plots.Alpha"]])

  # get old settings of defaults for geoms
  nsenv <- asNamespace("ggplot2")

  oldDefaults[["geomPoint"]] <- get("GeomPoint", envir = nsenv)$default_aes
  oldDefaults[["geomBoxplot"]] <- get("GeomBoxplot", envir = nsenv)$default_aes
  oldDefaults[["geomBar"]] <- get("GeomBar", envir = nsenv)$default_aes
  oldDefaults[["geomRibbon"]] <- get("GeomRibbon", envir = nsenv)$default_aes
  oldDefaults[["geomLine"]] <- get("GeomLine", envir = nsenv)$default_aes

  # set theme, color and shapes

  oldDefaults[["theme"]] <- setDefaultTheme()
  oldDefaults[["colorMaps"]] <- setDefaultColorMapDistinct(ColorMapList = ColorMapList)
  oldDefaults[["shapeValues"]] <- setDefaultShapeDiscrete(shapeValues = shapeValues)
  shapeValues <- getOption("ospsuite.plots.shapeValues")

  # set geoms
  if (!PointAsUnicode) {
    update_geom_defaults("point", list(
      shape = shapeValues[1]
    ))
  }
  update_geom_defaults("boxplot", list(
    fill = getOption("ggplot2.discrete.fill")[[1]][[1]],
    alpha = defaultAlpha
  ))
  update_geom_defaults("bar", list(
    fill =  getOption("ggplot2.discrete.fill")[[1]][[1]],
    alpha = defaultAlpha,
    color = "black"
  ))
  update_geom_defaults("ribbon", list(
    fill = getOption("ggplot2.discrete.fill")[[1]][[1]],
    alpha = defaultAlpha
  ))
  update_geom_defaults("line", list(
    linewidth = 1.25,
    fill = getOption("ggplot2.discrete.fill")[[1]][[1]]
  ))


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



  # switch between UniCodeMode and ggplot defaults
  currentPointAsUnicode <-
    getOption(
      x = "ospsuite.plots.GeomPointUnicode",
      default = getDefaultOptions()[["ospsuite.plots.GeomPointUnicode"]]
    )

  if (oldDefaults$PointAsUnicode & !currentPointAsUnicode) {
    showtext::showtext_auto()
  }
  if (!oldDefaults$PointAsUnicode & currentPointAsUnicode) {
    showtext::showtext_auto(enable = "off")
  }

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

  resetDefaultShapeDiscrete(oldShapeValues = oldDefaults$shapeValues)
}
