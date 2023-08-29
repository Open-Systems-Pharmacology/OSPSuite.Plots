# Default Theme -------------

#' @title set the default theme
#' @description set properties of the default theme
#'
#' @return  invisibly return the previous theme so you can easily save it, then later restore it.
#'
#' @export
#' @family setDefaultsOspsuite.plots
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


# Default ColorMap Discrete -------------

#' @title ColorMaps
#'
#' @description
#'
#' List with some color maps for `Theme` object.
#'
#' The `ospDefault` color map is based on colors in `default_igv` qualitative
#' color palette from `{ggsci}` package.
#'
#' @family setDefaultsOspsuite.plots
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
#' @title set the default colormap for discret colors
#'
#' @return list wih COlormpas previously set
#'
#' @family setDefaultsOspsuite.plots
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
    "ggplot2.discrete.fill",
    "ggplot2.ordinal.colour",
    "ggplot2.ordinal.fill"
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
#' @title reset the default colormap for discret colors
#'
#' @param oldColorMaps list of colormaps prevoisuly set
#'
#' @export
#' @family setDefaultsOspsuite.plots
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

#' set the default shapes scale for discrete values
#'
#' @param shapeValues vector of shapevalues
#'
#' @export
#' @family setDefaultsOspsuite.plots
setDefaultshapeDiscrete <- function(shapeValues = NULL) {
  if (is.null(shapeValues)) {
    shapeValues <-
      c("circle filled", "diamond filled", "triangle filled", "square filled")
  }
  # shape
  scale_shape_discrete <- function(...) {
    scale_shape_manual(values = shapeValues)
  }
  assign("scale_shape_discrete", scale_shape_discrete, envir = globalenv())
}


#' resets the scale for discrete shapes to ggplot default
#'
#' @family setDefaultsOspsuite.plots
#' @export
resetDefaultshapeDiscrete <- function() {
  assign("scale_shape_discrete", ggplot2::scale_shape_discrete, envir = globalenv())
}


# Options ----------------

#' get list of default options
#'
#' @family setDefaultsOspsuite.plots
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
    ospsuite.plots.geomRatioLineAttributes = list(linetype = "dashed"),
    ospsuite.plots.geomBoxplotAttributes = list(
      position = position_dodge(width = 1),
      color = "black"
    ),
    ospsuite.plots.geomBarAttributes = list(
      bins = 10,
      position = ggplot2::position_nudge()
    ),
    # default alpha
    ospsuite.plots.Alpha = 0.7,
    # alpha of LLOQ values
    ospsuite.plots.LLOQAlphaVector = c("TRUE" = 0.618, "FALSE" = 1),
    # percentiles
    ospsuite.plots.Percentiles = c(0.05, 0.25, 0.5, 0.75, 0.95)
  )

  return(optionList)
}

# Geoms ----------

#' get the defaults for the geom attributes used as defaults in plot functions
#' see \code{vignette("ospsuite.plots", package = "ospsuite.plots")} how to chnage defaults
#'
#' @param geom  type of geom to return attributes
#'
#' @family setDefaultsOspsuite.plots
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
#' for detailde information see
#' \code{vignette("ospsuite.plots", package = "ospsuite.plots")}
#'
#'
#' @param ColorMapList list of color maps
#' @param defaultOptions list of options
#' @param shapeValues list of Shapes
#'
#' @return list of old settings which can be used to reset defaults with restggOPSDefaults
#'
#' @family setDefaultsOspsuite.plots
#' @export
setDefaults <- function(ColorMapList = NULL,
                        shapeValues = NULL,
                        defaultOptions = getDefaultOptions()) {
  oldDefaults <- list()

  # get old settings of defaults for geoms
  nsenv <- asNamespace("ggplot2")

  oldDefaults[["geomBar"]] <- get("GeomBar", envir = nsenv)$default_aes
  oldDefaults[["geoPoint"]] <- get("GeomPoint", envir = nsenv)$default_aes
  oldDefaults[["geomBoxplot"]] <- get("GeomBoxplot", envir = nsenv)$default_aes
  oldDefaults[["geomRibbon"]] <- get("GeomRibbon", envir = nsenv)$default_aes
  oldDefaults[["geomLine"]] <- get("GeomLine", envir = nsenv)$default_aes

  # set them, color and shapes

  oldDefaults[["theme"]] <- setDefaultTheme()
  oldDefaults[["colorMaps"]] <- setDefaultColorMapDistinct(ColorMapList = ColorMapList)
  setDefaultshapeDiscrete(shapeValues = shapeValues)

  # options
  options(defaultOptions)

  defaultAlpha <- getOption("ospsuite.plots.Alpha", getDefaultOptions()[["ospsuite.plots.Alpha"]])

  # set geoms
  update_geom_defaults("point", list(
    shape = "circle"
  ))
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
    fill = getOption("ggplot2.discrete.fill")[[1]][[1]],
    alpha = defaultAlpha
  ))


  return(invisible(oldDefaults))
}





#' restore to previously stored settings
#'
#' @param oldDefaults `list`with previously stored settings
#'
#' @family setDefaultsOspsuite.plots
#' @export
#'
resetDefaults <- function(oldDefaults) {
  ggplot2::theme_set(oldDefaults$theme)


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

  resetDefaultshapeDiscrete()
}
