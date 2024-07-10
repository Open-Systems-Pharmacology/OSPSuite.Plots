#' @title export plot object
#' @description saves plot object to file
#'
#' For more details and examples see the vignettes:
#' * \code{vignette("ospsuite.plots", package = "ospsuite.plots")}
#'
#' @param plotObject ggplot object
#' @param filename name of file to save.
#'   extension is added or switched to default device
#'   `getOspsuite.plots.option(optionKey = OptionKeys$export.device)`
#' @param filepath name of export directory
#' @param width `double` width of exported file, defaults is `getOption('ospsuite.plots.export.width',default = 16)`
#'        unit `getOption('ospsuite.plots.export.units',  default = "cm")`
#' @param height `double` height of exported plot, if NULL the aspect ratio of plot is used,
#'            or if no aspect ratio is defined, a square plot is exported
#' @param ... Other arguments passed on to the ggsave
#'
#' @export
exportPlot <- function(plotObject,
                       filepath,
                       filename,
                       width = getOspsuite.plots.option(optionKey = OptionKeys$export.width),
                       height = NULL,
                       ...) {
  validateInputsExportPlot(plotObject,
    filepath,
    filename,
    width = getOspsuite.plots.option(optionKey = OptionKeys$export.width),
    height = NULL
  )

  # set extension of default device
  filename <- paste0(
    tools::file_path_sans_ext(filename),
    ".",
    getOspsuite.plots.option(optionKey = OptionKeys$export.device)
  )


  if (is.null(height)) {
    dimensions <- calculatePlotDimensions(plotObject, width)
    width <- dimensions$width
    height <- dimensions$height
  }

  ggsave(
    filename = file.path(filepath, filename),
    plot = plotObject,
    device = getOspsuite.plots.option(optionKey = OptionKeys$export.device),
    width = width,
    height = height,
    dpi = getOspsuite.plots.option(optionKey = OptionKeys$export.dpi),
    units = getOspsuite.plots.option(optionKey = OptionKeys$export.units),
    ...
  )
}


#' Validates the inputs for the plot export
#'
#' @inheritParams exportPlot
validateInputsExportPlot <- function(plotObject,
                                     filepath,
                                     filename,
                                     width,
                                     height) {
  checkmate::assertClass(plotObject, "ggplot")
  checkmate::assertCharacter(filename)
  checkmate::assertCharacter(filepath)
  checkmate::assertDouble(width, null.ok = FALSE)
  checkmate::assertDouble(height, null.ok = TRUE)

  if (!dir.exists(filepath)) {
    dir.create(filepath, recursive = TRUE)
  }

  if (filename != basename(filename)) {
    stop("filename should not contain a path, use input variable filepath")
  }

  return(invisible())
}

#' Logic to calculate height and possibly
#' adjust width based on plot dimensions,
#' aspect ratio, legend positioning, etc.
#'
#' @inheritParams exportPlot
#'
#' @return a list or vector with the calculated width and height
calculatePlotDimensions <- function(plotObject, width) {
  themeOfPlot <- utils::modifyList(theme_get(), plotObject$theme)
  exportunits <- getOspsuite.plots.option(optionKey = OptionKeys$export.units)

  aspect.ratio <- themeOfPlot$aspect.ratio
  if (is.null(aspect.ratio)) aspect.ratio <- 1

  pb <- ggplot_build(plotObject)
  nCol <- max(pb$layout$layout$COL)
  nRow <- max(pb$layout$layout$ROW)
  nPanel <- length(pb$layout$layout$PANEL)

  plotDim <- getPlotDimensions(
    plotObject = plotObject,
    exportunits = exportunits,
    nCol = nCol,
    nRow = nRow,
    nPanel
  )


  # check if legend adds to height or to width,
  # if legend is numeric it i assumed legend is within panel
  if (plotDim$legendHeight > 0) {
    legendPosition <- themeOfPlot$legend.position
    legendAddsToHeight <- as.numeric(any(
      grepl(pattern = "Top", x = legendPosition, ignore.case = TRUE),
      grepl(pattern = "Bottom", x = legendPosition, ignore.case = TRUE)
    ))
    legendAddsToWidth <- as.numeric(any(
      grepl(pattern = "Left", x = legendPosition, ignore.case = TRUE),
      grepl(pattern = "Right", x = legendPosition, ignore.case = TRUE)
    ))
  } else {
    legendAddsToHeight <- 0
    legendAddsToWidth <- 0
  }

  plotMargins <- grid::convertUnit(themeOfPlot$plot.margin, unitTo = exportunits, valueOnly = TRUE)

  # Update width if top/bottom legend is too wide (add 5% to legend width to ensure all the entry content are displayed)
  width <- max(width, 1.05 * plotDim$legendWidth * legendAddsToHeight)

  # scale height with aspect ratio taking into account axis width and heights are ta
  height <- nRow / nCol * (width -
    plotDim$axisWidth -
    sum(plotMargins[c(2, 4)]) -
    plotDim$legendWidth * legendAddsToWidth) * aspect.ratio +
    plotDim$axisHeight +
    plotDim$legendHeight * legendAddsToHeight +
    sum(plotMargins[c(1, 3)])


  return(list(
    width = width,
    height = height
  ))
}


#' gets dimension of plots
#'
#' @param plotObject  ggplot object
#' @param exportunits units of exported figure
#' @param nCol for panel plots number of cols
#' @param nRow  for panel plots number of cols
#' @param nPanel for panel plots number of panels
#'
#' @return list with dimension o plot components
getPlotDimensions <- function(plotObject, exportunits, nCol, nRow, nPanel) {
  plot <- cowplot::as_gtable(plotObject)
  grobNames <- cowplot::plot_component_names(plot)
  grobs <- cowplot::plot_components(plot)


  .getGrobDimForPattern <- function(patterns, selectedDim, n = 1) {
    gIndexVector <- c()
    for (pattern in patterns) {
      gIndexVector <- c(gIndexVector, grep(pattern, grobNames))
    }

    values <- c()
    for (gIndex in gIndexVector) {
      if (selectedDim %in% names(grobs[[gIndex]])) {
        values <- c(
          values,
          as.numeric(grid::convertUnit(grobs[[gIndex]][[selectedDim]], exportunits))
        )
      }
    }
    if (length(values) == 0) {
      return(0)
    }

    if (pattern %in% c("axis-t", "axis-b", "axis-r", "axis-l")) {
      gropDim <- ceiling(length(values) / n) * mean(values)
    } else if (pattern %in% c("strip-t", "strip-b", "strip-r", "strip-l")) {
      gropDim <- n * mean(values)
    } else {
      gropDim <- sum(values)
    }


    return(gropDim)
  }


  return(list(
    axisHeight = .getGrobDimForPattern(
      patterns = c("axis-t", "axis-b"),
      selectedDim = "height",
      n = nCol
    ) +
      .getGrobDimForPattern(
        patterns = c("xlab-t", "xlab-b"),
        selectedDim = "heights"
      ) +
      .getGrobDimForPattern(
        patterns = c("strip-t", "strip-b"),
        selectedDim = "heights",
        n = nRow
      ),
    axisWidth = .getGrobDimForPattern(
      patterns = c("axis-r", "axis-l"),
      selectedDim = "width",
      n = nRow
    ) +
      .getGrobDimForPattern(
        patterns = c("ylab-l", "ylab-r"),
        selectedDim = "widths"
      ) +
      .getGrobDimForPattern(
        patterns = c("strip-r", "strip-l"),
        selectedDim = "widths",
        n = nCol
      ),
    legendWidth = .getGrobDimForPattern(
      patterns = c("guide-box"),
      selectedDim = "widths"
    ),
    legendHeight = .getGrobDimForPattern(
      patterns = c("guide-box"),
      selectedDim = "heights"
    )
  ))
}

#' replace of special letters
#'
#' @param filename name of file to validate
#'
#' @return filename without special letters
validateFilename <- function(filename) {
  # replace µ by mc
  filename <- iconv(filename, from = "UTF-8",to = "UTF-8")
  filename <- gsub("\u00B5", "mc", filename, fixed = TRUE)

  # Replace forbidden characters with the replacement character
  forbiddenChars <- c(":", "*", "?", "<", ">", "|", "\\", "/")
  replacementChar <- "_"
  for (char in forbiddenChars) {
    filename <- gsub(char, replacementChar, filename, fixed = TRUE)
  }


  return(filename)
}
