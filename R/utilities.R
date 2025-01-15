#' adjust arguments for scale if dimension of scale is time
#'
#' adds break function with fixed width for breaks depending on unit:
#'
#' * s: width = 15,
#' * min: width = 15,
#' * h: width = 6,
#' * day(s): width = 7
#' * week(s): width = 4
#' * month(s): width = 6
#'
#' use width only if 2 * width < range of time values
#' use multiples of width if 20 * width < range of time values
#'
#' @param scale.args list of arguments for scale to be updated, passed to scale_x_continuous or scale_x_log10
#' @param dimension  dimension of axis, if not 'time' list will not be updated
#' @param unit A named list of information about the `data` such as the `dimension` and `unit` of its variables.
#'
#' @return update list of arguments for scale
#'
#' @export
#'
#' @examples
#' xscale.args <- list(limits = c(0, 24))
#' xscale.args <-
#'   updateScaleArgumentsForTimeUnit(
#'     scale.args = xscale.args,
#'     dimension = "time",
#'     unit = "h"
#'   )
#' addXscale(plotObject = ggplot(), xscale = "linear", xscale.args = xscale.args)
updateScaleArgumentsForTimeUnit <- function(scale.args,
                                            dimension,
                                            unit) {
  ## Validation
  checkmate::assertList(scale.args, null.ok = TRUE)

  # check if anything to do
  if (any(c("breaks", "labels") %in% names(scale.args))) {
    return(scale.args)
  }

  checkmate::assertCharacter(dimension, max.len = 1, null.ok = TRUE)
  checkmate::assertCharacter(unit, max.len = 1, null.ok = TRUE)


  if (is.null(dimension) | is.null(unit)) {
    return(scale.args)
  }


  # if x has no time Unit return
  if (tolower(dimension) != "time") {
    return(scale.args)
  }


  timeBreaks <- function(width) {
    function(x) {
      if (2 * width > diff(range(x))) {
        return(scales::breaks_extended(n = 5)(x))
      } else {
        while (10 * width < diff(range(x))) {
          width <- width * 2
        }
        return(x <- scales::fullseq(x, width))
      }
    }
  }

  scale.args$breaks <- switch(tolower(unit),
    "s" = timeBreaks(15),
    "min" = timeBreaks(15),
    "h" = timeBreaks(6),
    "day(s)" = timeBreaks(7),
    "week(s)" = timeBreaks(4),
    "month(s)" = timeBreaks(6)
  )


  # use minor steps 1
  scale.args$minor_breaks <- scales::breaks_width(1)


  return(scale.args)
}


#' adds a labels by meta data to ggplot object
#'
#' @param mappedData  MappedData object with information of mapped dimensions and units
#' @param plotObject  A `ggplot` object on which to add the labels
#'
#' @keywords internal
#' @return  updated `ggplot` object
addLabels <- function(plotObject, mappedData) {

  plotLabels <- plotObject$labels
  if (!is.null(mappedData)) {
    plotLabelsByMetData <- createDefaultPlotLabels(
      mappedData
    )
    plotLabels <- utils::modifyList(plotLabels, plotLabelsByMetData)
  }
  # set labels
  plotObject <- plotObject + do.call(labs, plotLabels)

  return(plotObject)
}


#' create Default labels with unit for plot
#'
#' @param mappedData  MappedData object with information of mapped dimensions and units
#'
#' @return  list with plotLabels
#' @keywords internal
createDefaultPlotLabels <- function(mappedData) {
  # match mapping to axis
  matchList <- list(
    x = "x",
    y = listOfAesthetics[which(listOfAesthetics$scalingRelevant >= 1), ]$aesthetic,
    y2 = "y2"
  )

  # get Labels
  plotLabels <- list()
  for (labelEntry in names(matchList)) {
    mapEntry <- intersect(
      matchList[[labelEntry]],
      names(mappedData$mapping)
    )

    for (aesthetic in mapEntry) {
      dimension <- mappedData$dimensions[[aesthetic]]
      unit <- mappedData$units[[aesthetic]]

      if (!is.null(dimension) & !is.null(unit)) {
        if (trimws(unit) != "") {
          plotLabels[[labelEntry]] <-  paste0(trimws(dimension), " [", trimws(unit), "]")
        } else {
          plotLabels[[labelEntry]] <- trimws(dimension)
        }
      }
    }
  }

  return(plotLabels)
}


#' converts metaData List to a data frame
#' row names specify properties
#'
#' @param metaData A named list of information about the `data` such as the `dimension` and `unit` of its variables.
#'
#' @export
#' @return metaData as `data.frame`
#'
metaData2DataFrame <- function(metaData) {
  metaDF <- data.frame()

  for (element in c("dimension", "unit")) {
    tmp <- lapply(metaData, getElement, element) %>%
      lapply(function(x) {
        ifelse(is.null(x), "", x)
      }) %>%
      as.data.frame()
    rownames(tmp) <- element

    metaDF <- rbind(
      metaDF,
      tmp
    )
  }


  return(metaDF)
}


#' creates a list with fold Distances
#'
#' this list is used as input for `plotRatioVsCov`, `plotPredVsObs`
#'
#' @param folds of folds e.g. c(1.5,2) must be >1
#' @param includeIdentity A `boolean`, if TRUE (default) line of identity is added
#'
#' @return named list with fold distances
#' @export
getFoldDistanceList <- function(folds = c(1.5, 2),
                                includeIdentity = TRUE) {
  checkmate::assertDouble(folds, lower = 1,null.ok = TRUE)

  foldDistance <- list()

  if (includeIdentity) {
    foldDistance[["identity"]] <- 1
  }

  for (fd in folds) {
    foldDistance[[paste(fd, "fold")]] <- c(fd, 1 / fd)
  }

  return(foldDistance)
}
