#' adjust arguments for scale if dimension of scale is time
#'
#' use metadata and mapping to determine dimension of axis
#' use fixed width for breaks depending on unit:
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
#' @param scale.args list of arguments for scale to be updated
#' @param mapping  Default list of aesthetic mappings to use for plot
#' @param metaData A named list of information about the `data` such as the `dimension` and `unit` of its variables.
#' @param scaleDirection either 'x' or 'y'
#'
#' @return update list of arguments for scale
#' @export
#'
#' @examples
#' newScaleArguments <- updateScaleArgumentsForTimeUnit(
#'   scale.args = list(),
#'   metaData = list(time = list(dimension = "Time", unit = "h")),
#'   mapping = aes(x = time)
#' )
#'
updateScaleArgumentsForTimeUnit <- function(scale.args,
                                            metaData,
                                            mapping,
                                            scaleDirection = "x") {
  ## Validation
  checkmate::assertList(metaData, types = "list", null.ok = TRUE)

  # check if anything to do
  if (is.null(metaData)) {
    return(scale.args)
  }
  if (any(c("breaks", "labels") %in% names(scale.args))) {
    return(scale.args)
  }

  # get unit and dimension of x Axis
  metaDF <- metaData2DataFrame(metaData)
  tmp <- getDataForAesthetic(
    aesthetic = scaleDirection,
    data = metaDF,
    mapping = mapping
  )

  # if x has no metadata return
  if (is.null(tmp)) {
    return(scale.args)
  }
  names(tmp) <- rownames(metaDF)

  dimension <- tmp[["dimension"]]
  unit <- tmp[["unit"]]


  # if x has no time Unit return
  if (tolower(dimension) != "time") {
    return(scale.args)
  }


  timeBreaks <- function(width) {
    function(x) {
      if (2 * width > diff(range(x))) {
        return(scales::breaks_extended(n = 5)(x))
      } else {
        while (20 * width < diff(range(x))) {
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
#' @inheritParams initializePlot
#' @param plotObject  A `ggplot` object on which to add the labels
#'
#' @keywords internal
#' @return  updated `ggplot` object
.addLabels <- function(plotObject, metaData, mapping) {
  plotLabels <- plotObject$labels
  if (!is.null(metaData) & !is.null(mapping)) {
    plotLabelsByMetData <- createDefaultPlotLabels(
      metaData = metaData,
      mapping = mapping
    )
    plotLabels <- c(plotLabels, plotLabelsByMetData)
    plotLabels <- plotLabels[unique(names(plotLabels))]
  }
  # set labels
  if (!is.null(plotLabels)) {
    plotObject <- plotObject +
      eval(parse(text = paste0(
        "labs(",
        paste(
          lapply(names(plotLabels), function(i) paste0(i, " = '", plotLabels[[i]], "'")),
          collapse = ","
        ),
        ")"
      )))
  }

  return(plotObject)
}


#' create Default labels with unit for plot
#'
#' @param mapping  Default list of aesthetic mappings to use for plot
#' @param metaData A named list of information about the `data` such as the `dimension` and `unit` of its variables.
#'
#' @return  list with plotLabels
#' @keywords internal
createDefaultPlotLabels <- function(metaData,
                                    mapping) {
  # initialize variables used in aes or data,table to avoid message in check
  y2 <- scalingRelevant <- NULL

  # convert metaData
  metaDF <- metaData2DataFrame(metaData)

  # add y2
  mapping <- addOverwriteAes(newMaps = aes(y2 = y2), mapping = mapping)

  # match mapping to axis
  matchList <- list(
    x = "x",
    y = listOfAesthetics[scalingRelevant >= 1, ]$aesthetic,
    y2 = "y2"
  )

  # get Labels
  plotLabels <- list()
  for (labelEntry in names(matchList)) {
    mapEntry <- intersect(
      matchList[[labelEntry]],
      names(mapping)
    )

    for (aesthetic in mapEntry) {
      tmp <- getDataForAesthetic(
        aesthetic = aesthetic,
        data = metaDF,
        mapping = mapping
      )

      if (!is.null(tmp)) {
        names(tmp) <- rownames(metaDF)
        if (trimws(tmp[["unit"]]) != "") {
          plotLabels[[labelEntry]] <- trimws(paste0(tmp[["dimension"]], " [", tmp[["unit"]], "]"))
        } else {
          plotLabels[[labelEntry]] <- tmp[["dimension"]]
        }
      }
    }
  }

  return(plotLabels)
}



#' getDataForAesthetic
#'
#' @param aesthetic Aesthetic fro which the the data is required
#' @param data  data.frame with data
#' @param mapping  Default list of aesthetic mappings to use for plot
#' @param stopIfNull If TRUE error will be thrown if data column does not exists,
#'    IF FALSE for non existing columns NULL will be returned
#'
#' @return vector with values corresponding to the mapped data column
#' @keywords internal
getDataForAesthetic <- function(aesthetic,
                                data,
                                mapping,
                                stopIfNull = FALSE) {
  dataCol <- tryCatch(
    {
      rlang::eval_tidy(
        expr = rlang::get_expr(mapping[[aesthetic]]),
        data = data,
        env = rlang::get_env(mapping[[aesthetic]])
      )
    },
    error = function(cond) {
      if (stopIfNull) {
        stop(paste("evaluation of aesthetic", aesthetic, "failed:", cond))
      } else {
        dataCol <- NULL
      }
    }
  )

  return(dataCol)
}


#' adds and update mapping
#'
#' @param mapping  Default list of aesthetic mappings to use for plot
#' @param newMaps new mapping entry
#'
#' @return uppdated mapping
#' @keywords internal
addOverwriteAes <- function(newMaps, mapping) {
  checkmate::assertList(newMaps, names = "named")

  mapping <-
    mapping[setdiff(names(mapping), names(newMaps))]

  mapping <-
    structure(c(mapping, newMaps), class = "uneval")

  return(mapping)
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
