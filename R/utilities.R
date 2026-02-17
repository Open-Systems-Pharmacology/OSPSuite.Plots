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
#' The function uses the following logic to determine the breaks:
#' - If the range of time values is relatively small (i.e., less than twice the width of the breaks),
#'   it will use a default set of extended breaks.
#' - If the range of time values is larger, the function will check if it is appropriate to use
#'   wider breaks. Specifically, it will continue to double the width until it finds a width
#'   that is suitable, ensuring that 10 times the width is still less than the total range of time values.
#'   This means that the breaks will be spaced far enough apart to be meaningful without overcrowding
#'   the axis, providing clarity in the visualization.
#'
#' @param scaleArgs list of arguments for scale to be updated, passed to scale_x_continuous or scale_x_log10
#' @param dimension  dimension of axis, if not 'time' list will not be updated
#' @param unit A named list of information about the `data` such as the `dimension` and `unit` of its variables.
#'
#' @return update list of arguments for scale
#'
#' @export
#'
#' @examples
#' xScaleArgs <- list(limits = c(0, 24))
#' xScaleArgs <-
#'   updateScaleArgumentsForTimeUnit(
#'     scaleArgs = xScaleArgs,
#'     dimension = "time",
#'     unit = "h"
#'   )
#' addXScale(plotObject = ggplot(), xScale = "linear", xScaleArgs = xScaleArgs)
updateScaleArgumentsForTimeUnit <- function(scaleArgs,
                                            dimension,
                                            unit) {
  ## Validation
  checkmate::assertList(scaleArgs, null.ok = TRUE)

  # check if anything to do
  if (any(c("breaks", "labels") %in% names(scaleArgs))) {
    return(scaleArgs)
  }

  checkmate::assertCharacter(dimension, max.len = 1, null.ok = TRUE)
  checkmate::assertCharacter(unit, max.len = 1, null.ok = TRUE)


  if (is.null(dimension) | is.null(unit)) {
    return(scaleArgs)
  }


  # if x has no time Unit return
  if (tolower(dimension) != "time") {
    return(scaleArgs)
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

  scaleArgs$breaks <- switch(tolower(unit),
    "s" = timeBreaks(15),
    "min" = timeBreaks(15),
    "h" = timeBreaks(6),
    "day(s)" = timeBreaks(7),
    "week(s)" = timeBreaks(4),
    "month(s)" = timeBreaks(6)
  )


  # use minor steps 1
  scaleArgs$minor_breaks <- scales::breaks_width(1)


  return(scaleArgs)
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


#' create default labels with unit for plot
#'
#' @param mappedData  `MappedData` object with information of mapped dimensions and units
#'
#' @return  list with plot labels
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
      if (!is.null(dimension)) {
        plotLabels[[labelEntry]] <- constructLabelWithUnit(label = dimension, unit = unit)
      }
    }
  }

  return(plotLabels)
}
#' Construct a Label with Unit
#'
#' This function constructs a label by appending a unit in square brackets
#' if both the label and unit are provided. If the unit is empty or NULL,
#' only the label is returned.
#'
#' @param label A character string representing the label. It should not be NULL.
#' @param unit A character string representing the unit. It can be NULL or an empty string.
#'
#' @return A character string that combines the label and the unit, formatted as
#'         "label [unit]", or just the label if the unit is empty or NULL.
#'
#' @examples
#' constructLabelWithUnit("Temperature", "Celsius") # Returns "Temperature [Celsius]"
#' constructLabelWithUnit("Length", "") # Returns "Length"
#' constructLabelWithUnit(NULL, "kg") # Returns NULL
#'
#' @export
constructLabelWithUnit <- function(label, unit) {
  # Validate input arguments
  if (is.factor(label)) label <- as.character(label)
  if (is.double(label)) label <- as.character(label)
  checkmate::assertCharacter(label, len = 1, null.ok = TRUE)
  if (is.factor(unit)) unit <- as.character(unit)
  checkmate::assertCharacter(unit, len = 1, null.ok = TRUE)

  if (!is.null(label) & !is.null(unit)) {
    if (trimws(unit) != "") {
      label <- paste0(trimws(label), " [", trimws(unit), "]")
    } else {
      label <- trimws(label)
    }
  }
  return(label)
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
  checkmate::assertList(metaData, null.ok = TRUE)
  metaDF <- data.frame()

  if (length(metaData) == 0) {
    return(metaDF)
  }

  for (element in c("dimension", "unit")) {
    tmp <- lapply(metaData, getElement, element) |>
      lapply(function(x) {
        ifelse(is.null(x), "", x)
      }) |>
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
  checkmate::assertDouble(folds, lower = 1, null.ok = TRUE)

  foldDistance <- list()

  if (includeIdentity) {
    foldDistance[["identity"]] <- 1
  }

  for (fd in folds) {
    foldDistance[[paste(fd, "fold")]] <- c(fd, 1 / fd)
  }

  return(foldDistance)
}

#' Calculate Residuals
#'
#' This function calculates residuals from predicted and observed values using different scaling methods.
#' The calculation method is consistent with the residual calculation used in `plotResVsCov()` and other
#' plotting functions in the ospsuite.plots package.
#'
#' @param predicted A numeric vector of predicted values. Must have the same length as `observed`.
#' @param observed A numeric vector of observed values. Must have the same length as `predicted`.
#' @param scaling A character string specifying the scaling method. Must be one of:
#'   * `"log"` (default): Residuals are calculated as `log(predicted) - log(observed)`.
#'     Invalid values (non-positive) or NA values are set to NA with a warning.
#'   * `"linear"`: Residuals are calculated as `predicted - observed`.
#'     NA values are set to NA with a warning.
#'   * `"ratio"`: Residuals are calculated as `predicted / observed`.
#'     Invalid values (zero observed values) or NA values are set to NA with a warning.
#'
#' @return A numeric vector of residuals with the same length as the input vectors.
#'   Invalid calculations and NA values are returned as NA.
#'
#' @details
#' This function implements the same residual calculation logic used internally by
#' the ospsuite.plots package when creating residual plots with `plotResVsCov()` and
#' `plotRatioVsCov()`. It is provided as a standalone function to enable consistent
#' residual calculations across different packages in the Open Systems Pharmacology ecosystem.
#'
#' **Calculation Details:**
#'
#' * **Log scaling**: `log(predicted) - log(observed)`
#'   - NA values are set to NA with a warning
#'   - Non-positive values are set to NA with a warning
#'   - Symmetric for over- and under-prediction on log scale
#'   - Commonly used for pharmacokinetic data
#'
#' * **Linear scaling**: `predicted - observed`
#'   - NA values are set to NA with a warning
#'   - Standard residual calculation
#'   - Positive values indicate over-prediction
#'   - Negative values indicate under-prediction
#'
#' * **Ratio scaling**: `predicted / observed`
#'   - Returns ratios instead of differences
#'   - NA values are set to NA with a warning
#'   - Zero observed values are set to NA with a warning
#'   - Values > 1 indicate over-prediction
#'   - Values < 1 indicate under-prediction
#'   - Value of 1 indicates perfect prediction
#'
#' @examples
#' # Example data
#' predicted <- c(1.5, 2.0, 3.5, 5.0, 7.5)
#' observed <- c(1.2, 2.1, 3.0, 5.5, 7.0)
#'
#' # Calculate residuals with different scaling methods
#' residualsLog <- calculateResiduals(predicted, observed, scaling = "log")
#' residualsLinear <- calculateResiduals(predicted, observed, scaling = "linear")
#' residualsRatio <- calculateResiduals(predicted, observed, scaling = "ratio")
#'
#' # Compare results
#' data.frame(
#'   predicted = predicted,
#'   observed = observed,
#'   logResiduals = residualsLog,
#'   linearResiduals = residualsLinear,
#'   ratioResiduals = residualsRatio
#' )
#'
#' # Example with invalid values
#' predictedInvalid <- c(1.5, -2.0, 3.5)
#' observedInvalid <- c(1.2, 2.1, 0)
#'
#' # Log scaling warns about non-positive values and returns NA
#' residualsLogInvalid <- calculateResiduals(predictedInvalid, observedInvalid, scaling = "log")
#'
#' # Ratio scaling warns about zero observed values and returns NA
#' residualsRatioInvalid <- calculateResiduals(predictedInvalid, observedInvalid, scaling = "ratio")
#'
#' @export
calculateResiduals <- function(predicted,
                              observed,
                              scaling = ResidualScales$log) {
  # Validation
  checkmate::assertNumeric(predicted, any.missing = TRUE, min.len = 1)
  checkmate::assertNumeric(observed, any.missing = TRUE, min.len = 1)
  checkmate::assertChoice(scaling, choices = c(
    ResidualScales$linear,
    ResidualScales$log,
    ResidualScales$ratio
  ))

  # Check that vectors have the same length
  if (length(predicted) != length(observed)) {
    stop("predicted and observed must have the same length")
  }

  # Check for NA values and warn
  naPredicted <- is.na(predicted)
  naObserved <- is.na(observed)
  naValues <- naPredicted | naObserved

  if (any(naValues)) {
    nNa <- sum(naValues)
    warning(sprintf(
      "%d residual value%s set to NA: NA values found in predicted or observed",
      nNa,
      ifelse(nNa == 1, "", "s")
    ))
  }

  # Initialize residuals vector
  residuals <- rep(NA_real_, length(predicted))

  # Calculate residuals based on scaling method
  if (scaling == ResidualScales$log) {
    # Check for positive values
    invalidPredicted <- predicted <= 0 & !is.na(predicted)
    invalidObserved <- observed <= 0 & !is.na(observed)
    invalidIndices <- invalidPredicted | invalidObserved

    if (any(invalidIndices)) {
      nInvalid <- sum(invalidIndices)
      warning(sprintf(
        "%d residual value%s set to NA: non-positive values found for log scaling",
        nInvalid,
        ifelse(nInvalid == 1, "", "s")
      ))
    }

    # Calculate residuals for valid values
    validIndices <- !invalidIndices & !naValues
    residuals[validIndices] <- log(predicted[validIndices]) - log(observed[validIndices])
  } else if (scaling == ResidualScales$linear) {
    residuals <- predicted - observed
  } else if (scaling == ResidualScales$ratio) {
    # Check for zero observed values
    invalidObserved <- observed == 0 & !is.na(observed)

    if (any(invalidObserved)) {
      nInvalid <- sum(invalidObserved)
      warning(sprintf(
        "%d residual value%s set to NA: zero observed values found for ratio scaling",
        nInvalid,
        ifelse(nInvalid == 1, "", "s")
      ))
    }

    # Calculate residuals for valid values
    validIndices <- !invalidObserved & !naValues
    residuals[validIndices] <- predicted[validIndices] / observed[validIndices]
  }

  return(residuals)
}
