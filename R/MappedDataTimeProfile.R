# TimeProfile ----------------
#' @title MappedDataTimeProfile
#' @description  R6 class for mapping variable to `data`
#' @export
#' @family MappedData classes
MappedDataTimeProfile <- R6::R6Class( # nolint
  "MappedDataTimeProfile",
  inherit = MappedData,
  public = list(
    #' @field ylimits double vector limits of primary y axis
    ylimits = NULL,
    #' @field y2limits double vector limits of secondary y axis
    y2limits = NULL,
    #' @param data data.frame used for mapping
    #' @param mapping list of aesthetic mappings
    #' @param groupAesthetics vector of aesthetics, which are used for columns mapped with group,
    #'            use of group aesthetics triggers second axis after simulation layers
    #' @param direction direction of plot either "x" or "y"
    #' @param scaleOfDirection  scale of direction, either "linear" or "log"
    #' @param isObserved Flag if TRUE mappings mdv, lloq, error and error_relative are evaluated
    #' @param scaleOfSecondaryAxis either 'linear' or 'log'
    #' @param ylimits limits for primary axis (may be NULL)
    #' @param y2limits limits for secondary axis (may be NULL)
    #'
    #' @description Create a new `MappedDataTimeProfile` object
    #' @return A new `MappedDataTimeProfile` object
    initialize = function(data,
                          mapping,
                          groupAesthetics = NULL,
                          direction = "y",
                          scaleOfDirection = "linear",
                          isObserved = TRUE,
                          scaleOfSecondaryAxis = "linear",
                          ylimits = NULL,
                          y2limits = NULL) {
      super$initialize(
        data = data,
        mapping = mapping,
        groupAesthetics = groupAesthetics,
        direction = direction,
        isObserved = isObserved,
        scaleOfDirection = scaleOfDirection
      )
      # check for secondaryAxis
      if ("y2axis" %in% names(self$mapping)) {
        checkmate::assertLogical(private$getDataForAesthetic(aesthetic = "y2axis"),
          any.missing = FALSE, .var.name = "y2axis",
        )

        private$secondaryAxisAvailable <- any(private$getDataForAesthetic("y2axis"))
      } else {
        private$secondaryAxisAvailable <- FALSE
      }


      # set fields for secondary Axis
      if (private$secondaryAxisAvailable) {
        checkmate::assertChoice(scaleOfSecondaryAxis,
          choices = c("linear", "log"),
          null.ok = TRUE
        )
        checkmate::assertDouble(
          ylimits,
          sorted = TRUE,
          any.missing = TRUE,
          len = 2,
          unique = TRUE,
          null.ok = TRUE
        )
        checkmate::assertDouble(
          y2limits,
          sorted = TRUE,
          any.missing = TRUE,
          len = 2,
          unique = TRUE,
          null.ok = TRUE
        )


        private$scaleOfSecondaryAxis <- scaleOfSecondaryAxis
        self$ylimits <- ylimits
        self$y2limits <- y2limits

        # setLimits
        private$setLimits()

        return(invisible(self))
      }
    },
    #' scales data for secondary axis and updates filed secAxis
    #'
    #' @param ylimits limits for primary axis
    #' @param y2limits limits for secondary axis
    #' @param y2scale.args arguments for secondary axis
    #'
    #' @return updated MappedDataTimeProfile
    scaleDataForSecondaryAxis = function(ylimits = NULL,
                                         y2limits = NULL,
                                         y2scale.args = list()) {
      if (!self$requireDualAxis) {
        private$dataScaled <- self$data
        return(invisible(self))
      }

      checkmate::assertDouble(
        ylimits,
        sorted = TRUE,
        any.missing = TRUE,
        len = 2,
        unique = TRUE,
        null.ok = TRUE
      )
      ylimits <- ylimits %||% self$ylimits
      if (private$scaleOfDirection == "log") {
        checkmate::assertDouble(log(ylimits), finite = TRUE)
      }

      checkmate::assertDouble(
        y2limits,
        sorted = TRUE,
        any.missing = TRUE,
        len = 2,
        unique = TRUE,
        null.ok = TRUE
      )
      y2limits <- y2limits %||% self$y2limits
      if (private$scaleOfSecondaryAxis == "log") {
        checkmate::assertDouble(log(y2limits), finite = TRUE)
      }

      # split data into two data sets
      dataUnscaled <- self$data %>%
        dplyr::filter(!!self$mapping[["y2axis"]] == FALSE)

      dataScaled <- self$data %>%
        dplyr::filter(!!self$mapping[["y2axis"]] == TRUE)

      # get Scaling function
      if (private$scaleOfSecondaryAxis == "linear") {
        if (private$scaleOfDirection == "linear") {
          offsetlin1 <- self$ylimits[1]
          deltalin1 <- diff(self$ylimits)

          offsetlin2 <- self$y2limits[1]
          deltalin2 <- diff(self$y2limits)

          funScale <- function(y2) {
            return((y2 - offsetlin2) / deltalin2 * deltalin1 + offsetlin1)
          }

          funScaleAxis <- function(ytrans) {
            return((ytrans - offsetlin1) / deltalin1 * deltalin2 + offsetlin2)
          }
        } else if (private$scaleOfDirection == "log") {
          offsetlin <- self$y2limits[1]
          deltalin <- diff(self$y2limits)

          offsetlog <- log(self$ylimits[1])
          deltalog <- diff(log(self$ylimits))

          funScale <- function(ylin) {
            return(exp((ylin - offsetlin) / deltalin * deltalog + offsetlog))
          }

          funScaleAxis <- function(yt) {
            return((log(yt) - offsetlog) / deltalog * deltalin + offsetlin)
          }
        }
      } else if (private$scaleOfSecondaryAxis == "log") {
        if (private$scaleOfDirection == "linear") {
          offsetlin <- self$ylimits[1]
          deltalin <- diff(self$ylimits)

          offsetlog <- log(self$y2limits[1])
          deltalog <- diff(log(self$y2limits))

          funScale <- function(ylog) {
            return((log(ylog) - offsetlog) / deltalog * deltalin + offsetlin)
          }

          funScaleAxis <- function(ylin) {
            return(exp((ylin - offsetlin) / deltalin * deltalog + offsetlog))
          }
        } else if (private$scaleOfDirection == "log") {
          offsetlog1 <- log(self$ylimits[1])
          deltalog1 <- diff(log(self$ylimits))

          offsetlog2 <- log(self$y2limits[1])
          deltalog2 <- diff(log(self$y2limits))

          funScale <- function(y2) {
            return(exp((log(y2) - offsetlog2) / deltalog2 * deltalog1 + offsetlog1))
          }

          funScaleAxis <- function(yt) {
            return(exp((log(yt) - offsetlog1) / deltalog1 * deltalog2 + offsetlog2))
          }
        }
      }
      # get data columns to scale
      scalingRelevantMappings <- listOfAesthetics[get("scalingRelevant") >= 1]$aesthetic %>%
        intersect(names(self$mapping))


      for (aesthetic in scalingRelevantMappings) {
        tmp <- private$getDataForAesthetic(
          aesthetic = aesthetic,
          stopIfNull = FALSE
        )

        if (!is.null(tmp) && is.numeric(tmp)) {
          dataScaled <- dataScaled %>%
            dplyr::mutate(!!self$mapping[[aesthetic]] :=
              funScale(!!self$mapping[[aesthetic]]))
        }
      }


      # merge data
      private$dataScaled <- rbind(
        dataUnscaled,
        dataScaled
      )

      # set scale
      y2scale.args[["trans"]] <- funScaleAxis
      y2scale.args[["guide"]] <- "axis_minor"
      if (private$scaleOfSecondaryAxis == "log") {
        y2scale.args[["breaks"]] <- scales::breaks_log(5, base = 10)(self$y2limits)
      } else if (private$scaleOfSecondaryAxis == "linear") {
        y2scale.args[["breaks"]] <- scales::breaks_extended()(self$y2limits)
      }
      y2scale.args$limits <- NULL
      private$.secAxis <- do.call(
        what = sec_axis,
        args = y2scale.args
      )


      return(invisible(self))
    }
  ),
  ## active -------
  active = list(
    #' Flag for secondary axis
    #'
    #' @field requireDualAxis Flag, If TRUE secondary axis is required
    requireDualAxis = function() {
      private$secondaryAxisAvailable
    },
    #' @field secAxis sec_axis() object
    secAxis = function() {
      if (is.null(private$.secAxis)) {
        return(waiver())
      } else {
        return(private$.secAxis)
      }
    },
    #' @field dataForPlot scaled data used for plotting
    dataForPlot = function() {
      if (private$secondaryAxisAvailable) {
        return(private$dataScaled)
      } else {
        return(self$data)
      }
    }
  ),
  ## private -------
  private = list(
    scaleOfSecondaryAxis = "linear",
    secondaryAxisAvailable = NULL,
    dataScaled = NULL,
    .secAxis = NULL,
    #' adjust limits
    setLimits = function() {
      # get data columns to scale
      scalingRelevantMappings <- listOfAesthetics[get("scalingRelevant") >= 1]$aesthetic %>%
        intersect(names(self$mapping))

      # get Limits
      for (ax in c("primary", "secondary")) {
        oldLimits <- switch(ax,
          "primary" = self$ylimits,
          "secondary" = self$y2limits
        )
        if (is.null(oldLimits) || any(is.na(oldLimits))) {
          ylimits <- c()
          for (aesthetic in scalingRelevantMappings) {
            tmpData <- switch(ax,
              "primary" = self$data %>%
                dplyr::filter(!!self$mapping[["y2axis"]] == FALSE),
              "secondary" = self$data %>%
                dplyr::filter(!!self$mapping[["y2axis"]] == TRUE)
            )
            yData <- private$getDataForAesthetic(
              aesthetic,
              data = tmpData,
              stopIfNull = FALSE
            )
            if (!is.null(yData)) ylimits <- range(c(ylimits, yData), na.rm = TRUE)
          }
          if (is.null(oldLimits)) {
            newLimits <- ylimits
          } else {
            newLimits <- oldLimits
            newLimits[is.na(oldLimits)] <- ylimits[is.na(oldLimits)]
          }

          if (ax == "primary") {
            self$ylimits <- newLimits
          } else {
            self$y2limits <- newLimits
          }
        }
      }

      return(invisible(self))
    }
  )
)
