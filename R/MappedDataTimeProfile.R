# TimeProfile ----------------
#' @title MappedDataTimeProfile
#' @description  R6 class for mapping variable to `data`
#' @export
#' @family MappedData classes
MappedDataTimeProfile <- R6::R6Class( # nolint
  "MappedDataTimeProfile",
  inherit = MappedData,
  public = list(
    #' @field y2limits double vector limits of secondary y axis
    y2limits = NULL,
    #' @param data data.frame used for mapping
    #' @param mapping list of aesthetic mappings
    #' @param groupAesthetics vector of aesthetics, which are used for columns mapped with aesthetic `groupby` ,
    #'            use of group aesthetics triggers second axis after simulation layers
    #' @param groupOrder labels and order for group aesthetic
    #' @param direction direction of plot either "x" or "y"
    #' @param isObserved A `boolean` if TRUE mappings mdv, lloq are evaluated
    #' @param xscale = scale of x-axis
    #' @param scaleOfPrimaryAxis  scale of direction, either "linear" or "log"
    #' @param scaleOfSecondaryAxis either 'linear' or 'log'
    #' @param xlimits limits for x-axis (may be NULL)
    #' @param ylimits limits for primary axis (may be NULL)
    #' @param y2limits limits for secondary axis (may be NULL)
    #'
    #' @description Create a new `MappedDataTimeProfile` object
    #' @return A new `MappedDataTimeProfile` object
    initialize = function(data,
                          mapping,
                          groupAesthetics = NULL,
                          groupOrder = NULL,
                          direction = "y",
                          isObserved = TRUE,
                          xlimits = NULL,
                          ylimits = NULL,
                          xscale = AxisScales$linear,
                          scaleOfPrimaryAxis = AxisScales$linear,
                          scaleOfSecondaryAxis = AxisScales$linear,
                          y2limits = NULL) {
      super$initialize(
        data = data,
        mapping = mapping,
        groupAesthetics = groupAesthetics,
        groupOrder = groupOrder,
        direction = direction,
        isObserved = isObserved,
        xlimits = NULL,
        ylimits = NULL,
        xscale = xscale,
        yscale = scaleOfPrimaryAxis
      )

      checkmate::assertChoice(scaleOfPrimaryAxis, choices = c(AxisScales$linear, AxisScales$log))
      private$scaleOfPrimaryAxis <- scaleOfPrimaryAxis

      # check for secondaryAxis
      if ("y2axis" %in% names(self$mapping)) {
        checkmate::assertLogical(private$getDataForAesthetic(aesthetic = "y2axis"),
          any.missing = FALSE, .var.name = "y2axis",
        )

        private$secondaryAxisAvailable <- any(private$getDataForAesthetic("y2axis"))

        private$addOverwriteAes(newMaps = aes(y2 = y2))
      } else {
        private$secondaryAxisAvailable <- FALSE
      }

      # convert scale aesthics to
      private$checkForCallAesthetics()

      # set fields for secondary Axis
      if (private$secondaryAxisAvailable) {
        checkmate::assertChoice(scaleOfSecondaryAxis,
          choices = c(AxisScales$linear, AxisScales$log),
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
        private$setyLimits()
      }

      return(invisible(self))
    },
    #' scales data for secondary axis and updates `secAxis`
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
      if (private$scaleOfPrimaryAxis == AxisScales$log) {
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
      if (private$scaleOfSecondaryAxis == AxisScales$log) {
        checkmate::assertDouble(log(y2limits), finite = TRUE)
      }

      # split data into two data sets
      dataUnscaled <- self$data %>%
        dplyr::filter(!!self$mapping[["y2axis"]] == FALSE)

      dataScaled <- self$data %>%
        dplyr::filter(!!self$mapping[["y2axis"]] == TRUE)

      # get Scaling function
      if (private$scaleOfSecondaryAxis == AxisScales$linear) {
        if (private$scaleOfPrimaryAxis == AxisScales$linear) {
          offsetlin1 <- ylimits[1]
          deltalin1 <- diff(ylimits)

          offsetlin2 <- y2limits[1]
          deltalin2 <- diff(y2limits)

          funScale <- function(y2) {
            return((y2 - offsetlin2) / deltalin2 * deltalin1 + offsetlin1)
          }

          funScaleAxis <- function(ytrans) {
            return((ytrans - offsetlin1) / deltalin1 * deltalin2 + offsetlin2)
          }
        } else if (private$scaleOfPrimaryAxis == AxisScales$log) {
          offsetlin <- y2limits[1]
          deltalin <- diff(y2limits)

          offsetlog <- log(ylimits[1])
          deltalog <- diff(log(ylimits))

          funScale <- function(ylin) {
            return(exp((ylin - offsetlin) / deltalin * deltalog + offsetlog))
          }

          funScaleAxis <- function(yt) {
            return((log(yt) - offsetlog) / deltalog * deltalin + offsetlin)
          }
        }
      } else if (private$scaleOfSecondaryAxis == AxisScales$log) {
        if (private$scaleOfPrimaryAxis == AxisScales$linear) {
          offsetlin <- ylimits[1]
          deltalin <- diff(ylimits)

          offsetlog <- log(y2limits[1])
          deltalog <- diff(log(y2limits))

          funScale <- function(ylog) {
            return((log(ylog) - offsetlog) / deltalog * deltalin + offsetlin)
          }

          funScaleAxis <- function(ylin) {
            return(exp((ylin - offsetlin) / deltalin * deltalog + offsetlog))
          }
        } else if (private$scaleOfPrimaryAxis == AxisScales$log) {
          offsetlog1 <- log(ylimits[1])
          deltalog1 <- diff(log(ylimits))

          offsetlog2 <- log(y2limits[1])
          deltalog2 <- diff(log(y2limits))

          funScale <- function(y2) {
            return(exp((log(y2) - offsetlog2) / deltalog2 * deltalog1 + offsetlog1))
          }

          funScaleAxis <- function(yt) {
            return(exp((log(yt) - offsetlog1) / deltalog1 * deltalog2 + offsetlog2))
          }
        }
      }
      # get data columns to scale
      scalingRelevantMappings <-
        listOfAesthetics[which(listOfAesthetics$scalingRelevant >= 1), ]$aesthetic %>%
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
      y2scale.args[["transform"]] <- funScaleAxis
      if (private$scaleOfSecondaryAxis == AxisScales$log) {
        y2scale.args[["breaks"]] <- scales::breaks_log(5, base = 10)(self$y2limits)
      } else if (private$scaleOfSecondaryAxis == AxisScales$linear) {
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
    #' boolean for secondary axis
    #'
    #' @field requireDualAxis boolean, If TRUE secondary axis is required
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
        plotData <- private$dataScaled
      } else {
        plotData <- self$data
      }

      if (private$scaleOfPrimaryAxis == AxisScales$log) {
        # get data columns to check for value <= 0
        scalingRelevantMappings <-
          listOfAesthetics[which(listOfAesthetics$scalingRelevant >= 1), ]$aesthetic %>%
          intersect(names(self$mapping))

        for (aesthetic in scalingRelevantMappings) {
          yData <- private$getDataForAesthetic(
            aesthetic,
            data = plotData,
            stopIfNull = FALSE
          )
          if (!is.null(yData)) {
            tryCatch(
              {
                plotData <-
                  plotData %>%
                  dplyr::mutate(!!self$mapping[[aesthetic]] :=
                    ifelse(!!self$mapping[[aesthetic]] <= 0, NA,
                      !!self$mapping[[aesthetic]]
                    ))
              }, # nolint
              error = function(cond) {
                # it my not work for calls like aesthetic = y/dose
                # then ggplot will produce warnings if Values values are less then 0
              }
            )
          }
        }
      }

      return(plotData)
    }
  ),
  ## private -------
  private = list(
    scaleOfPrimaryAxis = AxisScales$linear,
    scaleOfSecondaryAxis = AxisScales$linear,
    secondaryAxisAvailable = NULL,
    dataScaled = NULL,
    .secAxis = NULL,
    # check for scalingRelevantMappings aesthtics which are calls. The have to be transferred to make scaleble
    checkForCallAesthetics = function() {
      scalingRelevantMappings <-
        listOfAesthetics[which(listOfAesthetics$scalingRelevant >= 1), ]$aesthetic %>%
        intersect(names(self$mapping))

      for (aesthetic in scalingRelevantMappings) {
        if (inherits(x = rlang::get_expr(self$mapping[[aesthetic]]), "call")) {
          aestheticCol <- paste0(aesthetic, ".i")
          checkmate::assertNames(
            names(self$data),
            disjunct.from = c("isLLOQ.i"),
            .var.name = "column names of observed data"
          )

          self$data[[aestheticCol]] <- private$getDataForAesthetic(aesthetic,
            data = self$data,
            stopIfNull = TRUE
          )

          private$addOverwriteAes(eval(parse(
            text = paste0(
              "aes(",
              aesthetic,
              " = ",
              aestheticCol,
              ")"
            )
          )))
        }
      }
    },
    #' adjust limits
    setyLimits = function() {
      # get data columns to scale
      scalingRelevantMappings <-
        listOfAesthetics[which(listOfAesthetics$scalingRelevant >= 1), ]$aesthetic %>%
        intersect(names(self$mapping))

      # get Limits
      for (ax in c("primary", "secondary")) {
        oldLimits <- switch(ax,
          "primary" = self$ylimits,
          "secondary" = self$y2limits
        )
        yScale <- switch(ax,
          "primary" = private$scaleOfPrimaryAxis,
          "secondary" = private$scaleOfSecondaryAxis
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
            if (!is.null(yData)) {
              if (yScale == AxisScales$log) {
                ylimits <- range(c(ylimits, yData[yData > 0]), na.rm = TRUE)
              } else {
                ylimits <- range(c(ylimits, yData), na.rm = TRUE)
              }
            }
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
