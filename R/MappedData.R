#' @title MappedData
#' @description  R6 class for mapping `x` and `y` variable to `data`
#' @export
#' @family MappedData classes
MappedData <- R6::R6Class(
  "MappedData",
  public = list(
    #' @field data data.frame used for mapping
    data = NULL,
    #' @field mapping list of aesthetic mappings
    mapping = NULL,

    #' @param data data.frame used for mapping
    #' @param mapping list of aesthetic mappings
    #' @param groupAesthetics vector of aesthetics, which are used for columns mapped with group,
    #'            use of group aesthetics triggers second axis after simulation layers
    #' @param direction direction of plot either "x" or "y"
    #' @param scaleOfDirection  scale of direction, either "linear" or "log"
    #' @param isObserved Flag if TRUE mappings mdv, lloq, error and error_relative are evaluated
    #'
    #' @description Create a new `MappedData` object
    #' @return A new `MappedData` object
    initialize = function(data,
                          mapping,
                          groupAesthetics = NULL,
                          direction = "y",
                          scaleOfDirection = "linear",
                          isObserved = TRUE) {
      # Validation
      checkmate::assertClass(data, classes = "data.frame", null.ok = FALSE)
      checkmate::assertList(mapping,
        types = "quosure",
        names = "named",
        any.missing = FALSE
      )
      # listOfAesthetics is included in sysdata.rda
      checkmate::assertNames(x = names(mapping), subset.of = listOfAesthetics$aesthetic)
      checkmate::assertCharacter(groupAesthetics,
        any.missing = FALSE,
        null.ok = TRUE
      )
      checkmate::assertChoice(
        direction,
        choices = c("x", "y"),
        null.ok = FALSE,
        .var.name = ("direction of MappedData")
      )

      private$direction <- direction

      checkmate::assertChoice(scaleOfDirection, choices = c("linear", "log"))
      private$scaleOfDirection <- scaleOfDirection


      if (!is.null(groupAesthetics)) {
        checkmate::assertNames(
          groupAesthetics,
          subset.of = listOfAesthetics$aesthetic
        )
        private$groupAesthetics <-
          standardise_aes_names(groupAesthetics)
      }
      private$groupAesthetics <- groupAesthetics

      self$data <- data.frame(data) ## creates a copy
      self$mapping <- mapping


      if (isObserved) {
        # MDV is a Nonmem notation in which values with MDV==1 are removed
        # if a mdv column is mapped delete all entries with logical = TRUE
        private$adjustDataForMDV()


        # lloq values are matched
        private$adjustForLLOQMatch()

        # add ymin ymax aesthetic error and error_relativ
        private$translateErrorAestethics()
      }

      # delete data columns for aesthetics not used
      private$adjustMappingToExistent()

      # convert non factor integers to double
      private$convertIntegerToDouble()

      # transfer group to group aesthetics
      private$adjustGroupAesthetics()
    },

    #' filter possible aesthetics for a geom,
    #' check if mandatory are available
    #'
    #' @param geom  type of geomertic object
    #' @param geomAttributes additionally arguments for geom layer, will overwrite aesthetics
    #'
    #' @return list of axxepted mappings
    getAestheticsForGeom = function(geom,
                                    geomAttributes) {
      # Validation
      checkmate::assertNames(geom,
        subset.of = setdiff(
          unique(
            gsub(
              "_y", "",
              gsub("_x", "", names(listOfAesthetics))
            )
          ),
          c("aesthetic", "source", "scalingRelevant")
        )
      )


      # filter for accepted AES, exclude the ones included in geomAttributes and
      # take only the ones mapped by user
      # listOfAesthetics is included in sysdata.rda
      acceptedAes <- listOfAesthetics[get(paste0(geom, "_", private$direction)) >= 1]$aesthetic %>%
        setdiff(names(geomAttributes)) %>%
        intersect(names(self$mapping))

      # check for mandatory
      if (!all(listOfAesthetics[get(paste0(geom, "_", private$direction)) >= 2]$aesthetic
        %in% acceptedAes)) {
        return(NULL)
      } else {
        return(self$mapping[acceptedAes])
      }
    }
  ),
  ## active -------
  active = list(
    #' @field hasLLOQMatch `Flag` if TRUE data has matched lloq data
    hasLLOQMatch = function(value) {
      if (missing(value)) {
        return(private$LLOQMatch)
      }
      checkmate::assertFlag(value)
      private$LLOQMatch <- value %||% private$hasLLOQMatch
      return(invisible())
    },
    #' @field directionIsLogscale `Flag` if TRUE scale of direction is LOG
    directionIsLogscale = function() {
      return(private$scaleOfDirection == "log")
    },
    #' @field dataForPlot returns data used for plotting,
    #' may be adjusted in child classes (e.g. 2 axis in MappedDataTimeProfile)
    dataForPlot = function() {
      return(self$data)
    }
  ),
  ## private -------
  private = list(
    groupAesthetics = NULL,
    direction = NULL,
    LLOQMatch = FALSE,
    scaleOfDirection = NULL,
    #' check if aesthtic is available in data
    aestheticExists = function(aesthetic) {
      return(rlang::is_quosure(self$mapping[[aesthetic]]))
    },
    #' returns data column for aesthetic
    getDataForAesthetic = function(aesthetic,
                                   data = self$data,
                                   stopIfNull = TRUE) {
      dataCol <- getDataForAesthetic(
        aesthetic = aesthetic,
        data = data,
        mapping = self$mapping,
        stopIfNull = stopIfNull
      )


      return(dataCol)
    },
    #' adds and update mapping
    addOverwriteAes = function(newMaps) {
      self$mapping <- addOverwriteAes(
        newMaps = newMaps,
        mapping = self$mapping
      )

      return(invisible(self))
    },
    #' deletes data where mdv is 1
    #'
    adjustDataForMDV = function() {
      if (private$aestheticExists("mdv")) {
        checkmate::assertLogical(as.logical(private$getDataForAesthetic("mdv")),
          all.missing = FALSE,
          .var.name = "mdv mapping"
        )

        self$data <-
          self$data %>%
          dplyr::filter(!as.logical(!!self$mapping[["mdv"]]))
      }

      return(invisible(self))
    },
    #' adds new column isLLOQ.i and updtes Flag LLOQMatch
    adjustForLLOQMatch = function() {
      if (private$aestheticExists("lloq")) {
        checkmate::assertNames(
          names(self$data),
          disjunct.from = c("isLLOQ.i"),
          .var.name = "column names of observed data"
        )

        ## add new column
        self$data <- self$data %>%
          dplyr::mutate(isLLOQ.i = factor(!!self$mapping[[private$direction]] < !!self$mapping[["lloq"]],
            ordered = TRUE
          ))

        ## add or overwrite mapping alpha
        private$addOverwriteAes(aes(alpha = isLLOQ.i))

        ## add  intercept mapping
        private$addOverwriteAes(eval(parse(
          text = paste0(
            "aes(",
            paste0(private$direction, "intercept"),
            " = ",
            rlang::quo_get_expr(self$mapping[["lloq"]]),
            ")"
          )
        )))

        # set Flag for LLOQ check
        private$LLOQMatch <- TRUE
      }

      return(invisible(self))
    },
    #' adds new columns ymin and ymax if reequired
    translateErrorAestethics = function() {
      if (!private$aestheticExists(paste(private$direction, "min")) |
        !private$aestheticExists(paste(private$direction, "max"))) {
        errorType <-
          intersect(names(self$mapping), c("error", "error_relativ"))
        if (length(errorType) > 1) {
          stop(paste(
            "observed data mapping contains more then one error definition:",
            paste0(errorType, collapse = ", ")
          ))
        }

        if (length(errorType) == 1) {
          newMapping <- list()

          checkmate::assertNames(
            names(self$data),
            disjunct.from = c("error.min", "error.max"),
            .var.name = "column names of data"
          )


          if (!private$aestheticExists(paste(private$direction, "min"))) {
            if (errorType == "error") {
              self$data <- self$data %>%
                dplyr::mutate("error.min" = ifelse(!!self$mapping[[private$direction]] > !!self$mapping[[errorType]],
                  !!self$mapping[[private$direction]] - !!self$mapping[[errorType]],
                  !!self$mapping[[private$direction]]
                ))
            } else if (private$aestheticExists("error_relativ")) {
              self$data <- self$data %>%
                dplyr::mutate("error.min" = !!self$mapping[[private$direction]] / !!self$mapping[[errorType]])
            }
            newMapping <-
              c(newMapping, eval(parse(
                text = paste0(
                  "aes(",
                  private$direction,
                  "min = error.min)"
                )
              )))
          }

          if (!private$aestheticExists(paste(private$direction, "max"))) {
            if (errorType == "error") {
              self$data <- self$data %>%
                dplyr::mutate("error.max" = !!self$mapping[[private$direction]] + !!self$mapping[[errorType]])
            } else if (private$aestheticExists("error_relativ")) {
              self$data <- self$data %>%
                dplyr::mutate("error.max" = !!self$mapping[[private$direction]] * !!self$mapping[[errorType]])
            }

            newMapping <-
              c(newMapping, eval(parse(
                text = paste0(
                  "aes(",
                  private$direction,
                  "max = error.max)"
                )
              )))
          }
          private$addOverwriteAes(newMapping)
        }
      }

      return(invisible(self))
    },
    #' copy aesthetics "group", but only if not explicit set
    adjustGroupAesthetics = function() {
      if (!is.null(private$groupAesthetics)) {
        newMapping <- list()
        for (aesthetic in private$groupAesthetics) {
          if (!private$aestheticExists(aesthetic)) {
            newMapping[[aesthetic]] <- self$mapping$group
          }
        }
        private$addOverwriteAes(newMapping)
      }

      return(invisible(self))
    },
    #' deletes mappings which do not exists in data
    adjustMappingToExistent = function() {
      for (aesthetic in names(self$mapping)) {
        tmp <- private$getDataForAesthetic(aesthetic,
          stopIfNull = FALSE
        )
        if (is.null(tmp)) {
          self$mapping[[aesthetic]] <- NULL
        }
      }
      return(invisible(self))
    },
    #' converts Integer columns, which are no factors to double
    convertIntegerToDouble = function() {
      for (aesthetic in names(self$mapping)) {
        tmp <- private$getDataForAesthetic(aesthetic,
          stopIfNull = FALSE
        )
        if (!is.null(tmp) &&
          !is.factor(tmp) &&
          is.integer(tmp)) {
          self$data %>%
            dplyr::mutate(!!self$mapping[[aesthetic]] := as.double(!!self$mapping[[aesthetic]]))
        }
      }
      return(invisible(self))
    }
  )
)

# TimeProfile ----------------
#' @title MappedDataTimeProfile
#' @description  R6 class for mapping `x` and `y` variable to `data`
#' @export
#' @family MappedData classes
MappedDataTimeProfile <- R6::R6Class(
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
        dataScaled <- dataScaled %>%
          dplyr::mutate(!!self$mapping[[aesthetic]] :=
            funScale(!!self$mapping[[aesthetic]]))
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
