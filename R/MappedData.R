#' @title MappedData
#' @description  R6 class for mapping  variables to `data`
#' @export
#' @family MappedData classes
MappedData <- R6::R6Class( # nolint
  "MappedData",
  public = list(
    #' @field data data.frame used for mapping
    data = NULL,
    #' @field mapping list of aesthetic mappings
    mapping = NULL,
    #' @field dimensions list with dimensions of mapping
    dimensions = list(),
    #' @field units list with dimensions of mapping
    units = list(),
    #' @field columnClasses list with class of mapped columns
    columnClasses = list(),
    #' @field xlimits double vector limits of primary y axis
    xlimits = NULL,
    #' @field ylimits double vector limits of primary y axis
    ylimits = NULL,
    #' @field hasResidualMapping flag to indicate if residual mapping is used
    hasResidualMapping = FALSE,
    #' @field residualLabel label for residuals
    residualLabel = NULL,

    #' @param data data.frame used for mapping
    #' @param mapping list of aesthetic mappings
    #' @param xScale scale of x-axis either 'linear' or 'log'
    #' @param yScale scale of y-axis either 'linear' or 'log'
    #' @param groupAesthetics vector of aesthetics, which are used for columns mapped with `groupby`
    #' @param groupOrder labels and order for group aesthetic
    #' @param direction direction of plot either "x" or "y"
    #' @param isObserved A `boolean `if TRUE mappings mdv, lloq
    #' @param xlimits limits for x-axis (may be NULL)
    #' @param ylimits limits for y-axis (may be NULL)
    #' @param residualScale scale of x residuals
    #' @param residualAesthetic aesthetic used for mapping residuals
    #'
    #' @description Create a new `MappedData` object
    #' @return A new `MappedData` object
    initialize = function(data,
                          mapping,
                          xScale,
                          yScale,
                          groupAesthetics = NULL,
                          groupOrder = NULL,
                          direction = "y",
                          isObserved = TRUE,
                          xlimits = NULL,
                          ylimits = NULL,
                          residualScale = NULL,
                          residualAesthetic = "y") {
      # Validation
      checkmate::assertClass(data, classes = "data.frame", null.ok = FALSE)
      checkmate::assertList(mapping,
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

      checkmate::assertDouble(
        xlimits,
        sorted = TRUE,
        any.missing = TRUE,
        len = 2,
        unique = TRUE,
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

      # delete columns with NA
      data <- data |>
        dplyr::select(names(data)[sapply(data, function(x) !all(is.na(x)))])

      self$data <- data.frame(data) ## creates a copy
      self$mapping <- mapping

      # add group order
      private$addGroupOrder(groupOrder)

      if (!is.null(groupAesthetics)) {
        checkmate::assertNames(
          groupAesthetics,
          subset.of = listOfAesthetics$aesthetic
        )
        private$groupAesthetics <-
          standardise_aes_names(groupAesthetics)
      }
      private$groupAesthetics <- unique(c(private$groupAesthetics, "group"))


      if (isObserved) {
        # MDV is a Nonmem notation in which values with MDV==1 are removed
        # if a mdv column is mapped delete all entries with logical = TRUE
        private$adjustDataForMDV()


        # lloq values are matched
        private$adjustForLLOQMatch()
      }
      # add ymin ymax aesthetic error and error_relative
      private$translateErrorAestethics()

      # convert non factor integers to double
      private$convertIntegerToDouble()

      # transfer groupby to group aesthetics
      private$adjustGroupAesthetics()

      private$adjustForResidualMatch(
        residualScale = residualScale,
        residualAesthetic = residualAesthetic
      )

      # setLimits
      private$setLimits(xScale, yScale)
    },

    #' filter possible aesthetics for a geom,
    #' check if mandatory are available
    #'
    #' @param geom  type of geometric object
    #' @param geomAttributes additionally arguments for geom layer, will overwrite aesthetics
    #'
    #' @return list of accepted mappings
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
      acceptedAes <-
        listOfAesthetics[which(listOfAesthetics[[(paste0(geom, "_", private$direction))]] >= 1), ]$aesthetic |>
        setdiff(names(geomAttributes)) |>
        intersect(names(self$mapping))

      # check for mandatory
      if (!all(listOfAesthetics[which(listOfAesthetics[[(paste0(geom, "_", private$direction))]] >= 2), ]$aesthetic
        %in% acceptedAes)) {
        return(NULL)
      } else {
        return(structure(self$mapping[acceptedAes], class = "uneval"))
      }
    },
    #' adds list with dimension, units and column classes
    #'
    #'
    #' @param metaData A named list of information about `data` such as the `dimension` and `unit` of its variables.
    #'
    #' @return  updated `MappedData` object
    addMetaData = function(metaData) {
      for (aesthetic in names(self$mapping)) {
        tmp <- private$getDataForAesthetic(
          aesthetic = aesthetic,
          data = metaData2DataFrame(metaData),
          stopIfNull = FALSE
        )

        if (!is.null(tmp) & !is.function(tmp) & length(tmp) == 2) {
          self$dimensions[[aesthetic]] <- tmp[1]
          self$units[[aesthetic]] <- tmp[2]
        }

        tmp <- private$getDataForAesthetic(
          aesthetic = aesthetic,
          stopIfNull = FALSE
        )

        if (!is.null(tmp)) {
          if (is.factor(tmp)) {
            self$columnClasses[[aesthetic]] <- "factor"
          } else {
            self$columnClasses[[aesthetic]] <- class(tmp)
          }
        }
      }
      return(invisible(self))
    },
    #' check if unit of scale direction i s time and sets the breaks accordingly
    #'
    #' @param scaleArgs additional arguments passed on to scale function
    #' @param scaleDirection direction of axis either 'x' or 'y'
    #'
    #' @return `scaleArgs` with adjusted break function
    updateScaleArgumentsForTimeUnit = function(scaleArgs,
                                               scaleDirection = "x") {
      ## Validation
      checkmate::assertList(scaleArgs, null.ok = TRUE)

      # check if anything to do
      if (any(c("breaks", "labels") %in% names(scaleArgs))) {
        return(scaleArgs)
      }
      if (length(self$dimensions) == 0) {
        return(scaleArgs)
      }


      return(updateScaleArgumentsForTimeUnit(
        scaleArgs = scaleArgs,
        dimension = self$dimensions[[scaleDirection]],
        unit = self$units[[scaleDirection]]
      ))
    }
  ),
  ## active -------
  active = list(
    #' @field hasLLOQMatch `boolean` if TRUE data has matched lloq data
    hasLLOQMatch = function(value) {
      if (missing(value)) {
        return(private$LLOQMatch)
      }
      checkmate::assertFlag(value)
      private$LLOQMatch <- value %||% private$hasLLOQMatch
      return(invisible())
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
    #' check if aesthetic is available in data
    aestheticExists = function(aesthetic) {
      return(rlang::is_quosure(self$mapping[[aesthetic]]))
    },
    #' returns data column for aesthetic
    getDataForAesthetic = function(aesthetic,
                                   data = self$data,
                                   stopIfNull = TRUE) {
      dataCol <- tryCatch(
        {
          rlang::eval_tidy(
            expr = rlang::get_expr(self$mapping[[aesthetic]]),
            data = data,
            env = rlang::get_env(self$mapping[[aesthetic]])
          )
        },
        error = function(cond) {
          if (stopIfNull) {
            stop(paste("evaluation of aesthetic", aesthetic, "failed:", cond))
          } else {
            NULL
          }
        }
      )

      return(dataCol)
    },
    #' adds and update mapping
    addOverwriteAes = function(newMaps) {
      checkmate::assertList(newMaps, names = "named")

      self$mapping <-
        self$mapping[setdiff(names(self$mapping), names(newMaps))]

      self$mapping <-
        structure(c(self$mapping, newMaps), class = "uneval")

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
          self$data |>
          dplyr::filter(!as.logical(!!self$mapping[["mdv"]]))
      }

      return(invisible(self))
    },
    #' adds new column `isLLOQ.i` and updates boolean `LLOQMatch`
    adjustForLLOQMatch = function() {
      if (private$aestheticExists("lloq")) {
        checkmate::assertNames(
          names(self$data),
          disjunct.from = c("isLLOQ.i"),
          .var.name = "column names of observed data"
        )

        ## add new column
        self$data <- self$data |>
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

        # set boolean for LLOQ check
        private$LLOQMatch <- TRUE
      }

      return(invisible(self))
    },
    #' adds new columns `ymin` and `ymax` if required
    translateErrorAestethics = function() {
      if (!private$aestheticExists(paste(private$direction, "min")) |
        !private$aestheticExists(paste(private$direction, "max"))) {
        errorType <-
          intersect(names(self$mapping), c("error", "error_relative"))
        if (length(errorType) > 1) {
          stop(paste(
            "observed data mapping contains more then one error definition:",
            paste0(errorType, collapse = ", ")
          ))
        }

        if (length(errorType) == 1 &&
          !is.null(private$getDataForAesthetic(errorType,
            stopIfNull = FALSE
          ))) {
          newMapping <- list()

          checkmate::assertNames(
            names(self$data),
            disjunct.from = c("error.min", "error.max"),
            .var.name = "column names of data"
          )


          if (!private$aestheticExists(paste(private$direction, "min"))) {
            if (errorType == "error") {
              self$data <- self$data |>
                dplyr::mutate("error.min" = ifelse(!!self$mapping[[private$direction]] > !!self$mapping[[errorType]],
                  !!self$mapping[[private$direction]] - !!self$mapping[[errorType]],
                  !!self$mapping[[private$direction]]
                ))
            } else if (private$aestheticExists("error_relative")) {
              self$data <- self$data |>
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
              self$data <- self$data |>
                dplyr::mutate("error.max" = !!self$mapping[[private$direction]] + !!self$mapping[[errorType]])
            } else if (private$aestheticExists("error_relative")) {
              self$data <- self$data |>
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

          if (private$aestheticExists("error")) self$mapping$error <- NULL
          if (private$aestheticExists("error_relative")) self$mapping$error_relative <- NULL
        }
      }

      return(invisible(self))
    },
    #' copy aesthetics `groupby`, but only if not explicit set
    adjustGroupAesthetics = function() {
      if (!is.null(private$groupAesthetics)) {
        newMapping <- list()
        for (aesthetic in private$groupAesthetics) {
          if (!private$aestheticExists(aesthetic)) {
            newMapping[[aesthetic]] <- self$mapping$groupby

            tmp <- private$getDataForAesthetic(aesthetic,
              stopIfNull = FALSE
            )
            if (!is.null(tmp) &&
              !is.factor(tmp)) {
              self$data |>
                dplyr::mutate(!!self$mapping[[aesthetic]] := factor(!!self$mapping[[aesthetic]]))
            }
          }
        }
        private$addOverwriteAes(newMapping)
      }
      self$mapping$groupby <- NULL

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
          self$data <- self$data |>
            dplyr::mutate_at(vars(!!self$mapping[[aesthetic]]), as.double)
        }
      }
      return(invisible(self))
    },
    setLimits = function(xScale, yScale) {
      # get data columns to scale
      relevantMappings <- list(x = "x", y = "y")
      relevantMappings[[private$direction]] <- gsub(
        "y",
        private$direction,
        listOfAesthetics[which(listOfAesthetics$scalingRelevant >= 1), ]$aesthetic
      ) |>
        intersect(names(self$mapping))

      # get Limits
      for (ax in c(private$direction, setdiff(c("x", "y"), private$direction))) {
        oldLimits <- switch(ax,
          "x" = self$xlimits,
          "y" = self$ylimits
        )
        axisScale <- switch(ax,
          "x" = xScale,
          "y" = yScale
        )
        if (is.null(oldLimits) || any(is.na(oldLimits))) {
          ylimits <- c()

          for (aesthetic in relevantMappings[[ax]]) {
            yData <- private$getDataForAesthetic(
              aesthetic,
              data = self$data,
              stopIfNull = FALSE
            )

            if (!is.null(yData) && !is.function(yData)) {
              if (axisScale == AxisScales$log) yData <- yData[yData > 0]
              ylimits <- range(c(ylimits, yData), na.rm = TRUE)
            }
          }
          if (is.null(oldLimits)) {
            newLimits <- ylimits
          } else {
            newLimits <- oldLimits
            newLimits[is.na(oldLimits)] <- ylimits[is.na(oldLimits)]
          }

          if (ax == "x") {
            self$xlimits <- newLimits
          } else {
            self$ylimits <- newLimits
          }
        }
      }

      return(invisible(self))
    },
    #' adds new column `residuals.i`
    adjustForResidualMatch = function(residualScale,
                                      residualAesthetic) {
      if (is.null(residualScale)) {
        return(invisible(self))
      }
      if (private$aestheticExists("predicted") &
        private$aestheticExists("observed")) {
        checkmate::assertNames(
          names(self$data),
          disjunct.from = c("residuals.i"),
          .var.name = "column names of observed data"
        )

        if (!residualAesthetic %in% names(self$mapping)) {
          ## add new column
          if (residualScale == ResidualScales$log) {
            self$data <- self$data |>
              dplyr::mutate(residuals.i = log(!!self$mapping[["predicted"]]) - log(!!self$mapping[["observed"]]))
          } else if (residualScale == ResidualScales$linear) {
            self$data <- self$data |>
              dplyr::mutate(residuals.i = !!self$mapping[["predicted"]] - !!self$mapping[["observed"]])
          } else if (residualScale == ResidualScales$ratio) {
            self$data <- self$data |>
              dplyr::mutate(residuals.i = !!self$mapping[["observed"]] / !!self$mapping[["predicted"]])
          }


          # add mapping for residuals
          private$addOverwriteAes(eval(parse(
            text = paste0(
              "aes(",
              residualAesthetic,
              "= residuals.i)"
            )
          )))

          # set boolean
          self$hasResidualMapping <- TRUE

          self$residualLabel <-
            switch(residualScale,
              linear = "residuals\npredicted - observed",
              log = "residuals\nlog(predicted) - log(observed)",
              ratio = "observed/predicted"
            )
        }
      }

      # clean up
      self$mapping[["observed"]] <- NULL
      self$mapping[["predicted"]] <- NULL


      return(invisible(self))
    },
    #' factorize column for group to factor
    addGroupOrder = function(groupOrder) {
      if (is.null(groupOrder)) {
        return(invisible(self))
      }
      if (!private$aestheticExists("group") & !private$aestheticExists("groupby")) {
        stop('for mapping of observed to simulated aesthetic "group" or "groupby" is needed')
      }

      aesthetics <- intersect(names(self$mapping), c("group", "groupby"))

      tmp <- private$getDataForAesthetic(aesthetics[1],
        stopIfNull = FALSE
      )

      checkmate::assertNames(
        x = as.character(unique(tmp)),
        subset.of = groupOrder, .var.name = "Mapping vector"
      )

      # add new column as factor
      checkmate::assertNames(
        names(self$data),
        disjunct.from = c("groupBy.i"),
        .var.name = "column names of data"
      )

      self$data[["groupBy.i"]] <- factor(tmp, levels = groupOrder)
      self$data[order(self$data$groupBy.i), ]

      # adjust mapping
      for (aesthetic in aesthetics) {
        private$addOverwriteAes(eval(parse(
          text = paste0(
            "aes(",
            aesthetic,
            " = groupBy.i)"
          )
        )))
      }

      return(invisible(self))
    }
  )
)
