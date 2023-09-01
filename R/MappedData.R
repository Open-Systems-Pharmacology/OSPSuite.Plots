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

        # add ymin ymax aesthetic error and error_relative
        private$translateErrorAestethics()
      }

      # convert non factor integers to double
      private$convertIntegerToDouble()

      # transfer group to group aesthetics
      private$adjustGroupAesthetics()
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
    #' check if aesthetic is available in data
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
    #' adds new column `isLLOQ.i` and updates Flag `LLOQMatch`
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
    #' adds new columns ymin and ymax if required
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
            } else if (private$aestheticExists("error_relative")) {
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
            } else if (private$aestheticExists("error_relative")) {
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
