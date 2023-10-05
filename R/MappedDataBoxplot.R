# TimeProfile ----------------
#' @title object to mappe data for boxplots
#' @description  R6 class for mapping variable to `data`
#' @export
#' @family MappedData classes
MappedDataBoxplot <- R6::R6Class( # nolint
  "MappedDataBoxplot",
  inherit = MappedData,
  public = list(
    #' @field scale of x axis
    xscale = NULL,
    #' @field arguments for scale of x axis
    xscale.args = NULL,
    #' @field Flag indicates if x is mapped
    hasXmapping = NULL,
    #'
    #' @param data data.frame used for mapping
    #' @param mapping list of aesthetic mappings
    #' @param groupAesthetics vector of aesthetics, which are used for columns mapped with group,
    #'            use of group aesthetics triggers second axis after simulation layers
    #' @param direction direction of plot either "x" or "y"
    #' @param scaleOfDirection  scale of direction, either "linear" or "log"
    #' @param isObserved Flag if TRUE mappings mdv, lloq, error and error_relative are evaluated
    #'
    #' @description Create a new `MappedDataBoxplot` object
    #'
    #' @return MappedDataBoxplot class object
    initialize = function(data,
                          mapping,
                          groupAesthetics = NULL,
                          direction = "y",
                          scaleOfDirection = "linear",
                          isObserved = TRUE,
                          xscale,
                          xscale.args) {
      super$initialize(
        data = data,
        mapping = mapping,
        groupAesthetics = groupAesthetics,
        direction = direction,
        isObserved = isObserved,
        scaleOfDirection = scaleOfDirection
      )
      # check if one dimensional in x
      self$hasXmapping <- ("x" %in% names(self$mapping))
      # check scale of x axis
      private$checkXscale(xscale = xscale)
    }
  ),
  ## active -------
  #' @field mapping for box whisker plot
  active = list(
    boxwhiskerMapping = function() {
      if (self$hasXmapping) {
        return(aes())
      } else {
        return(aes(x = " "))
      }
    }
  ),
  private = list(
    checkXscale = function(xscale, xscale.args) {
      if (self$hasXmapping) {
        # set breaks explicitly for usage in function getBoxWhiskerLimits
        tmp <- private$getDataForAesthetic(aesthetic = "x")
        if (is.factor(tmp)) {
          if (xscale %in% c("linear", "log")) {
            stop('constinuous x scale is not possible for factors, please select "discrete"')
          }
          xscale <- "discrete"
        } else {
          if (is.numeric(tmp)) {
            if (xscale == "discrete") {
              stop('discrete x scale is not possible for continuous data. Select "linear" or "log" or convert data to factor')
            }
            if (xscale == "auto") {
              xscale <- "linear"
            }
          } else {
            xscale <- "discrete"
          }
        }
      } else {
        xscale <- "discrete"
      }

      self$xscale <- xscale

      return(invisible(NULL))
    }
  )
)
