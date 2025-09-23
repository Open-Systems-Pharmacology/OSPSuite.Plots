# Boxplots ----------------
#' @title MappedDataBoxplot
#' @description R6 class for mapping variable to data for boxplot visualizations.
#' This class extends MappedData to provide specialized mapping functionality for box-and-whisker plots,
#' including handling of discrete and continuous x-axis scales and automatic grouping logic.
#' @examples
#' \dontrun{
#' # Create boxplot mapping with continuous x variable
#' boxplotData <- MappedDataBoxplot$new(
#'   data = myDataFrame,
#'   mapping = aes(x = dose, y = concentration),
#'   xscale = "linear"
#' )
#'
#' # Create boxplot mapping with categorical x variable
#' boxplotData <- MappedDataBoxplot$new(
#'   data = myDataFrame,
#'   mapping = aes(x = treatment_group, y = response),
#'   xscale = "discrete"
#' )
#' }
#' @export
#' @family MappedData classes
MappedDataBoxplot <- R6::R6Class( # nolint
  "MappedDataBoxplot",
  inherit = MappedData,
  public = list(
    #' @field xscale scale of x axis
    xscale = NULL,
    #' @field xscale.args arguments for scale of x axis
    xscale.args = NULL,
    #' @field hasXmapping boolean, if TRUE x is mapped
    hasXmapping = NULL,
    #'
    #' @param data data.frame used for mapping
    #' @param mapping list of aesthetic mappings
    #' @param groupAesthetics vector of aesthetics, which are used for columns mapped with aesthetic `groupby`
    #' @param direction direction of plot either "x" or "y"
    #' @param isObserved A `boolean` if TRUE mappings mdv, lloq, error and error_relative are evaluated
    #' @param xscale scale of x-axis either 'linear' or 'log'
    #' @param yscale scale of y-axis either 'linear' or 'log'
    #' @param xlimits limits for x-axis (may be NULL)
    #' @param ylimits limits for y-axis (may be NULL)
    #' @param residualScale scale of x residuals
    #' @param residualAesthetic aesthetic used for mapping residuals
    #'
    #' @description Create a new `MappedDataBoxplot` object
    #'
    #' @return `MappedDataBoxplot` class object
    initialize = function(data,
                          mapping,
                          groupAesthetics = NULL,
                          direction = "y",
                          isObserved = TRUE,
                          xlimits = NULL,
                          ylimits = NULL,
                          xscale = AxisScales$linear,
                          yscale = AxisScales$linear,
                          residualScale = NULL,
                          residualAesthetic = "y") {
      super$initialize(
        data = data,
        mapping = mapping,
        groupAesthetics = groupAesthetics,
        direction = direction,
        isObserved = isObserved,
        xlimits = xlimits,
        ylimits = ylimits,
        xscale = xscale,
        yscale = yscale,
        residualScale = residualScale,
        residualAesthetic = residualAesthetic
      )
      # check if one dimensional in x
      self$hasXmapping <- ("x" %in% names(self$mapping))
    },
    #' use Metadata to adjust binning of x-axis, and group aesthetic
    #'
    #' @param originalmapping mapping provided by user
    #' @param xscale either 'linear','log', 'discrete' or 'auto' (default) auto select linear for continuous data and discrete for categorical data
    #' @param xscale.args list of arguments passed to `ggplot2::scale_x_continuous()`, `ggplot2::scale_x_log10()` or
    #'    `ggplot2::scale_x_discrete()`
    #'
    #' @return adjusted `MappedDataBoxplot` class object
    doAdjustmentsWithMetaData = function(originalmapping,
                                         xscale,
                                         xscale.args) {
      if (is.null(self$columnClasses[["x"]])) {
        warning("No metaData available for x-axis")
        return(invisible(self))
      }
      # Validate input mapping structure
      checkmate::assertList(originalmapping, null.ok = FALSE)
      checkmate::assertCharacter(xscale, len = 1, null.ok = FALSE)
      checkmate::assertList(xscale.args, null.ok = TRUE)

      # Adjust group aesthetic based on x variable type and mapping requirements
      private$adjustGroupMapping(originalmapping = originalmapping)
      # Determine and validate appropriate x-axis scale based on data type
      private$checkXscale(xscale = xscale, xscale.args)

      return(invisible(self))
    }
  ),
  ## active -------
  #' @field boxwhiskerMapping mapping for box whisker plot
  active = list(
    boxwhiskerMapping = function() {
      # Return different mapping based on whether x variable is mapped
      if (self$hasXmapping) {
        # When x is mapped, use existing aesthetics (x variable drives grouping)
        return(aes())
      } else {
        # When no x mapping, create a single category for all data points
        return(aes(x = " "))
      }
    }
  ),
  private = list(
    checkXscale = function(xscale, xscale.args) {
      if (self$hasXmapping) {
        if (self$columnClasses[["x"]] == "factor") {
          if (xscale %in% c(AxisScales$linear, AxisScales$log)) {
            stop(paste0('continuous x scale is not possible for factors, please select "', AxisScales$discrete, '"'))
          }
          xscale <- AxisScales$discrete
        } else {
          if (self$columnClasses[["x"]] == "numeric") {
            if (xscale == AxisScales$discrete) {
              stop(paste0(
                'discrete x scale is not possible for continuous data. Select "',
                AxisScales$linear, '" or "', AxisScales$log, '" or convert data to factor'
              ))
            }
            if (xscale == "auto") {
              xscale <- AxisScales$linear
            }
          } else {
            xscale <- AxisScales$discrete
          }
        }
      } else {
        xscale <- AxisScales$discrete
      }

      self$xscale <- xscale

      return(invisible(NULL))
    },
    adjustGroupMapping = function(originalmapping) {
      if (self$hasXmapping) {
        # non numeric should  not have a group mapping,
        # so delete it if not explicitly set by user
        if (self$columnClasses[["x"]] != "numeric" &
          private$aestheticExists("group") &
          !("group" %in% names(originalmapping))) {
          self$mapping[["group"]] <- NULL
        }
        # numeric should have a group mapping
        if (self$columnClasses[["x"]] == "numeric" &
          !private$aestheticExists("group")) {
          newMapping <- list()
          newMapping[["group"]] <- self$mapping$x
          private$addOverwriteAes(newMapping)
        }
      }

      return(invisible(self))
    }
  )
)
