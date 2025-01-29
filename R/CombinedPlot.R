#' @title CombinedPlot
#' @docType class
#' @description
#' This class represents a combined plot object that includes a plot and an optional table.
#' It provides methods to get and set the plot and table objects, as well as to print the combined output.
#' @examples
#'\dontrun{
#'
#' # Create a new CombinedPlot instance
#' combinedPlotInstance <- CombinedPlot$new(plotObject <- myPlotObject, tableObject <- myTableObject)
#'
#' # Print the combined plot and table
#' print(combinedPlotInstance)
#' # or simply
#' combinedPlotInstance
#'}
#' @export
CombinedPlot <- R6::R6Class(
  "CombinedPlot",
  cloneable = TRUE,
  public = list(
    #' Initialize
    #' @param tableObject A ggplot object for the table.
    #' @param plotObject A ggplot object for the main plot.
    initialize = function(plotObject = ggplot(), tableObject = NULL) {
      self$plotObject <- plotObject
      self$tableObject <- tableObject
      self$relWidths <- c(4,1)
    },
    #' Combine the combined plot and table
    #'
    #' This method combines the plot and table into a single output and displays it.
    #' @return The combined plot
    combined = function() {
      if (is.null(private$.tableObject)) return(self$plotObject)

      # Check the current legend position
      if ((is.null(private$.plotObject$theme$legend.position) &&
           theme_get()$legend.position == "right") |
          (!is.null(private$.plotObject$theme$legend.position) &&
           private$.plotObject$theme$legend.position == "right")){
        # Change legend position to top if table is included
        private$.plotObject <- private$.plotObject + theme(legend.position = "top")
      }
      return(cowplot::plot_grid(
        self$plotObject,
        self$tableObject,
        nrow = 1,
        axis = "tb",
        align = 'h',
        rel_widths = self$relWidths
      ))
    },
    #' Print the combined plot and table
    #'
    #' This method overrides the default print function to display the combined output.
    #' @return The combined plot
    print = function() {
      print(self$combined())
    }
  ),
  active = list(
    #' @field plotObject A ggplot object representing the main plot.
    plotObject = function(value) {
      if (missing(value)) {
        return(private$.plotObject)  # Get the plotObject
      } else {
        checkmate::assertClass(value, "gg")
        private$.plotObject <- value  # Set the plotObject
      }
    },
    #' @field tableObject A ggplot object representing the table.
    tableObject = function(value) {
      if (missing(value)) {
        return(private$.tableObject)  # Get the tableObject
      } else {
        checkmate::assertClass(value, "gg",null.ok = TRUE)
        private$.tableObject <- value  # Set the tableObject
      }
    },
    #' @field relWidths  A numeric vector of length 2 specifying the relative widths of the plot and table.
    relWidths = function(value) {
      if (missing(value)) {
        return(private$.relWidths)  # Get the relative width
      } else {
        checkmate::assertNumeric(value,lower = 0,len = 2)
        private$.relWidths <- value  # Set the  relative width
      }
    }
  ),
  private = list(
    .plotObject = NULL,
    .tableObject = NULL,
    .relWidths = NULL
  )
)
