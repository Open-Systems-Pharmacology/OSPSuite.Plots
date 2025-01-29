#' Create a Forest Plot
#'
#' This function generates a forest plot with optional faceting and a corresponding table.
#'
#' @param plotData A data.table containing the data to be plotted. Must include columns specified in `yFacetColumns`, `xFacetColumn`, `tableColumns`, and others.
#' @param yColumn A character string specifying the name of the column to be used for the y-axis.
#' @param xLabel A string representing the label for the x-axis.
#' @param yFacetColumns A character vector of column names used for faceting on the y-axis. Can be NULL or of length up to 2.
#' @param xFacetColumn A character string specifying the column name for the x-axis facet. Must be of length 1 or NULL.
#' @param xscale A character string indicating the scale type for the x-axis. Options are "linear" or "log".
#' @param xscale.args A list of additional arguments for customizing the x-axis scale.
#' @param tableColumns A character vector of column names to be included in the table.
#' @param tableLabels A character vector of labels corresponding to `tableColumns`.
#' @param labelWrapWidth A numeric value specifying the width for label wrapping in facets.
#' @param digitsToRound An integer specifying the number of digits to round in the table.
#' @param digitsToShow An integer specifying the number of digits to display in the table.
#' @param withTable A logical flag indicating whether to include the table in the output. Defaults to TRUE if `xFacetColumn` is not NULL.
#'
#' @return A combined plot object containing the forest plot and the table (if applicable).
#' @export
plotForest <- function(plotData,
                       yColumn,
                       xLabel,
                       yFacetColumns = NULL,
                       xFacetColumn = NULL,
                       xscale = c('linear','log'),
                       xscale.args = list(),
                       tableColumns = c('x', 'xMin', 'xMax'),
                       tableLabels = c('Ratio', '90%\nCI lower', '90%\nCI upper'),
                       labelWrapWidth = 10,
                       digitsToRound = 2,
                       digitsToShow = 2,
                       withTable = is.null(xFacetColumn)
) {

  # Input checks
  checkmate::assertDataTable(plotData)
  xscale <- match.arg(xscale, c('linear', 'log'))
    checkmate::assertCharacter(xFacetColumn, null.ok = TRUE, len = 1)
  checkmate::assertCharacter(yFacetColumns, null.ok = TRUE, max.len = 2)
  checkmate::assertCharacter(tableColumns)
  checkmate::assertCharacter(tableLabels, len = length(tableColumns))
  checkmate::assertNames(names(plotData),
                         must.include = c(yFacetColumns, xFacetColumn, tableColumns,
                                          yColumn, 'dataType', 'x', 'xMin', 'xMax'))
  checkmate::assertIntegerish(digitsToRound, lower = 0, len = 1)
  checkmate::assertIntegerish(digitsToShow, lower = 0, len = 1)
  checkmate::assertString(xLabel)
  checkmate::assertNumeric(labelWrapWidth, lower = 0)
  checkmate::assertFlag(withTable)

  plotObject <-
    createPlotObject(plotData = plotData,
                     yColumn = yColumn,
                     xscale = xscale,
                     xscale.args = xscale.args,
                     xLabel = xLabel,
                     yFacetColumns = yFacetColumns,
                     xFacetColumn = xFacetColumn,
                     labelWrapWidth = labelWrapWidth)

  combinedPlot <- CombinedPlot$new(plotObject = plotObject)

  # Create table data and object only if xFacetColumn is NULL
  if (is.null(xFacetColumn) & withTable) {
    tableData <- createTableData(plotData = plotData,
                                 tableColumns = tableColumns,
                                 tableLabels = tableLabels)
    combinedPlot$tableObject <- createTableObject(tableData = tableData,
                                     yColumn = yColumn,
                                     digitsToRound = digitsToShow,
                                     digitsToShow = digitsToRound,
                                     yFacetColumns = yFacetColumns)
  } else {
    if (!is.null(xFacetColumn) & withTable) {
      warning('tables will be only added if xFacetColumn is NULL')
    }
  }

  return(combinedPlot)
}

#' Create the Plot Object
#'
#' This function generates the main plot object for the forest plot.
#'
#' @param plotData A data.table containing the data to be plotted.
#' @param yColumn A character string specifying the name of the column to be used for the y-axis.
#' @param xscale A character string indicating the scale type for the x-axis.
#' @param xscale.args A list of additional arguments for customizing the x-axis scale.
#' @param xLabel A string representing the label for the x-axis.
#' @param yFacetColumns A character vector of column names used for faceting on the y-axis.
#' @param xFacetColumn A character string specifying the column name for the x-axis facet.
#' @param labelWrapWidth A numeric value specifying the width for label wrapping in facets.
#'
#' @return A ggplot object representing the main plot.
#' @keywords internal
createPlotObject <- function(plotData, yColumn, xscale, xscale.args, xLabel, yFacetColumns, xFacetColumn, labelWrapWidth) {
  # Generate the facet formula if yFacetColumns are provided
  facetFormula <- if (!is.null(yFacetColumns) && length(yFacetColumns) > 0) {
    if (!is.null(xFacetColumn) && length(xFacetColumn) > 0){
      as.formula(paste(paste(yFacetColumns, collapse = " + "), "~", xFacetColumn))
    } else {
      as.formula(paste(paste(yFacetColumns, collapse = " + "), "~."))
    }
  } else {
    NULL
  }

  mappedData = ospsuite.plots::MappedData$new(
    data = plotData,
    mapping = aes(
      y = get(yColumn),
      x = x,
      xmin = xMin,
      xmax = xMax,
      color = dataType,
      fill = dataType,
      shape = dataType
    ),
    yscale = 'linear',
    xscale = xscale
  )

  plotObject <- ospsuite.plots::initializePlot(mappedData) +
    geom_pointrange(position = position_dodge(width = 1)) +
    (if (!is.null(facetFormula)) {
      facet_nested(facetFormula,
                   switch = 'y',
                   scales = 'free',
                   space = 'free_y',
                   labeller = label_wrap_gen(width = labelWrapWidth),
                   nest_line = element_line(color = 'black'),
                   resect = unit(1, units = 'lines'))
    }) +
    labs(y = '', x = xLabel) +
    theme(panel.spacing = unit(0, "lines"),
          panel.border = element_rect(colour = 'grey'),
          axis.line = element_line(color = 'black'),
          strip.text.y.left = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
          strip.background.y = element_rect(fill = NA, color = NA),
          strip.text.x = element_text(hjust = 0, vjust = 1, angle = 0),
          strip.background.x = element_rect(fill = NA, color = NA),
          strip.placement = "outside")

  plotObject <- ospsuite.plots::addXscale(plotObject = plotObject,
                                          xscale = xscale,
                                          xscale.args = xscale.args)

  return(plotObject)
}

#' Combine Plot and Table
#'
#' This function combines the plot and the table into a single output.
#'
#' @param plotObject A ggplot object representing the main plot.
#' @param tableObject A ggplot object representing the table.
#' @param relWidths A numeric vector of length 2 specifying the relative widths of the plot and table.
#'
#' @return A combined ggplot object containing both the plot and the table.
#' @keywords internal
createTableData <- function(plotData, tableColumns, tableLabels) {
  tableData <- plotData %>%
    tidyr::pivot_longer(cols = all_of(tableColumns), names_to = '.valueType', values_to = '.value')

  tableData$.valueType <- factor(tableData$.valueType,
                                 levels = tableColumns,
                                 labels = tableLabels,
                                 ordered = TRUE)

  return(tableData)
}

#' Create the Table Object
#'
#' This function generates the table plot object.
#'
#' @param tableData A data.table containing the data to be plotted in the table.
#' @param digitsToRound An integer specifying the number of digits to round in the table.
#' @param digitsToShow An integer specifying the number of digits to display in the table.
#' @param yFacetColumns A character string specifying the column name for the x-axis facet.
#'
#' @return A ggplot object representing the table.
#' @keywords internal
createTableObject <- function(tableData, yColumn, digitsToRound, digitsToShow, yFacetColumns) {
  facetFormula <- if ('dataType' %in% names(tableData) &
                      length(unique(tableData$dataType)) > 1) {
    as.formula(paste(paste(yFacetColumns, collapse = " + "), "~dataType"))
  } else {
    as.formula(paste(paste(yFacetColumns, collapse = " + "), "~."))
  }

  mappedData = ospsuite.plots::MappedData$new(
    data = tableData,
    mapping = aes(),
    yscale = 'linear',
    xscale = 'linear'
  )

  tableObject <- ospsuite.plots::initializePlot(mappedData) +
    geom_text(aes(y = get(yColumn),
                  x = .valueType,
                  label = formattable::formattable(
                    round(.value, digitsToRound),
                    digits = digitsToShow, format = "f")),
              size = 3) +
    (if (!is.null(facetFormula)) {
      facet_grid(facetFormula, scales = 'free_y', space = 'free_y')
    }) +
    scale_x_discrete(position = 'top') +
    labs(y = '', x = '') +
    theme(panel.spacing = unit(0, "lines"),
          panel.border = element_rect(colour = 'grey'),
          panel.grid = element_blank(),
          axis.text.x = element_text(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          strip.text.y = element_blank(),
          strip.placement = "outside")

  return(tableObject)
}

#' Combine Plot and Table
#'
#' This function combines the plot and the table into a single output.
#'
#' @param plotObject A ggplot object representing the main plot.
#' @param tableObject A ggplot object representing the table.
#' @param relWidths A numeric vector of length 2 specifying the relative widths of the plot and table.
#'
#' @return A combined ggplot object containing both the plot and the table.
#' @keywords internal
combinePlots <- function(plotObject, tableObject, relWidths) {

  # Check the current legend position
  if (theme_get()$legend.position == "right") {
    # Change legend position to top if table is included
    plotObject <- plotObject + theme(legend.position = "top")
  }

  cowplot::plot_grid(
    plotObject,
    tableObject,
    nrow = 1,
    axis = "tb",
    align = 'h',
    rel_widths = relWidths
  )
}
