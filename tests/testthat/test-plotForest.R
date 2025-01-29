# setDefaults
oldDefaults <- ospsuite.plots::setDefaults()



# Create a sample data table for testing
create_sample_data <- function(dataTypeCount = 2, rowsPerType = 10) {
  dataTypes <- c("observed", "simulated")[seq(1,dataTypeCount)]
  plotData <- data.table(
    scenarioGroup = rep(c("Group with a verylong Name to wrap", "Group 2"), each = rowsPerType * dataTypeCount),
    scenarioShortName = rep(c("Short 1", "Short 2"), each = rowsPerType * dataTypeCount),
    outputPathId = rep(letters[1:5], times = rowsPerType * dataTypeCount / 5),
    parameter = rep(c("Type A", "Type B"), times = rowsPerType),
    dataType = rep(dataTypes, each = rowsPerType),
    x = runif(rowsPerType * dataTypeCount, 0, 100),
    xMin = runif(rowsPerType * dataTypeCount, 0, 50),
    xMax = runif(rowsPerType * dataTypeCount, 50, 100)
  )
  return(plotData)
}


test_that("plotForest generates expected plots", {

  # Test with multiple data types
  set.seed(123)
  plotData <- create_sample_data(dataTypeCount = 2)

  # Test without table
  vdiffr::expect_doppelganger("plotForest_with_table_multiple",
                      plotForest(plotData = plotData,
                                 yColumn = 'parameter',
                                 xLabel = "X Axis",
                                 withTable = FALSE)
  )

  # Test with table
  vdiffr::expect_doppelganger("plotForest_without_table_multiple",
                      plotForest(plotData = plotData,
                                 yColumn = 'parameter',
                                 xLabel = "X Axis",
                                 withTable = TRUE)
  )

  # Test with specific facets
  vdiffr::expect_doppelganger("plotForest_with_facets_multiple",
                      plotForest(plotData = plotData,
                                 yColumn = "parameter",
                                 xLabel = "X Axis",
                                 yFacetColumns = c("scenarioGroup", "scenarioShortName"),
                                 xFacetColumn = "outputPathId")
                      )

  # Test with specific facets and table
  vdiffr::expect_doppelganger("plotForest_with_facets_multiple",
                      plotForest(plotData = plotData,
                                 yColumn = "parameter",
                                 xLabel = "X Axis",
                                 yFacetColumns = c("scenarioGroup", "scenarioShortName"),
                                 xFacetColumn = NULL)
  )


  # Test with only one data type
  set.seed(123)
  plotDataSingleType <- create_sample_data(dataTypeCount = 1)

  # Test with specific facets
  vdiffr::expect_doppelganger("plotForest_with_facets_single",
                      plotForest(plotData = plotDataSingleType, yColumn = "parameter", xLabel = "X Axis",
                                 yFacetColumns = c("scenarioGroup", "scenarioShortName"),
                                 xFacetColumn = NULL)
                      )

  # Test with invalid xFacetColumn (should give warning)
  expect_error(
    plotForest(plotData = plotData,
                            yColumn = "parameter",
                            xLabel = "X Axis",
                            xFacetColumn = "invalidColumn", withTable = TRUE)
    )

  # Test with missing yColumn
  expect_error(plotForest(plotData, yColumn = "nonExistentColumn", xLabel = "X Axis"))

  # Test with invalid tableColumns
  expect_error(plotForest(plotData, yColumn = "parameter", xLabel = "X Axis", tableColumns = c("invalidColumn")))

  # Test with a very label wrap width
  vdiffr::expect_doppelganger("plotForest_label_wrap",
                      plotForest(plotData, yFacetColumns = c("scenarioGroup", "scenarioShortName"),
                                 yColumn = "parameter", xLabel = "X Axis", labelWrapWidth = 20)
                      )

  # Test relWidth
  combinedObject <- plotForest(plotData, yFacetColumns = c("scenarioGroup", "scenarioShortName"),
                               yColumn = "parameter", xLabel = "X Axis")
  combinedObject$relWidths <- c(1,1)
  vdiffr::expect_doppelganger("plotForest_label_wrap",
                              combinedObject
  )


})



ospsuite.plots::resetDefaults(oldDefaults)

