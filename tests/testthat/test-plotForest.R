oldDefaults <- ospsuite.plots::setDefaults()

# Sample data for testing, similar to the vignette
histData <- exampleDataCovariates %>%
  dplyr::filter(SetID == "DataSet1") %>%
  dplyr::select(c("ID", "Country", "Age", "AgeBin", "Obs", "Pred")) %>%
  melt(
    id.vars = c("ID", "Country", "Age", "AgeBin"),
    value.name = "value",
    variable.name = "DataType"
  )

plotData <- histData[, .(Mean = mean(value), SD = sd(value)),
  by = c("Country", "AgeBin", "DataType")
]

# Unit tests for plotForest function with vdiffr
test_that("plotForest function generates correct plots", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  # Basic plot test
  plotObject <- plotForest(
    plotData = plotData,
    mapping = aes(x = Mean, error = SD, y = AgeBin, groupby = DataType),
    xLabel = "Mean",
    yFacetColumns = "Country",
    tableColumns = c("Mean", "SD"),
    tableLabels = c("Mean", "SD")
  )

  # Visual test using vdiffr
  vdiffr::expect_doppelganger("basic_forest_plot", plotObject)

  # Plot without table
  plotObject_no_table <- plotForest(
    plotData = plotData,
    mapping = aes(x = Mean, error = SD, y = AgeBin, groupby = DataType),
    xLabel = "Mean",
    yFacetColumns = "Country",
    tableColumns = c("Mean", "SD"),
    tableLabels = c("Mean", "SD"),
    withTable = FALSE
  )

  # Visual test using vdiffr
  vdiffr::expect_doppelganger("forest_plot_no_table", plotObject_no_table)

  # Faceting by DataType
  plotObject_facet_data_type <- plotForest(
    plotData = plotData,
    mapping = aes(x = Mean, error = SD, y = AgeBin),
    xLabel = "Mean",
    yFacetColumns = "Country",
    xFacetColumn = "DataType",
    tableColumns = c("Mean", "SD"),
    tableLabels = c("Mean", "SD"),
    withTable = FALSE
  )

  # Visual test using vdiffr
  vdiffr::expect_doppelganger("facet_forest_plot", plotObject_facet_data_type)

  # Test for valid output when xFacetColumn is provided
  plotObject_with_facet <- plotForest(
    plotData = plotData,
    mapping = aes(x = Mean, error = SD, y = AgeBin),
    xLabel = "Mean",
    yFacetColumns = "Country",
    xFacetColumn = "DataType",
    tableColumns = c("Mean", "SD"),
    tableLabels = c("Mean", "SD")
  )

  # Visual test using vdiffr
  vdiffr::expect_doppelganger("facet_forest_plot_with_data_type", plotObject_with_facet)
})

# Additional test cases
test_that("plotForest handles edge cases and invalid inputs", {
  # Test for missing required columns in plotData
  invalid_data <- data.table(
    Country = c("USA", "Canada"),
    AgeBin = c("18-25", "26-35"),
    Mean = c(5, 10)
  )

  expect_error(
    plotForest(
      plotData = invalid_data,
      mapping = aes(x = Mean, error = SD, y = AgeBin, groupby = DataType),
      xLabel = "Mean",
      yFacetColumns = "Country",
      tableColumns = c("Mean", "SD"),
      tableLabels = c("Mean", "SD")
    ),
    "must include"
  )

  # Test for invalid xscale argument
  expect_error(
    plotForest(
      plotData = plotData,
      mapping = aes(x = Mean, error = SD, y = AgeBin, groupby = DataType),
      xLabel = "Mean",
      yFacetColumns = "Country",
      xscale = "invalid_scale", # Invalid scale
      tableColumns = c("Mean", "SD"),
      tableLabels = c("Mean", "SD")
    )
  )

  # Test for missing yFacetColumns
  expect_no_error(
    plotForest(
      plotData = plotData,
      mapping = aes(x = Mean, error = SD, y = AgeBin, groupby = DataType),
      xLabel = "Mean",
      yFacetColumns = NULL, # NULL should be valid, but check for warnings
      tableColumns = c("Mean", "SD"),
      tableLabels = c("Mean", "SD")
    )
  )

  # Test for empty plotData
  empty_data <- data.table()
  expect_error(
    plotForest(
      plotData = empty_data,
      mapping = aes(x = Mean, error = SD, y = AgeBin, groupby = DataType),
      xLabel = "Mean",
      yFacetColumns = "Country",
      tableColumns = c("Mean", "SD"),
      tableLabels = c("Mean", "SD")
    )
  )
})

ospsuite.plots::resetDefaults(oldDefaults)
