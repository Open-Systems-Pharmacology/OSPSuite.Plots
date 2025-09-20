# Test CombinedPlot R6 class

test_that("CombinedPlot initialization works correctly", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("R6")
  
  # Create test plot object
  test_plot <- ggplot2::ggplot() + ggplot2::geom_blank()
  test_table <- data.frame(Parameter = c("A", "B"), Value = c(1, 2))
  
  # Test basic initialization
  combined <- CombinedPlot$new(plotObject = test_plot)
  expect_r6_class(combined, "CombinedPlot")
  expect_equal(combined$plotObject, test_plot)
  expect_null(combined$tableObject)
  expect_equal(combined$relWidths, c(4, 1))
  
  # Test initialization with table
  combined_with_table <- CombinedPlot$new(plotObject = test_plot, tableObject = test_table)
  expect_equal(combined_with_table$plotObject, test_plot)
  expect_equal(combined_with_table$tableObject, test_table)
  
  # Test input validation
  expect_error(CombinedPlot$new(plotObject = "not_a_plot"))
  expect_error(CombinedPlot$new(plotObject = test_plot, tableObject = "not_a_dataframe"))
})

test_that("CombinedPlot active bindings work correctly", {
  skip_if_not_installed("ggplot2")
  
  test_plot <- ggplot2::ggplot() + ggplot2::geom_blank()
  test_plot2 <- ggplot2::ggplot() + ggplot2::geom_point(data = data.frame(x = 1, y = 1), aes(x = x, y = y))
  test_table <- data.frame(Parameter = c("A", "B"), Value = c(1, 2))
  
  combined <- CombinedPlot$new(plotObject = test_plot)
  
  # Test plotObject getter and setter
  expect_equal(combined$plotObject, test_plot)
  combined$plotObject <- test_plot2
  expect_equal(combined$plotObject, test_plot2)
  
  # Test tableObject getter and setter
  expect_null(combined$tableObject)
  combined$tableObject <- test_table
  expect_equal(combined$tableObject, test_table)
  combined$tableObject <- NULL
  expect_null(combined$tableObject)
  
  # Test relWidths getter and setter
  expect_equal(combined$relWidths, c(4, 1))
  combined$relWidths <- c(3, 2)
  expect_equal(combined$relWidths, c(3, 2))
  
  # Test validation in setters
  expect_error(combined$plotObject <- "not_a_plot")
  expect_error(combined$tableObject <- "not_a_dataframe")
  expect_error(combined$relWidths <- c(1, 2, 3)) # wrong length
  expect_error(combined$relWidths <- c(-1, 2)) # negative value
})

test_that("CombinedPlot combined method works correctly", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("cowplot")
  
  test_plot <- ggplot2::ggplot() + ggplot2::geom_blank()
  
  # Test without table returns plot directly
  combined <- CombinedPlot$new(plotObject = test_plot)
  result <- combined$combined()
  expect_equal(result, test_plot)
  
  # Test with table returns cowplot grid
  test_table <- data.frame(Parameter = c("A", "B"), Value = c(1, 2))
  combined$tableObject <- test_table
  result <- combined$combined()
  expect_s3_class(result, "ggplot")
  # Should be different from original plot since it's now a combined plot
  expect_false(identical(result, test_plot))
})

test_that("CombinedPlot print method works", {
  skip_if_not_installed("ggplot2")
  
  test_plot <- ggplot2::ggplot() + ggplot2::geom_blank()
  combined <- CombinedPlot$new(plotObject = test_plot)
  
  # Test that print method runs without errors
  expect_no_error(capture.output(combined$print()))
  
  # Test with table
  test_table <- data.frame(Parameter = c("A", "B"), Value = c(1, 2))
  combined$tableObject <- test_table
  expect_no_error(capture.output(combined$print()))
  
  # Print method should return the combined plot invisibly
  result <- capture.output(returned_value <- combined$print())
  expect_s3_class(returned_value, "ggplot")
})

test_that("CombinedPlot legend adjustment works", {
  skip_if_not_installed("ggplot2")
  
  # Create plot with legend on right
  test_data <- data.frame(x = 1:3, y = 1:3, color = factor(c("A", "B", "A")))
  test_plot <- ggplot2::ggplot(test_data, ggplot2::aes(x = x, y = y, color = color)) +
    ggplot2::geom_point() +
    ggplot2::theme(legend.position = "right")
  
  combined <- CombinedPlot$new(plotObject = test_plot)
  test_table <- data.frame(Parameter = c("A", "B"), Value = c(1, 2))
  combined$tableObject <- test_table
  
  # When combined with table, legend should be moved to top
  result <- combined$combined()
  expect_s3_class(result, "ggplot")
  
  # The original plot should have been modified to move legend to top
  # (this tests the private adjustLegendPosition method)
  expect_equal(combined$plotObject$theme$legend.position, "top")
})

test_that("CombinedPlot cloning works", {
  skip_if_not_installed("ggplot2")
  
  test_plot <- ggplot2::ggplot() + ggplot2::geom_blank()
  test_table <- data.frame(Parameter = c("A", "B"), Value = c(1, 2))
  
  original <- CombinedPlot$new(plotObject = test_plot, tableObject = test_table)
  cloned <- original$clone()
  
  expect_r6_class(cloned, "CombinedPlot")
  expect_equal(cloned$plotObject, original$plotObject)
  expect_equal(cloned$tableObject, original$tableObject)
  expect_equal(cloned$relWidths, original$relWidths)
  
  # Test deep cloning - changes to clone shouldn't affect original
  cloned$relWidths <- c(2, 3)
  expect_equal(original$relWidths, c(4, 1)) # should be unchanged
  expect_equal(cloned$relWidths, c(2, 3))
})