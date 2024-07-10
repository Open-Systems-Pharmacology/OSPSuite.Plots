testthat::test_that("validateFilename replaces forbidden characters", {
  expect_equal(validateFilename("concentration in µg/L"), "concentration in mcg_L")
  expect_equal(validateFilename("project:details"), "project_details")
  expect_equal(validateFilename("invalid*filename?"), "invalid_filename_")
  expect_equal(validateFilename("<test>|doc"), "_test__doc")
})


testthat::test_that("calculatePlotDimensions returns expected dimensions", {
  # Create a simple ggplot object
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point()

  # Assuming default width and height for simplicity
  dimensions <- calculatePlotDimensions(p, width = 10)

  # Check if the function returns a list with width and height
  expect_type(dimensions, "list")
  expect_true(all(c("width", "height") %in% names(dimensions)))

  # width should not change, and plot should be square
  expect_equal(dimensions$width, expected = 10)
  expect_equal(round(dimensions$height), expected = 10)


  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    facet_wrap(vars(gear), ncol = 1) +
    theme(aspect.ratio = 1)


  # Assuming default width and height for simplicity
  dimensions <- calculatePlotDimensions(p, width = 10)

  # width should not change, and each panel should be square
  expect_equal(dimensions$width, expected = 10)
  expect_true(dimensions$height > 25)

  # free scale should increase height
  p2 <- p + facet_wrap(vars(gear), ncol = 1, scales = "free")
  dimensions2 <- calculatePlotDimensions(p2, width = 10)

  expect_true(dimensions2$height > dimensions$height)


  p <-
    ggplot(mtcars, aes(x = wt, y = mpg, shape = as.factor(gear), color = as.factor(cyl))) +
    geom_point() +
    theme(
      legend.position = "top",
      legend.direction = "vertical"
    )


  # Assuming default width and height for simplicity
  dimensions <- calculatePlotDimensions(p, width = 10)

  # width should not change, legend must be added to height to keep the plot square
  expect_equal(dimensions$width, expected = 10)
  expect_true(dimensions$height > 12)
})

testthat::test_that("exportPlot saves a file", {
  tmpDir <- tempdir()
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point()

  # Use a temporary directory and a test filename
  testFilename <- paste0("test_plot.", getOspsuite.plots.option(optionKey = OptionKeys$export.device))
  exportPlot(p, filepath = tmpDir, filename = testFilename, width = 10, height = 10)

  # Clean up the file after testing
  unlink(file.path(tmpDir, testFilename))

  # change ending to default
  testFilename <- "test_plot2.myExtension"
  testFilenamedefault <- paste0("test_plot2.", getOspsuite.plots.option(optionKey = OptionKeys$export.device))
  exportPlot(p, filepath = tmpDir, filename = testFilename, width = 10, height = 10)


  expect_false(file.exists(file.path(tmpDir, testFilename)))
  expect_true(file.exists(file.path(tmpDir, testFilenamedefault)))


  # Optionally, clean up the file after testing
  unlink(file.path(tmpDir, testFilenamedefault))
})
