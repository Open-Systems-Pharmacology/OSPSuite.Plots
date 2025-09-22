# Test MappedDataTimeProfile class

test_that("MappedDataTimeProfile initialization works correctly", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("R6")

  # Create test data
  test_data <- data.frame(
    time = c(0, 1, 2, 4, 8),
    concentration = c(0, 10, 8, 6, 3),
    y2axis = c(FALSE, FALSE, FALSE, FALSE, FALSE)
  )

  test_mapping <- ggplot2::aes(x = time, y = concentration)

  # Test basic initialization
  mapped_data <- MappedDataTimeProfile$new(
    data = test_data,
    mapping = test_mapping
  )

  expect_s3_class(mapped_data, "MappedDataTimeProfile")
  expect_s3_class(mapped_data, "MappedData")
  expect_equal(mapped_data$data, test_data)
  expect_false(mapped_data$requireDualAxis)
})

test_that("MappedDataTimeProfile handles secondary axis correctly", {
  skip_if_not_installed("ggplot2")

  # Create test data with y2axis mapping
  test_data <- data.frame(
    time = c(0, 1, 2, 4, 8, 0, 1, 2, 4, 8),
    concentration = c(0, 10, 8, 6, 3, 0, 0.5, 0.4, 0.3, 0.15),
    y2axis = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE)
  )

  test_mapping <- ggplot2::aes(x = time, y = concentration, y2axis = y2axis)

  mapped_data <- MappedDataTimeProfile$new(
    data = test_data,
    mapping = test_mapping,
    scaleOfPrimaryAxis = "linear",
    scaleOfSecondaryAxis = "linear"
  )

  expect_true(mapped_data$requireDualAxis)
  expect_true("y2" %in% names(mapped_data$mapping))
})

test_that("MappedDataTimeProfile scaling validation works", {
  skip_if_not_installed("ggplot2")

  test_data <- data.frame(
    time = c(0, 1, 2),
    concentration = c(1, 10, 8)
  )

  # Test valid scales
  expect_no_error({
    MappedDataTimeProfile$new(
      data = test_data,
      mapping = ggplot2::aes(x = time, y = concentration),
      scaleOfPrimaryAxis = "linear"
    )
  })

  expect_no_error({
    MappedDataTimeProfile$new(
      data = test_data,
      mapping = ggplot2::aes(x = time, y = concentration),
      scaleOfPrimaryAxis = "log"
    )
  })

  # Test invalid scales
  expect_error({
    MappedDataTimeProfile$new(
      data = test_data,
      mapping = ggplot2::aes(x = time, y = concentration),
      scaleOfPrimaryAxis = "invalid_scale"
    )
  })
})

test_that("MappedDataTimeProfile scaleDataForSecondaryAxis works", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("dplyr")

  # Create test data with secondary axis
  test_data <- data.frame(
    time = c(0, 1, 2, 0, 1, 2),
    concentration = c(1, 10, 8, 0.1, 1, 0.8),
    y2axis = c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE)
  )

  mapped_data <- MappedDataTimeProfile$new(
    data = test_data,
    mapping = ggplot2::aes(x = time, y = concentration, y2axis = y2axis),
    scaleOfPrimaryAxis = "linear",
    scaleOfSecondaryAxis = "linear",
    ylimits = c(0, 10),
    y2limits = c(0, 1)
  )

  # Test scaling
  expect_no_error({
    mapped_data$scaleDataForSecondaryAxis()
  })

  # Test that dataForPlot returns scaled data
  plot_data <- mapped_data$dataForPlot
  expect_s3_class(plot_data, "data.frame")
  expect_equal(nrow(plot_data), nrow(test_data))
})

test_that("MappedDataTimeProfile handles different scale combinations", {
  skip_if_not_installed("ggplot2")

  test_data <- data.frame(
    time = c(0, 1, 2, 0, 1, 2),
    concentration = c(1, 10, 8, 0.1, 1, 0.8),
    y2axis = c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE)
  )

  # Test linear-linear combination
  mapped_data_lin_lin <- MappedDataTimeProfile$new(
    data = test_data,
    mapping = ggplot2::aes(x = time, y = concentration, y2axis = y2axis),
    scaleOfPrimaryAxis = "linear",
    scaleOfSecondaryAxis = "linear",
    ylimits = c(0, 10),
    y2limits = c(0, 1)
  )

  expect_no_error(mapped_data_lin_lin$scaleDataForSecondaryAxis())

  # Test log-linear combination (skip if data has values <= 0)
  test_data_positive <- data.frame(
    time = c(1, 2, 3, 1, 2, 3),
    concentration = c(1, 10, 8, 0.1, 1, 0.8),
    y2axis = c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE)
  )

  mapped_data_log_lin <- MappedDataTimeProfile$new(
    data = test_data_positive,
    mapping = ggplot2::aes(x = time, y = concentration, y2axis = y2axis),
    scaleOfPrimaryAxis = "log",
    scaleOfSecondaryAxis = "linear",
    ylimits = c(0.1, 10),
    y2limits = c(0.1, 1)
  )

  expect_no_error(mapped_data_log_lin$scaleDataForSecondaryAxis())
})

test_that("MappedDataTimeProfile active fields work correctly", {
  skip_if_not_installed("ggplot2")

  test_data <- data.frame(
    time = c(0, 1, 2),
    concentration = c(1, 10, 8),
    group = c("A", "A", "B")
  )

  mapped_data <- MappedDataTimeProfile$new(
    data = test_data,
    mapping = ggplot2::aes(x = time, y = concentration, color = group),
    groupAesthetics = c("color")
  )

  # Test listOfGroups
  groups <- mapped_data$listOfGroups
  expect_true(is.character(groups) || is.null(groups))

  # Test secAxis (should be waiver when no secondary axis)
  sec_axis <- mapped_data$secAxis
  expect_s3_class(sec_axis,'waiver')

  # Test dataForPlot
  plot_data <- mapped_data$dataForPlot
  expect_s3_class(plot_data, "data.frame")
  expect_equal(nrow(plot_data), nrow(test_data))
})

test_that("MappedDataTimeProfile handles limits correctly", {
  skip_if_not_installed("ggplot2")

  test_data <- data.frame(
    time = c(0, 1, 2, 0, 1, 2),
    concentration = c(1, 10, 8, 0.1, 1, 0.8),
    y2axis = c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE)
  )

  # Test with explicit limits
  mapped_data <- MappedDataTimeProfile$new(
    data = test_data,
    mapping = ggplot2::aes(x = time, y = concentration, y2axis = y2axis),
    ylimits = c(0, 12),
    y2limits = c(0, 1.5)
  )

  expect_equal(mapped_data$ylimits, c(0, 12))
  expect_equal(mapped_data$y2limits, c(0, 1.5))

  # Test with NULL limits (should be calculated automatically)
  mapped_data_auto <- MappedDataTimeProfile$new(
    data = test_data,
    mapping = ggplot2::aes(x = time, y = concentration, y2axis = y2axis)
  )

  expect_type(mapped_data_auto$ylimits, "double")
  expect_type(mapped_data_auto$y2limits, "double")
  expect_length(mapped_data_auto$ylimits, 2)
  expect_length(mapped_data_auto$y2limits, 2)
})

test_that("MappedDataTimeProfile input validation works", {
  skip_if_not_installed("ggplot2")

  test_data <- data.frame(time = c(0, 1, 2), concentration = c(1, 10, 8), y2 = c(0, 1, 0))

  # Test y2limits validation
  expect_error({
    mappedData <- MappedDataTimeProfile$new(
      data = test_data,
      mapping = ggplot2::aes(x = time, y = concentration, y2axis = as.logical(y2)),
      y2limits = c(10, 5)  # Should be sorted
    )
  })

  expect_error({
    MappedDataTimeProfile$new(
      data = test_data,
      mapping = ggplot2::aes(x = time, y = concentration, y2axis = as.logical(y2)),
      y2limits = c(1, 1)  # Should be unique
    )
  })
})
