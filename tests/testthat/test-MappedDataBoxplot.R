# Test MappedDataBoxplot class

test_that("MappedDataBoxplot initialization works correctly", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("R6")

  # Create test data
  test_data <- data.frame(
    x = c(1, 2, 3, 1, 2, 3),
    y = c(10, 15, 12, 8, 14, 11),
    group = factor(c("A", "A", "A", "B", "B", "B"))
  )

  test_mapping <- ggplot2::aes(x = x, y = y)

  # Test basic initialization
  mapped_data <- MappedDataBoxplot$new(
    data = test_data,
    mapping = test_mapping
  )

  expect_s3_class(mapped_data, "MappedDataBoxplot")
  expect_s3_class(mapped_data, "MappedData")
  expect_equal(mapped_data$data, test_data)
  expect_true(mapped_data$hasXmapping)
})

test_that("MappedDataBoxplot handles x mapping detection correctly", {
  skip_if_not_installed("ggplot2")

  test_data <- data.frame(y = c(10, 15, 12))

  # Test without x mapping
  mapped_data_no_x <- MappedDataBoxplot$new(
    data = test_data,
    mapping = ggplot2::aes(y = y)
  )
  expect_false(mapped_data_no_x$hasXmapping)

  # Test with x mapping
  test_data_with_x <- data.frame(x = c(1, 2, 3), y = c(10, 15, 12))
  mapped_data_with_x <- MappedDataBoxplot$new(
    data = test_data_with_x,
    mapping = ggplot2::aes(x = x, y = y)
  )
  expect_true(mapped_data_with_x$hasXmapping)
})

test_that("MappedDataBoxplot boxwhiskerMapping active field works correctly", {
  skip_if_not_installed("ggplot2")

  test_data <- data.frame(
    x = c(1, 2, 3),
    y = c(10, 15, 12)
  )

  # Test with x mapping
  mapped_data_with_x <- MappedDataBoxplot$new(
    data = test_data,
    mapping = ggplot2::aes(x = x, y = y)
  )

  boxwhisker_mapping <- mapped_data_with_x$boxwhiskerMapping
  expect_s3_class(boxwhisker_mapping, "uneval")
  # When x is mapped, should return empty aes()
  expect_equal(length(boxwhisker_mapping), 0)

  # Test without x mapping
  test_data_no_x <- data.frame(y = c(10, 15, 12))
  mapped_data_no_x <- MappedDataBoxplot$new(
    data = test_data_no_x,
    mapping = ggplot2::aes(y = y)
  )

  boxwhisker_mapping_no_x <- mapped_data_no_x$boxwhiskerMapping
  expect_s3_class(boxwhisker_mapping_no_x, "uneval")
  expect_true("x" %in% names(boxwhisker_mapping_no_x))
})

test_that("MappedDataBoxplot doAdjustmentsWithMetaData works correctly", {
  skip_if_not_installed("ggplot2")

  # Test data with numeric x
  testDataNumeric <- data.frame(
    x = c(1.5, 2.7, 3.1),
    y = c(10, 15, 12)
  )

  mappedDataNumeric <- MappedDataBoxplot$new(
    data = testDataNumeric,
    mapping = ggplot2::aes(x = x, y = y)
  )

  # Test with linear scale for numeric data
  expect_warning({
    mappedDataNumeric$doAdjustmentsWithMetaData(
      originalmapping = ggplot2::aes(x = x, y = y),
      xscale = "linear",
      xscale.args = list()
    )
  })

  mappedDataNumeric$addMetaData(
    metaData = list(x = list(dimension = 'time',
                             unit = 'h'))
  )

  # Test with linear scale for numeric data
  expect_no_error({
    mappedDataNumeric$doAdjustmentsWithMetaData(
      originalmapping = ggplot2::aes(x = x, y = y),
      xscale = "linear",
      xscale.args = list()
    )
  })


  expect_equal(mappedDataNumeric$xscale, "linear")

  # Test data with factor x
  test_data_factor <- data.frame(
    x = factor(c("A", "B", "C")),
    y = c(10, 15, 12)
  )

  mapped_data_factor <- MappedDataBoxplot$new(
    data = test_data_factor,
    mapping = ggplot2::aes(x = x, y = y)
  )

  mapped_data_factor$addMetaData(
    metaData = list(x = list(dimension = 'time',
                             unit = 'h'))
  )

  # Test with discrete scale for factor data
  expect_no_error({
    mapped_data_factor$doAdjustmentsWithMetaData(
      originalmapping = ggplot2::aes(x = x, y = y),
      xscale = "auto",
      xscale.args = list()
    )
  })

  expect_equal(mapped_data_factor$xscale, "discrete")
})

test_that("MappedDataBoxplot scale validation works correctly", {
  skip_if_not_installed("ggplot2")

  # Test error when trying to use continuous scale with factor data
  test_data_factor <- data.frame(
    x = factor(c("A", "B", "C")),
    y = c(10, 15, 12)
  )

  mapped_data_factor <- MappedDataBoxplot$new(
    data = test_data_factor,
    mapping = ggplot2::aes(x = x, y = y)
  )
  mapped_data_factor$addMetaData(
    metaData = list(x = list(dimension = 'time',
                             unit = 'h'))
  )

  expect_error({
    mapped_data_factor$doAdjustmentsWithMetaData(
      originalmapping = ggplot2::aes(x = x, y = y),
      xscale = "linear",  # This should fail for factor data
      xscale.args = list()
    )
  })

  # Test error when trying to use discrete scale with numeric data
  testDataNumeric <- data.frame(
    x = c(1, 2, 3),
    y = c(10, 15, 12)
  )

  mappedDataNumeric <- MappedDataBoxplot$new(
    data = testDataNumeric,
    mapping = ggplot2::aes(x = x, y = y)
  )
  mappedDataNumeric$addMetaData(
    metaData = list(x = list(dimension = 'time',
                             unit = 'h'))
  )

  expect_error({
    mappedDataNumeric$doAdjustmentsWithMetaData(
      originalmapping = ggplot2::aes(x = x, y = y),
      xscale = "discrete",  # This should fail for numeric data
      xscale.args = list()
    )
  })
})

test_that("MappedDataBoxplot input validation works", {
  skip_if_not_installed("ggplot2")

  test_data <- data.frame(x = c(1, 2, 3), y = c(10, 15, 12))
  mapped_data <- MappedDataBoxplot$new(
    data = test_data,
    mapping = ggplot2::aes(x = x, y = y)
  )
  mapped_data$addMetaData(
    metaData = list(x = list(dimension = 'time',
                             unit = 'h'))
  )


  # Test doAdjustmentsWithMetaData input validation
  expect_error({
    mapped_data$doAdjustmentsWithMetaData(
      originalmapping = "not_a_mapping",  # Should be a list/mapping
      xscale = "linear",
      xscale.args = list()
    )
  })

  expect_error({
    mapped_data$doAdjustmentsWithMetaData(
      originalmapping = ggplot2::aes(x = x, y = y),
      xscale = c("linear", "log"),  # Should be single character
      xscale.args = list()
    )
  })
})
