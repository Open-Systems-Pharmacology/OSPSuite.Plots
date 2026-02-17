# Test utilities.R functions

test_that("updateScaleArgumentsForTimeUnit works correctly", {
  # Test with time dimension and various units
  scaleArgs <- list(limits = c(0, 24))

  # Test seconds
  result <- updateScaleArgumentsForTimeUnit(scaleArgs, "time", "s")
  expect_true("breaks" %in% names(result))
  expect_true("minor_breaks" %in% names(result))

  # Test minutes
  result <- updateScaleArgumentsForTimeUnit(scaleArgs, "time", "min")
  expect_true("breaks" %in% names(result))

  # Test hours
  result <- updateScaleArgumentsForTimeUnit(scaleArgs, "time", "h")
  expect_true("breaks" %in% names(result))

  # Test days
  result <- updateScaleArgumentsForTimeUnit(scaleArgs, "time", "day(s)")
  expect_true("breaks" %in% names(result))

  # Test weeks
  result <- updateScaleArgumentsForTimeUnit(scaleArgs, "time", "week(s)")
  expect_true("breaks" %in% names(result))

  # Test months
  result <- updateScaleArgumentsForTimeUnit(scaleArgs, "time", "month(s)")
  expect_true("breaks" %in% names(result))

  # Test non-time dimension returns unchanged
  result <- updateScaleArgumentsForTimeUnit(scaleArgs, "concentration", "mg/L")
  expect_identical(result, scaleArgs)

  # Test NULL dimension returns unchanged
  result <- updateScaleArgumentsForTimeUnit(scaleArgs, NULL, "h")
  expect_identical(result, scaleArgs)

  # Test NULL unit returns unchanged
  result <- updateScaleArgumentsForTimeUnit(scaleArgs, "time", NULL)
  expect_identical(result, scaleArgs)

  # Test scaleArgs with existing breaks/labels returns unchanged
  scaleArgsWithBreaks <- list(limits = c(0, 24), breaks = c(0, 12, 24))
  result <- updateScaleArgumentsForTimeUnit(scaleArgsWithBreaks, "time", "h")
  expect_identical(result, scaleArgsWithBreaks)
})

test_that("constructLabelWithUnit works correctly", {
  # Test with both label and unit
  result <- constructLabelWithUnit("Temperature", "Celsius")
  expect_equal(result, "Temperature [Celsius]")

  # Test with label and empty unit
  result <- constructLabelWithUnit("Length", "")
  expect_equal(result, "Length")

  # Test with label and whitespace unit
  result <- constructLabelWithUnit("Mass", "   ")
  expect_equal(result, "Mass")

  # Test with NULL label
  result <- constructLabelWithUnit(NULL, "kg")
  expect_null(result)

  # Test with NULL unit
  result <- constructLabelWithUnit("Weight", NULL)
  expect_equal(result, "Weight")

  # Test with both NULL
  result <- constructLabelWithUnit(NULL, NULL)
  expect_null(result)

  # Test with factor input (converted to character)
  result <- constructLabelWithUnit(factor("Distance"), "m")
  expect_equal(result, "Distance [m]")

  # Test with numeric input (converted to character)
  result <- constructLabelWithUnit(123, "units")
  expect_equal(result, "123 [units]")

  # Test trimming whitespace
  result <- constructLabelWithUnit("  Pressure  ", "  Pa  ")
  expect_equal(result, "Pressure [Pa]")
})

test_that("getFoldDistanceList works correctly", {
  # Test default case
  result <- getFoldDistanceList()
  expect_named(result, c("identity", "1.5 fold", "2 fold"))
  expect_equal(result$identity, 1)
  expect_equal(result$`1.5 fold`, c(1.5, 1 / 1.5))
  expect_equal(result$`2 fold`, c(2, 0.5))

  # Test without identity
  result <- getFoldDistanceList(includeIdentity = FALSE)
  expect_named(result, c("1.5 fold", "2 fold"))
  expect_false("identity" %in% names(result))

  # Test custom folds
  result <- getFoldDistanceList(folds = c(3, 5))
  expect_named(result, c("identity", "3 fold", "5 fold"))
  expect_equal(result$`3 fold`, c(3, 1 / 3))
  expect_equal(result$`5 fold`, c(5, 0.2))

  # Test single fold
  result <- getFoldDistanceList(folds = 2.5)
  expect_named(result, c("identity", "2.5 fold"))
  expect_equal(result$`2.5 fold`, c(2.5, 0.4))

  # Test error for invalid folds (should be > 1)
  expect_error(getFoldDistanceList(folds = 0.5))
  expect_error(getFoldDistanceList(folds = c(1.5, 0.8)))
})

test_that("metaData2DataFrame works correctly", {
  # Test with complete metadata
  metaData <- list(
    concentration = list(dimension = "Concentration", unit = "mg/L"),
    time = list(dimension = "Time", unit = "h"),
    weight = list(dimension = "Weight", unit = "kg")
  )

  result <- metaData2DataFrame(metaData)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2) # dimension and unit rows
  expect_equal(ncol(result), 3) # three variables
  expect_equal(rownames(result), c("dimension", "unit"))
  expect_equal(result["dimension", "concentration"], "Concentration")
  expect_equal(result["unit", "time"], "h")

  # Test with missing elements (should be empty string)
  metaDataIncomplete <- list(
    concentration = list(dimension = "Concentration"), # missing unit
    time = list(unit = "h") # missing dimension
  )

  result <- metaData2DataFrame(metaDataIncomplete)
  expect_equal(result["unit", "concentration"], "")
  expect_equal(result["dimension", "time"], "")

  # Test with empty metadata
  result <- metaData2DataFrame(list())
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 0)
})

test_that("calculateResiduals works correctly with log scaling", {
  predicted <- c(1.5, 2.0, 3.5, 5.0, 7.5)
  observed <- c(1.2, 2.1, 3.0, 5.5, 7.0)

  # Test log scaling
  result <- calculateResiduals(predicted, observed, scaling = "log")
  expected <- log(predicted) - log(observed)
  expect_equal(result, expected)
  expect_length(result, 5)

  # Test that default is log scaling
  result_default <- calculateResiduals(predicted, observed)
  expect_equal(result_default, result)

  # Test warning and NA for non-positive values with log scaling
  expect_warning(
    result <- calculateResiduals(c(-1, 2, 3), c(1, 2, 3), scaling = "log"),
    "1 residual value set to NA: non-positive values found for log scaling"
  )
  expect_true(is.na(result[1]))
  expect_false(is.na(result[2]))

  expect_warning(
    result <- calculateResiduals(c(1, 2, 3), c(-1, 2, 3), scaling = "log"),
    "1 residual value set to NA: non-positive values found for log scaling"
  )
  expect_true(is.na(result[1]))

  expect_warning(
    result <- calculateResiduals(c(0, 2, 3), c(1, 2, 3), scaling = "log"),
    "1 residual value set to NA: non-positive values found for log scaling"
  )
  expect_true(is.na(result[1]))

  # Test multiple invalid values
  expect_warning(
    result <- calculateResiduals(c(-1, 0, 3), c(1, -2, 3), scaling = "log"),
    "3 residual values set to NA: non-positive values found for log scaling"
  )
  expect_true(is.na(result[1]))
  expect_true(is.na(result[2]))
  expect_false(is.na(result[3]))
})

test_that("calculateResiduals works correctly with linear scaling", {
  predicted <- c(1.5, 2.0, 3.5, 5.0, 7.5)
  observed <- c(1.2, 2.1, 3.0, 5.5, 7.0)

  # Test linear scaling
  result <- calculateResiduals(predicted, observed, scaling = "linear")
  expected <- predicted - observed
  expect_equal(result, expected)
  expect_length(result, 5)

  # Test with negative values (should work for linear)
  result_neg <- calculateResiduals(c(-1, 2, 3), c(1, -2, 3), scaling = "linear")
  expect_equal(result_neg, c(-2, 4, 0))
})

test_that("calculateResiduals works correctly with ratio scaling", {
  predicted <- c(1.5, 2.0, 3.5, 5.0, 7.5)
  observed <- c(1.2, 2.1, 3.0, 5.5, 7.0)

  # Test ratio scaling - now predicted / observed
  result <- calculateResiduals(predicted, observed, scaling = "ratio")
  expected <- predicted / observed
  expect_equal(result, expected)
  expect_length(result, 5)

  # Test warning and NA for zero observed values with ratio scaling
  expect_warning(
    result <- calculateResiduals(c(1, 2, 3), c(0, 2, 3), scaling = "ratio"),
    "1 residual value set to NA: zero observed values found for ratio scaling"
  )
  expect_true(is.na(result[1]))
  expect_false(is.na(result[2]))

  # Test multiple zero observed values
  expect_warning(
    result <- calculateResiduals(c(1, 2, 3), c(0, 0, 3), scaling = "ratio"),
    "2 residual values set to NA: zero observed values found for ratio scaling"
  )
  expect_true(is.na(result[1]))
  expect_true(is.na(result[2]))
  expect_false(is.na(result[3]))
})

test_that("calculateResiduals handles NA values correctly", {
  predicted <- c(1.5, NA, 3.5, 5.0)
  observed <- c(1.2, 2.1, NA, 5.5)

  # Test that NA values are preserved
  result_log <- calculateResiduals(predicted, observed, scaling = "log")
  expect_true(is.na(result_log[2]))
  expect_true(is.na(result_log[3]))
  expect_false(is.na(result_log[1]))
  expect_false(is.na(result_log[4]))

  result_linear <- calculateResiduals(predicted, observed, scaling = "linear")
  expect_true(is.na(result_linear[2]))
  expect_true(is.na(result_linear[3]))

  result_ratio <- calculateResiduals(predicted, observed, scaling = "ratio")
  expect_true(is.na(result_ratio[2]))
  expect_true(is.na(result_ratio[3]))
})

test_that("calculateResiduals validates input correctly", {
  # Test length mismatch
  expect_error(
    calculateResiduals(c(1, 2, 3), c(1, 2)),
    "predicted and observed must have the same length"
  )

  # Test invalid scaling
  expect_error(
    calculateResiduals(c(1, 2, 3), c(1, 2, 3), scaling = "invalid"),
    "Must be element of set"
  )

  # Test non-numeric inputs
  expect_error(
    calculateResiduals(c("a", "b"), c(1, 2)),
    "Assertion on 'predicted' failed"
  )
  expect_error(
    calculateResiduals(c(1, 2), c("a", "b")),
    "Assertion on 'observed' failed"
  )
})

test_that("calculateResiduals matches MappedData internal calculations", {
  # This test ensures consistency with the internal residual calculation
  # in MappedData$adjustForResidualMatch
  predicted <- c(1.5, 2.0, 3.5, 5.0, 7.5)
  observed <- c(1.2, 2.1, 3.0, 5.5, 7.0)

  # Log scaling: log(predicted) - log(observed)
  result_log <- calculateResiduals(predicted, observed, scaling = "log")
  expected_log <- log(predicted) - log(observed)
  expect_equal(result_log, expected_log)

  # Linear scaling: predicted - observed
  result_linear <- calculateResiduals(predicted, observed, scaling = "linear")
  expected_linear <- predicted - observed
  expect_equal(result_linear, expected_linear)

  # Ratio scaling: predicted / observed (changed from observed / predicted)
  result_ratio <- calculateResiduals(predicted, observed, scaling = "ratio")
  expected_ratio <- predicted / observed
  expect_equal(result_ratio, expected_ratio)
})
