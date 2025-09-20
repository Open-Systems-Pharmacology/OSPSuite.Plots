# Test utilities.R functions

test_that("updateScaleArgumentsForTimeUnit works correctly", {
  # Test with time dimension and various units
  scale_args <- list(limits = c(0, 24))
  
  # Test seconds
  result <- updateScaleArgumentsForTimeUnit(scale_args, "time", "s")
  expect_true("breaks" %in% names(result))
  expect_true("minor_breaks" %in% names(result))
  
  # Test minutes
  result <- updateScaleArgumentsForTimeUnit(scale_args, "time", "min")
  expect_true("breaks" %in% names(result))
  
  # Test hours
  result <- updateScaleArgumentsForTimeUnit(scale_args, "time", "h")
  expect_true("breaks" %in% names(result))
  
  # Test days
  result <- updateScaleArgumentsForTimeUnit(scale_args, "time", "day(s)")
  expect_true("breaks" %in% names(result))
  
  # Test weeks
  result <- updateScaleArgumentsForTimeUnit(scale_args, "time", "week(s)")
  expect_true("breaks" %in% names(result))
  
  # Test months
  result <- updateScaleArgumentsForTimeUnit(scale_args, "time", "month(s)")
  expect_true("breaks" %in% names(result))
  
  # Test non-time dimension returns unchanged
  result <- updateScaleArgumentsForTimeUnit(scale_args, "concentration", "mg/L")
  expect_identical(result, scale_args)
  
  # Test NULL dimension returns unchanged
  result <- updateScaleArgumentsForTimeUnit(scale_args, NULL, "h")
  expect_identical(result, scale_args)
  
  # Test NULL unit returns unchanged
  result <- updateScaleArgumentsForTimeUnit(scale_args, "time", NULL)
  expect_identical(result, scale_args)
  
  # Test scale_args with existing breaks/labels returns unchanged
  scale_args_with_breaks <- list(limits = c(0, 24), breaks = c(0, 12, 24))
  result <- updateScaleArgumentsForTimeUnit(scale_args_with_breaks, "time", "h")
  expect_identical(result, scale_args_with_breaks)
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
  expect_equal(result$`1.5 fold`, c(1.5, 1/1.5))
  expect_equal(result$`2 fold`, c(2, 0.5))
  
  # Test without identity
  result <- getFoldDistanceList(includeIdentity = FALSE)
  expect_named(result, c("1.5 fold", "2 fold"))
  expect_false("identity" %in% names(result))
  
  # Test custom folds
  result <- getFoldDistanceList(folds = c(3, 5))
  expect_named(result, c("identity", "3 fold", "5 fold"))
  expect_equal(result$`3 fold`, c(3, 1/3))
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
  metaData_incomplete <- list(
    concentration = list(dimension = "Concentration"), # missing unit
    time = list(unit = "h") # missing dimension
  )
  
  result <- metaData2DataFrame(metaData_incomplete)
  expect_equal(result["unit", "concentration"], "")
  expect_equal(result["dimension", "time"], "")
  
  # Test with empty metadata
  result <- metaData2DataFrame(list())
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 0)
})