# Test ospsuite.plots-package.R

test_that("Package documentation and imports are properly defined", {
  # Test that key imports are available (these should be loaded when package loads)
  # We test by checking if the namespace can access these functions

  # Check for ggplot2 functions
  expect_true(exists("ggplot", envir = asNamespace("ggplot2"), inherits = FALSE))
  expect_true(exists("aes", envir = asNamespace("ggplot2"), inherits = FALSE))

  # Check that magrittr pipe is accessible
  expect_true(exists("%>%", envir = asNamespace("magrittr"), inherits = FALSE))
})

test_that("Package constants and utilities are defined", {
  # Test that the . utility is defined (used in the package)
  expect_true(exists(".", envir = asNamespace("ospsuite.plots"), inherits = FALSE))

  # The . should be defined as list
  dot_object <- get(".", envir = asNamespace("ospsuite.plots"))
  expect_identical(dot_object, list)
})

test_that("Package namespace is properly structured", {
  # Test that the package namespace exists and contains expected objects
  ns <- asNamespace("ospsuite.plots")
  expect_true(is.environment(ns))

  # Test that some key functions are exported (available in namespace)
  expected_functions <- c(
    "AxisScales", "ResidualScales", "OptionKeys", "Shapes"
  )

  for (func in expected_functions) {
    expect_true(exists(func, envir = ns, inherits = FALSE),
      info = paste("Function", func, "should exist in namespace")
    )
  }
})
