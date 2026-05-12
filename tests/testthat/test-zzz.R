# Test zzz.R package loading functionality

test_that(".onLoad function exists and can be called", {
  # Test that .onLoad function exists in the namespace
  expect_true(exists(".onLoad", envir = asNamespace("ospsuite.plots"), inherits = FALSE))

  # Test that .onLoad can be called without errors
  expect_no_error({
    .onLoad("ospsuite.plots", "ospsuite.plots")
  })
})

test_that(".onLoad sets watermarkEnabled to TRUE when option is not set", {
  oldValue <- getOption("ospsuite.plots.watermarkEnabled")
  on.exit(options(ospsuite.plots.watermarkEnabled = oldValue))

  # Clear the option to simulate a fresh session
  options(ospsuite.plots.watermarkEnabled = NULL)

  .onLoad("ospsuite.plots", "ospsuite.plots")

  expect_true(getOption("ospsuite.plots.watermarkEnabled"),
    info = ".onLoad should set watermarkEnabled to TRUE when unset"
  )
})

test_that(".onLoad does not override watermarkEnabled when already set", {
  oldValue <- getOption("ospsuite.plots.watermarkEnabled")
  on.exit(options(ospsuite.plots.watermarkEnabled = oldValue))

  # Simulate user having set watermarkEnabled = FALSE in .Rprofile
  options(ospsuite.plots.watermarkEnabled = FALSE)

  .onLoad("ospsuite.plots", "ospsuite.plots")

  expect_false(getOption("ospsuite.plots.watermarkEnabled"),
    info = ".onLoad should not override a user-defined value"
  )
})

test_that(".onLoad runs without error", {
  expect_no_error({
    .onLoad("ospsuite.plots", "ospsuite.plots")
  })
})

test_that(".onLoad returns invisibly", {
  result <- .onLoad("ospsuite.plots", "ospsuite.plots")
  expect_null(result)
})

