# Test zzz.R package loading functionality

test_that(".onLoad function exists and can be called", {
  # Test that .onLoad function exists in the namespace
  expect_true(exists(".onLoad", envir = asNamespace("ospsuite.plots"), inherits = FALSE))

  # Test that .onLoad can be called without errors
  # This tests font loading functionality
  expect_no_error({
    .onLoad("ospsuite.plots", "ospsuite.plots")
  })
})

test_that(".onLoad displays message when watermark option is not set", {
  # Save current option value
  oldValue <- getOption("ospsuite.plots.watermarkEnabled")

  # Clear the option
  options(ospsuite.plots.watermarkEnabled = NULL)

  # Test that .onLoad produces a message when watermark option is not set
  expect_message(
    .onLoad("ospsuite.plots", "ospsuite.plots"),
    "ospsuite.plots.watermarkEnabled.*not set"
  )

  # Restore option
  options(ospsuite.plots.watermarkEnabled = oldValue)
})

test_that(".onLoad does not display message when watermark option is set", {
  # Save current option value
  oldValue <- getOption("ospsuite.plots.watermarkEnabled")

  # Set the option
  options(ospsuite.plots.watermarkEnabled = TRUE)

  # Test that .onLoad does not produce a message when option is set
  # We can't use expect_no_message directly, so we capture messages
  messages <- capture.output(.onLoad("ospsuite.plots", "ospsuite.plots"), type = "message")

  # Check that there's no watermark-related message
  watermarkMessages <- grep("ospsuite.plots.watermarkEnabled", messages, value = TRUE)
  expect_equal(length(watermarkMessages), 0)

  # Restore option
  options(ospsuite.plots.watermarkEnabled = oldValue)
})


test_that(".onLoad runs without error", {
  expect_no_error({
    .onLoad("ospsuite.plots", "ospsuite.plots")
  })
})

test_that(".onLoad returns invisibly", {
  # Test that .onLoad returns invisible NULL as documented
  result <- .onLoad("ospsuite.plots", "ospsuite.plots")
  expect_null(result)
})
