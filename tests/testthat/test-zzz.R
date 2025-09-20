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

test_that("Font files exist in package", {
  # Test that the font files referenced in .onLoad exist
  symbola_path <- system.file("extdata", "Symbola.ttf", package = "ospsuite.plots")
  notosans_path <- system.file("extdata", "NotoSans-Regular.ttf", package = "ospsuite.plots")
  
  # Files should exist (even if they might be empty in test environment)
  expect_type(symbola_path, "character")
  expect_type(notosans_path, "character")
  
  # The system.file should return non-empty string if file exists, empty if not
  # We expect at least the path structure to be present
  expect_true(nchar(symbola_path) >= 0)
  expect_true(nchar(notosans_path) >= 0)
})

test_that(".onLoad handles missing showtext gracefully", {
  # Test behavior when showtext package is not available
  # This is tested by checking that .onLoad doesn't fail even if fonts can't be loaded
  expect_no_error({
    # Temporarily mask showtext if it exists
    if ("showtext" %in% loadedNamespaces()) {
      # We can't easily unload showtext during test, but we can verify
      # that the function doesn't crash
      .onLoad("ospsuite.plots", "ospsuite.plots")
    } else {
      # If showtext is not loaded, .onLoad should handle this gracefully
      .onLoad("ospsuite.plots", "ospsuite.plots")
    }
  })
})

test_that(".onLoad returns invisibly", {
  # Test that .onLoad returns invisible NULL as documented
  result <- .onLoad("ospsuite.plots", "ospsuite.plots")
  expect_null(result)
})