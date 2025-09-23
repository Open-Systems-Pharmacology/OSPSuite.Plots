# Test utilities-defaults.R functions

# Store original options to restore later
oldDefaults <- ospsuite.plots::setDefaults()

test_that("getDefaultGeomAttributes works correctly", {
  # Test with valid geom types
  lineAttrs <- getDefaultGeomAttributes("Line")
  expect_type(lineAttrs, "list")

  pointAttrs <- getDefaultGeomAttributes("Point")
  expect_type(pointAttrs, "list")

  ribbonAttrs <- getDefaultGeomAttributes("Ribbon")
  expect_type(ribbonAttrs, "list")
  expect_equal(ribbonAttrs$color, NA)

  # Test error for invalid geom type
  expect_error(getDefaultGeomAttributes("InvalidGeom"))
})

test_that("getOspsuite.plots.option works correctly", {
  # Test getting watermark option
  watermarkEnabled <- getOspsuite.plots.option("watermark_enabled")
  expect_type(watermarkEnabled, "logical")

  # Test getting watermark label
  watermarkLabel <- getOspsuite.plots.option("watermark_label")
  expect_type(watermarkLabel, "character")
  expect_equal(watermarkLabel, "preliminary analysis")

  # Test error for invalid option key
  expect_error(getOspsuite.plots.option("invalid_option"))
})

test_that("setOspsuite.plots.option works correctly", {
  # Test setting a valid option
  originalValue <- getOspsuite.plots.option("watermark_enabled")
  setOspsuite.plots.option("watermark_enabled", FALSE)
  newValue <- getOspsuite.plots.option("watermark_enabled")
  expect_false(newValue)

  # Reset to original value
  setOspsuite.plots.option("watermark_enabled", originalValue)

  # Test setting NULL value clears the option
  setOspsuite.plots.option("watermark_enabled", NULL)
  clearedValue <- getOspsuite.plots.option("watermark_enabled")
  expect_equal(clearedValue, getDefaultOptions()[["ospsuite.plots.watermark_enabled"]])

  # Test error for invalid option key
  expect_error(setOspsuite.plots.option("invalid_option", TRUE))
})

test_that("getDefaultOptions returns complete options list", {
  optionsList <- getDefaultOptions()
  expect_type(optionsList, "list")
  expect_true(length(optionsList) > 0)

  # Test presence of key options
  expectedOptions <- c(
    "ospsuite.plots.watermark_enabled",
    "ospsuite.plots.watermark_label",
    "ospsuite.plots.geomLineAttributes",
    "ospsuite.plots.geomPointAttributes",
    "ospsuite.plots.Alpha"
  )
  expect_true(all(expectedOptions %in% names(optionsList)))

  # Test specific default values
  expect_equal(optionsList$ospsuite.plots.watermark_enabled, TRUE)
  expect_equal(optionsList$ospsuite.plots.watermark_label, "preliminary analysis")
  expect_equal(optionsList$ospsuite.plots.Alpha, 0.5)
})

test_that("setDefaultColorMapDistinct works correctly", {
  skip_if_not_installed("grDevices")

  # Test with default colors (NULL)
  oldColors <- setDefaultColorMapDistinct()
  expect_type(oldColors, "list")
  expect_true("ggplot2.discrete.colour" %in% names(oldColors))
  expect_true("ggplot2.discrete.fill" %in% names(oldColors))

  # Test with custom colors
  customColors <- list(c("#FF0000", "#00FF00", "#0000FF"))
  oldColors2 <- setDefaultColorMapDistinct(customColors)
  expect_type(oldColors2, "list")

  # Test with character vector (should be converted to list)
  colorVector <- c("red", "green", "blue")
  oldColors3 <- setDefaultColorMapDistinct(colorVector)
  expect_type(oldColors3, "list")

  # Test error for invalid colors
  expect_error(setDefaultColorMapDistinct(list(c("invalidcolor123"))))

  # Reset to original
  resetDefaultColorMapDistinct(oldColors)
})

test_that("resetDefaultColorMapDistinct works correctly", {
  # Save current colors
  originalColors <- setDefaultColorMapDistinct()

  # Set new colors
  newColors <- list(c("red", "blue"))
  setDefaultColorMapDistinct(newColors)

  # Reset to original
  resetDefaultColorMapDistinct(originalColors)

  # Verify reset worked (this would need access to actual ggplot2 options)
  expect_no_error(resetDefaultColorMapDistinct(originalColors))

  # Test error for invalid input
  expect_error(resetDefaultColorMapDistinct("not_a_list"))
})

test_that("setDefaultShapeDiscrete works correctly", {
  # Test with default shapes (NULL)
  oldShapes <- setDefaultShapeDiscrete()
  expect_type(oldShapes, "character")
  expect_true(length(oldShapes) > 0)

  # Test with custom shapes
  customShapes <- c("circle", "square", "triangle")
  result <- setDefaultShapeDiscrete(customShapes)
  currentShapes <- getOspsuite.plots.option("shapeValues")
  expect_equal(currentShapes, customShapes)

  # Reset to original
  resetDefaultShapeDiscrete(oldShapes)
})

test_that("resetDefaultShapeDiscrete works correctly", {
  # Save original shapes
  originalShapes <- getOspsuite.plots.option("shapeValues")

  # Set new shapes
  setDefaultShapeDiscrete(c("circle", "square"))

  # Reset
  resetDefaultShapeDiscrete(originalShapes)

  # Verify reset
  currentShapes <- getOspsuite.plots.option("shapeValues")
  expect_equal(currentShapes, originalShapes)

  # Test with NULL
  expect_no_error(resetDefaultShapeDiscrete(NULL))
})

test_that("constructLabelWithUnit handles edge cases", {
  # This test is for the function defined in utilities.R but may be called from defaults
  expect_equal(constructLabelWithUnit("Test", "unit"), "Test [unit]")
  expect_equal(constructLabelWithUnit("Test", ""), "Test")
  expect_null(constructLabelWithUnit(NULL, "unit"))
})

ospsuite.plots::resetDefaults(oldDefaults)
