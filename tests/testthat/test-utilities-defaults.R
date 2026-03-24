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
  # Test getting watermark option when it's set
  # Set it first since it's no longer in defaults
  setOspsuite.plots.option("watermarkEnabled", TRUE)
  watermarkEnabled <- getOspsuite.plots.option("watermarkEnabled")
  expect_type(watermarkEnabled, "logical")

  # Test getting watermark label
  watermarkLabel <- getOspsuite.plots.option("watermarkLabel")
  expect_type(watermarkLabel, "character")
  expect_equal(watermarkLabel, "preliminary analysis")

  # Test error for invalid option key
  expect_error(getOspsuite.plots.option("invalid_option"))
})

test_that("setOspsuite.plots.option works correctly", {
  # Test setting a valid option
  setOspsuite.plots.option("watermarkEnabled", TRUE)
  originalValue <- getOspsuite.plots.option("watermarkEnabled")
  setOspsuite.plots.option("watermarkEnabled", FALSE)
  newValue <- getOspsuite.plots.option("watermarkEnabled")
  expect_false(newValue)

  # Reset to original value
  setOspsuite.plots.option("watermarkEnabled", originalValue)

  # Test setting NULL value clears the option
  setOspsuite.plots.option("watermarkEnabled", NULL)
  clearedValue <- getOspsuite.plots.option("watermarkEnabled")
  # watermarkEnabled has no default, so cleared value should be NULL
  expect_null(clearedValue)

  # Reset for other tests
  setOspsuite.plots.option("watermarkEnabled", TRUE)

  # Test error for invalid option key
  expect_error(setOspsuite.plots.option("invalid_option", TRUE))
})

test_that("getDefaultOptions returns complete options list", {
  optionsList <- getDefaultOptions()
  expect_type(optionsList, "list")
  expect_true(length(optionsList) > 0)

  # Test presence of all key options (verifies the renaming)
  expectedOptions <- c(
    "ospsuite.plots.watermarkLabel",
    "ospsuite.plots.watermarkFormat",
    "ospsuite.plots.geomLineAttributes",
    "ospsuite.plots.geomPointAttributes",
    "ospsuite.plots.alpha",
    "ospsuite.plots.lloqAlphaVector",
    "ospsuite.plots.lloqLineType",
    "ospsuite.plots.percentiles",
    "ospsuite.plots.defaultPercentiles",
    "ospsuite.plots.geomPointUnicode",
    "ospsuite.plots.exportWidth",
    "ospsuite.plots.exportUnits",
    "ospsuite.plots.exportDevice",
    "ospsuite.plots.exportDpi"
  )
  expect_true(all(expectedOptions %in% names(optionsList)))

  # Test specific default values for the renamed keys
  expect_equal(
    optionsList$ospsuite.plots.watermarkLabel,
    "preliminary analysis"
  )
  expect_equal(optionsList$ospsuite.plots.alpha, 0.5)
  expect_equal(optionsList$ospsuite.plots.lloqLineType, "dashed")
  expect_equal(
    optionsList$ospsuite.plots.percentiles,
    c(0.05, 0.25, 0.5, 0.75, 0.95)
  )
  expect_equal(
    optionsList$ospsuite.plots.defaultPercentiles,
    c(0.05, 0.5, 0.95)
  )
  expect_false(optionsList$ospsuite.plots.geomPointUnicode)
  expect_equal(optionsList$ospsuite.plots.exportWidth, 16)
  expect_equal(optionsList$ospsuite.plots.exportUnits, "cm")
  expect_equal(optionsList$ospsuite.plots.exportDevice, "png")
  expect_equal(optionsList$ospsuite.plots.exportDpi, 300)
  expect_equal(
    optionsList$ospsuite.plots.lloqAlphaVector,
    c('TRUE' = 0.3, 'FALSE' = 1)
  )
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
