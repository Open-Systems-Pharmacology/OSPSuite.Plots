# Test utilities-defaults.R functions

# Store original options to restore later
oldDefaults <- NULL

setup({
  oldDefaults <<- ospsuite.plots::setDefaults()
})

teardown({
  ospsuite.plots::resetDefaults(oldDefaults)
})

test_that("getDefaultGeomAttributes works correctly", {
  # Test with valid geom types
  line_attrs <- getDefaultGeomAttributes("Line")
  expect_type(line_attrs, "list")
  
  point_attrs <- getDefaultGeomAttributes("Point")
  expect_type(point_attrs, "list")
  
  ribbon_attrs <- getDefaultGeomAttributes("Ribbon")
  expect_type(ribbon_attrs, "list")
  expect_equal(ribbon_attrs$color, NA)
  
  # Test error for invalid geom type
  expect_error(getDefaultGeomAttributes("InvalidGeom"))
})

test_that("getOspsuite.plots.option works correctly", {
  # Test getting watermark option
  watermark_enabled <- getOspsuite.plots.option("watermark_enabled")
  expect_type(watermark_enabled, "logical")
  
  # Test getting watermark label
  watermark_label <- getOspsuite.plots.option("watermark_label")
  expect_type(watermark_label, "character")
  expect_equal(watermark_label, "preliminary analysis")
  
  # Test error for invalid option key
  expect_error(getOspsuite.plots.option("invalid_option"))
})

test_that("setOspsuite.plots.option works correctly", {
  # Test setting a valid option
  original_value <- getOspsuite.plots.option("watermark_enabled")
  setOspsuite.plots.option("watermark_enabled", FALSE)
  new_value <- getOspsuite.plots.option("watermark_enabled")
  expect_false(new_value)
  
  # Reset to original value
  setOspsuite.plots.option("watermark_enabled", original_value)
  
  # Test setting NULL value clears the option
  setOspsuite.plots.option("watermark_enabled", NULL)
  cleared_value <- getOspsuite.plots.option("watermark_enabled")
  expect_equal(cleared_value, getDefaultOptions()[["ospsuite.plots.watermark_enabled"]])
  
  # Test error for invalid option key
  expect_error(setOspsuite.plots.option("invalid_option", TRUE))
})

test_that("getDefaultOptions returns complete options list", {
  options_list <- getDefaultOptions()
  expect_type(options_list, "list")
  expect_true(length(options_list) > 0)
  
  # Test presence of key options
  expected_options <- c(
    "ospsuite.plots.watermark_enabled",
    "ospsuite.plots.watermark_label", 
    "ospsuite.plots.geomLineAttributes",
    "ospsuite.plots.geomPointAttributes",
    "ospsuite.plots.Alpha"
  )
  expect_true(all(expected_options %in% names(options_list)))
  
  # Test specific default values
  expect_equal(options_list$ospsuite.plots.watermark_enabled, TRUE)
  expect_equal(options_list$ospsuite.plots.watermark_label, "preliminary analysis")
  expect_equal(options_list$ospsuite.plots.Alpha, 0.5)
})

test_that("setDefaultColorMapDistinct works correctly", {
  skip_if_not_installed("grDevices")
  
  # Test with default colors (NULL)
  old_colors <- setDefaultColorMapDistinct()
  expect_type(old_colors, "list")
  expect_true("ggplot2.discrete.colour" %in% names(old_colors))
  expect_true("ggplot2.discrete.fill" %in% names(old_colors))
  
  # Test with custom colors
  custom_colors <- list(c("#FF0000", "#00FF00", "#0000FF"))
  old_colors2 <- setDefaultColorMapDistinct(custom_colors)
  expect_type(old_colors2, "list")
  
  # Test with character vector (should be converted to list)
  color_vector <- c("red", "green", "blue")
  old_colors3 <- setDefaultColorMapDistinct(color_vector)
  expect_type(old_colors3, "list")
  
  # Test error for invalid colors
  expect_error(setDefaultColorMapDistinct(list(c("invalidcolor123"))))
  
  # Reset to original
  resetDefaultColorMapDistinct(old_colors)
})

test_that("resetDefaultColorMapDistinct works correctly", {
  # Save current colors
  original_colors <- setDefaultColorMapDistinct()
  
  # Set new colors
  new_colors <- list(c("red", "blue"))
  setDefaultColorMapDistinct(new_colors)
  
  # Reset to original
  resetDefaultColorMapDistinct(original_colors)
  
  # Verify reset worked (this would need access to actual ggplot2 options)
  expect_no_error(resetDefaultColorMapDistinct(original_colors))
  
  # Test error for invalid input
  expect_error(resetDefaultColorMapDistinct("not_a_list"))
})

test_that("setDefaultShapeDiscrete works correctly", {
  # Test with default shapes (NULL)
  old_shapes <- setDefaultShapeDiscrete()
  expect_type(old_shapes, "character")
  expect_true(length(old_shapes) > 0)
  
  # Test with custom shapes
  custom_shapes <- c("circle", "square", "triangle")
  result <- setDefaultShapeDiscrete(custom_shapes)
  current_shapes <- getOspsuite.plots.option("shapeValues")
  expect_equal(current_shapes, custom_shapes)
  
  # Reset to original
  resetDefaultShapeDiscrete(old_shapes)
})

test_that("resetDefaultShapeDiscrete works correctly", {
  # Save original shapes
  original_shapes <- getOspsuite.plots.option("shapeValues")
  
  # Set new shapes
  setDefaultShapeDiscrete(c("circle", "square"))
  
  # Reset
  resetDefaultShapeDiscrete(original_shapes)
  
  # Verify reset
  current_shapes <- getOspsuite.plots.option("shapeValues")
  expect_equal(current_shapes, original_shapes)
  
  # Test with NULL
  expect_no_error(resetDefaultShapeDiscrete(NULL))
})

test_that("constructLabelWithUnit handles edge cases", {
  # This test is for the function defined in utilities.R but may be called from defaults
  expect_equal(constructLabelWithUnit("Test", "unit"), "Test [unit]")
  expect_equal(constructLabelWithUnit("Test", ""), "Test")
  expect_null(constructLabelWithUnit(NULL, "unit"))
})