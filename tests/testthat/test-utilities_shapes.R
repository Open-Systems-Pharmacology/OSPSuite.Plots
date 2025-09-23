# Test utilities_shapes.R functionality

test_that("Shapes list contains expected shapes", {
  expect_type(Shapes, "list")
  expect_true(length(Shapes) > 0)

  # Test presence of basic shapes
  expectedShapes <- c("circle", "diamond", "triangle", "square", "cross", "plus", "star")
  expect_true(all(expectedShapes %in% names(Shapes)))

  # Test presence of open shapes
  openShapes <- c("circleOpen", "diamondOpen", "triangleOpen", "squareOpen")
  expect_true(all(openShapes %in% names(Shapes)))

  # Test presence of some emojis/special shapes
  specialShapes <- c("male", "female", "blank")
  expect_true(all(specialShapes %in% names(Shapes)))

  # All shapes should be character strings (Unicode)
  expect_true(all(sapply(Shapes, is.character)))

  # All shapes should be Unicode strings of 1 or 2 characters (to allow for surrogate pairs or combining glyphs)
  expect_true(all(sapply(Shapes, function(x) nchar(x, type = "chars") == 1 | nchar(x, type = "chars") == 2)))

  # Test specific known shapes
  expect_equal(Shapes$circle, "\u25cf")
  expect_equal(Shapes$square, "\u25a0")
  expect_equal(Shapes$blank, " ")
})

test_that(".asPlotShape converts shapes correctly", {
  # Test with shape names from Shapes list
  result <- .asPlotShape("circle")
  expect_equal(result, Shapes$circle)

  result <- .asPlotShape("square")
  expect_equal(result, Shapes$square)

  # Test with multiple shapes
  result <- .asPlotShape(c("circle", "square"))
  expect_equal(result, c(Shapes$circle, Shapes$square))

  # Test with unicode characters directly
  unicodeChar <- "\u25cf"
  result <- .asPlotShape(unicodeChar)
  expect_equal(result, unicodeChar)

  # Test with invalid shape name (should return as-is if not in Shapes)
  result <- .asPlotShape("nonexistent")
  expect_equal(result, "nonexistent")

  # Test with empty string should generate warning and use default
  expect_warning(result <- .asPlotShape(""))
  expect_equal(result, Shapes[["square"]])

  # Test input validation
  expect_error(.asPlotShape(NULL))
  expect_error(.asPlotShape(123))
})

test_that(".selectFontFamily works correctly", {
  # Test default behavior
  result <- .selectFontFamily()
  expect_type(result, "character")
  expect_length(result, 1)

  # Test with custom font family
  result <- .selectFontFamily("serif")
  expect_type(result, "character")

  # Result should be either the input font or "Symbola" if available
  expect_true(result %in% c("serif", "sans", "Symbola"))
})

test_that("geomPointUnicode creates layer correctly", {
  # Test that geomPointUnicode returns a ggplot layer
  layer <- geomPointUnicode()
  expect_s3_class(layer, "LayerInstance")
  expect_s3_class(layer, "ggproto")

  # Test with custom parameters
  layerCustom <- geomPointUnicode(na.rm = TRUE, show.legend = FALSE)
  expect_s3_class(layerCustom, "LayerInstance")
  expect_false(layerCustom$show.legend)
})

test_that("GeomPointUnicodeProto has correct structure", {
  # Test that the ggproto object has required components
  expect_true(ggplot2::is_ggproto(GeomPointUnicodeProto))
  expect_true("default_aes" %in% names(GeomPointUnicodeProto))
  expect_true("draw_panel" %in% names(GeomPointUnicodeProto))
  expect_true("draw_key" %in% names(GeomPointUnicodeProto))

  # Test default aesthetics
  defaultAes <- GeomPointUnicodeProto$default_aes
  expect_true("shape" %in% names(defaultAes))
  expect_true("colour" %in% names(defaultAes))
  expect_true("size" %in% names(defaultAes))

  # Default shape should be a Unicode character
  expect_equal(defaultAes$shape, "\u2588")
})

# Integration test with actual plotting (if ggplot2 is available)
test_that("geomPointUnicode integrates with ggplot2", {
  skip_if_not_installed("ggplot2")

  # Create simple test data
  testData <- data.frame(x = 1:3, y = 1:3, shape = c("circle", "square", "triangle"))

  # Test that the geom can be added to a ggplot without errors
  expect_no_error({
    p <- ggplot(testData, aes(x = x, y = y, shape = shape)) +
      geomPointUnicode()
  })

  # Test building the plot doesn't throw errors
  expect_no_error({
    builtPlot <- ggplot_build(p)
  })
})
