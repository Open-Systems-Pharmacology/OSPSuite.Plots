oldDefaults <- ospsuite.plots::setDefaults()

test_that("Change watermark", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  setOspsuite.plots.option(
    optionKey = OptionKeys$watermark_format,
    value = list(
      x = 0.2,
      y = 0.9,
      color = "red",
      angle = 90,
      fontsize = 24,
      alpha = 0.2
    )
  )
  setOspsuite.plots.option(
    optionKey = OptionKeys$watermark_label,
    value = "NEW"
  )

  vdiffr::expect_doppelganger(
    title = "watermarkChange",
    fig = initializePlot()
  )

  setOspsuite.plots.option(
    optionKey = OptionKeys$watermark_enabled,
    value = FALSE
  )

  vdiffr::expect_doppelganger(
    title = "watermarkDisabled",
    fig = initializePlot()
  )

  setOspsuite.plots.option(
    optionKey = OptionKeys$watermark_format,
    value = NULL
  )
  setOspsuite.plots.option(
    optionKey = OptionKeys$watermark_label,
    value = NULL
  )
  setOspsuite.plots.option(
    optionKey = OptionKeys$watermark_enabled,
    value = NULL
  )

  vdiffr::expect_doppelganger(
    title = "watermarkReset",
    fig = initializePlot()
  )

  vdiffr::expect_doppelganger(
    title = "watermark_log",
    fig = initializePlot() +
      scale_y_log10() +
      scale_x_log10()
  )
})


test_that("saves plot with watermark in SVG", {
  fig <- ggplotWithWatermark(mtcars, aes(mpg, wt)) + geom_point()
  # Create a temporary file for saving the SVG
  tempSvg <- tempfile(fileext = ".svg")

  # Save the plot as SVG
  suppressMessages(ggsave(tempSvg, plot = fig, device = "svg"))

  # Read the SVG file as text
  svgContent <- readLines(tempSvg)

  watermarkLabel <- getOspsuite.plots.option(optionKey = OptionKeys$watermark_label)

  # Check if the watermark label is present in the SVG content
  expect_true(any(grepl(watermarkLabel, svgContent)),
    info = "Watermark label should be present in the SVG content"
  )
})

# Test for plot_list with cowplot
test_that("cowplot::plot_list works correctly", {
  # Create multiple ggplot objects
  p1 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point()
  p2 <- ggplot(mtcars, aes(x = hp, y = mpg)) +
    geom_point()

  # Combine plots using cowplot
  combiPlot <- cowplot::plot_grid(p1, p2) %>%
    addWatermark()

  # Check that the combined plot is a ggplot object
  vdiffr::expect_doppelganger(
    title = "watermark_cowplotCombi",
    fig = combiPlot
  )
})


ospsuite.plots::resetDefaults(oldDefaults)
