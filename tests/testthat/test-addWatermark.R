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
})
