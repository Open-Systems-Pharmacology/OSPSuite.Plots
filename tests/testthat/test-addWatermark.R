test_that("Change watermark", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  options(list(
    ospsuite.plots.watermark_format = list(
      x = 0.2,
      y = 0.9,
      color = "red",
      angle = 90,
      fontsize = 24,
      alpha = 0.2
    ),
    ospsuite.plots.watermark_label = "NEW"
  ))

  vdiffr::expect_doppelganger(
    title = "watermarkChange",
    fig = initializePlot()
  )


  options(list(ospsuite.plots.watermark_enabled = FALSE))

  vdiffr::expect_doppelganger(
    title = "watermarkDisabled",
    fig = initializePlot()
  )

  options(list(
    ospsuite.plots.watermark_format = NULL,
    ospsuite.plots.watermark_label = NULL,
    ospsuite.plots.watermark_enabled = NULL
  ))

  vdiffr::expect_doppelganger(
    title = "watermarkReset",
    fig = initializePlot()
  )
})
