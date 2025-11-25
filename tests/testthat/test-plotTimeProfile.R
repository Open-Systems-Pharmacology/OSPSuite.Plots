# setDefaults
oldDefaults <- ospsuite.plots::setDefaults()

test_that("plotTimeProfile works basic", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  simData <- exampleDataTimeProfile |>
    dplyr::filter(SetID %in% c("DataSet1", "DataSet2")) |>
    dplyr::filter(Type == "simulated") |>
    dplyr::select(c("time", "values", "maxValues", "minValues", "caption"))

  obsData <- exampleDataTimeProfile |>
    dplyr::filter(SetID %in% c("DataSet1", "DataSet2")) |>
    dplyr::filter(Type == "observed") |>
    dplyr::select(c("time", "values", "maxValues", "minValues", "caption"))

  metaData <- attr(exampleDataTimeProfile, "metaData")

  fig <- plotTimeProfile(
    data = simData,
    observedData = obsData,
    metaData = metaData,
    mapping = aes(
      x = time,
      y = values,
      ymin = minValues,
      ymax = maxValues,
      groupby = caption
    ),
  ) +
    theme(
      legend.position = "top"
    )

  vdiffr::expect_doppelganger(
    title = "basic",
    fig = fig,
  )
})


test_that("plotTimeProfile works logscale", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  simData <- exampleDataTimeProfile |>
    dplyr::filter(SetID %in% c("DataSet1", "DataSet2")) |>
    dplyr::filter(Type == "simulated") |>
    dplyr::select(c("time", "values", "maxValues", "minValues", "caption"))

  obsData <- exampleDataTimeProfile |>
    dplyr::filter(SetID %in% c("DataSet1", "DataSet2")) |>
    dplyr::filter(Type == "observed") |>
    dplyr::select(c("time", "values", "maxValues", "minValues", "caption"))

  metaData <- attr(exampleDataTimeProfile, "metaData")

  fig <- plotTimeProfile(
    data = simData[simData$time > 0, ],
    observedData = obsData,
    metaData = metaData,
    mapping = aes(
      x = time,
      y = values,
      ymin = minValues,
      ymax = maxValues,
      groupby = caption
    ),
    yScale = AxisScales$log
  ) +
    theme(
      legend.position = "top",
      legend.title = element_blank()
    )

  vdiffr::expect_doppelganger(
    title = "basic_log",
    fig
  )
})

test_that("plotTimeProfile works mapping observed plot", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  simData <- exampleDataTimeProfile |>
    dplyr::filter(SetID %in% c("DataSet1", "DataSet2")) |>
    dplyr::filter(Type == "simulated") |>
    dplyr::select(c("time", "values", "minValues", "maxValues", "caption"))

  obsData <- exampleDataTimeProfile |>
    dplyr::filter(SetID %in% c("DataSet1", "DataSet2")) |>
    dplyr::filter(Type == "observed") |>
    dplyr::select(c("time", "values", "sd", "maxValues", "minValues", "caption"))

  metaData <- attr(exampleDataTimeProfile, "metaData")

  # map with reverse order to make sure
  mapSimulatedAndObserved <- data.frame(
    simulated = rev(unique(simData$caption)),
    observed = unique(obsData$caption)
  )

  fig <- plotTimeProfile(
    data = simData,
    observedData = obsData,
    metaData = metaData,
    mapping <- aes(
      x = time,
      y = values,
      groupby = caption
    ),
    mapSimulatedAndObserved = mapSimulatedAndObserved
  ) +
    theme(
      legend.position = "top"
    )

  vdiffr::expect_doppelganger(
    title = "mapped-observed-and-simulated",
    fig
  )
})

test_that("plotTimeProfile works lloq", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  simData <- exampleDataTimeProfile |>
    dplyr::filter(SetID == c("DataSet3")) |>
    dplyr::filter(Type == "simulated") |>
    dplyr::filter(dimension == "concentration") |>
    dplyr::select(c("time", "values", "caption"))

  obsData <- exampleDataTimeProfile |>
    dplyr::filter(SetID == "DataSet3") |>
    dplyr::filter(Type == "observed") |>
    dplyr::filter(dimension == "concentration") |>
    dplyr::select(c("time", "values", "caption", "lloq", "error_relative"))

  obsData$lloq[3] <- NA
  obsData$lloq[4] <- obsData$lloq[4] / 2

  metaData <- attr(exampleDataTimeProfile, "metaData")

  fig <- plotTimeProfile(
    data = simData,
    observedData = obsData,
    metaData = metaData,
    mapping = aes(
      x = time,
      y = values,
      error_relative = error_relative,
      groupby = caption,
      lloq = lloq
    ),
    yScale = AxisScales$log,
    yScaleArgs = list(limits = c(0.01, NA)),
    geomLineAttributes = list(color = "black")
  ) +
    theme(
      legend.position = "top",
      legend.title = element_blank()
    )

  vdiffr::expect_doppelganger(
    title = "with lloq",
    fig
  )


  fig <- plotTimeProfile(
    data = simData,
    observedData = obsData,
    metaData = metaData,
    mapping = aes(
      x = time,
      y = values,
      linetype = "Simulated"
    ),
    observedMapping = aes(
      x = time,
      y = values,
      error_relative = error_relative,
      groupby = caption,
      lloq = lloq
    ),
    groupAesthetics = c("color", "shape", "fill"),
    yScale = AxisScales$log,
    yScaleArgs = list(limits = c(0.01, NA)),
    geomLineAttributes = list(color = "black")
  ) +
    theme(
      legend.position = "top",
      legend.title = element_blank()
    )


  vdiffr::expect_doppelganger(
    title = "with lloq and dashed lines",
    fig
  )
})

test_that("plotTimeProfile works secondary axis", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  metaData <- list(
    time = list(
      dimension = "Time",
      unit = "h"
    ),
    values = list(
      dimension = "Concentration",
      unit = "mg/l"
    ),
    y2 = list(
      dimension = "Fraction",
      unit = ""
    )
  )

  simData <- exampleDataTimeProfile |>
    dplyr::filter(SetID == "DataSet3") |>
    dplyr::filter(Type == "simulated") |>
    dplyr::select(c("time", "values", "dimension", "caption"))


  obsData <- exampleDataTimeProfile |>
    dplyr::filter(SetID == "DataSet3") |>
    dplyr::filter(Type == "observed") |>
    dplyr::select(c("time", "values", "dimension", "caption", "lloq", "error_relative"))


  fig <- plotTimeProfile(
    data = simData,
    observedData = obsData,
    mapping = aes(
      x = time,
      y = values,
      error_relative = error_relative,
      lloq = lloq,
      shape = caption,
      y2axis = (dimension == "fraction"),
      groupby = dimension
    ),
    metaData = metaData,
    yScale = AxisScales$log,
    yScaleArgs = list(limits = c(0.01, NA)),
    y2Scale = AxisScales$linear,
    y2ScaleArgs = list(limits = c(0, 1.1)),
    groupAesthetics = c("color", "fill")
  ) +
    theme(
      axis.title.y.right = element_text(angle = 90),
      legend.position = "top"
    ) +
    guides(shape = guide_legend(order = 2))

  vdiffr::expect_doppelganger(
    title = "secAxis linLog",
    fig = fig
  )

  fig <- plotTimeProfile(
    data = simData,
    observedData = obsData |>
      dplyr::filter(dimension != "fraction"),
    mapping = aes(
      x = time,
      y = values,
      error_relative = error_relative,
      lloq = lloq,
      shape = caption,
      y2axis = (dimension == "fraction"),
      groupby = dimension
    ),
    metaData = metaData,
    yScale = AxisScales$log,
    yScaleArgs = list(limits = c(0.01, NA)),
    y2Scale = AxisScales$linear,
    y2ScaleArgs = list(limits = c(0, 1.1)),
    groupAesthetics = c("color", "fill")
  ) +
    theme(
      axis.title.y.right = element_text(angle = 90),
      legend.position = "top"
    ) +
    guides(shape = guide_legend(order = 2))

  vdiffr::expect_doppelganger(
    title = "secAxis linLog only simulation",
    fig = fig
  )

  metaData <- list(
    time = list(
      dimension = "Time",
      unit = "h"
    ),
    y2 = list(
      dimension = "Concentration",
      unit = "mg/l"
    ),
    values = list(
      dimension = "Fraction",
      unit = ""
    )
  )

  fig <- plotTimeProfile(
    data = simData,
    observedData = obsData,
    mapping = mapping <- aes(
      x = time,
      y = values,
      shape = caption,
      error_relative = error_relative,
      lloq = lloq,
      y2axis = (dimension != "fraction"),
      color = dimension,
      linetype = dimension
    ),
    metaData = metaData,
    y2Scale = AxisScales$log,
    y2ScaleArgs = list(limits = c(0.01, NA)),
    yScale = AxisScales$linear,
    yScaleArgs = list(limits = c(0, 1)),
  ) +
    theme(
      axis.title.y.right = element_text(angle = 90),
      legend.position = "top",
      legend.title = element_blank()
    )


  vdiffr::expect_doppelganger(
    title = "secAxis logLin",
    fig = fig
  )
})


test_that("plotTimeProfile works with formula as aesthic", {
  obsData <- data.table(
    x = rep(seq(1, 10), 2),
    y = c(rnorm(10, mean = 1), rnorm(10, mean = 2)),
    dose = rep(c(1, 2), each = 10)
  )

  mapping <- aes(
    x = x,
    y = y / dose,
    color = as.factor(dose)
  )


  expect_no_error(plotTimeProfile(
    data = obsData,
    observedData = obsData,
    mapping = aes(
      x = x,
      y = y / dose,
      y2axis = dose == 1,
      color = as.factor(dose)
    ),
    yScale = AxisScales$log
  ))
})
ospsuite.plots::resetDefaults(oldDefaults)
