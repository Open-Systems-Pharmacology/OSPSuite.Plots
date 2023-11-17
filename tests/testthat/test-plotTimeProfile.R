# setDefaults
oldDefaults <- ospsuite.plots::setDefaults()
theme_update(legend.position = "top")
theme_update(legend.title = element_blank())


test_that("plotTimeProfile works basic", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")


  simData <- exampleDataTimeProfile %>%
    dplyr::filter(SetID %in% c("DataSet1", "DataSet2")) %>%
    dplyr::filter(Type == "simulated") %>%
    dplyr::select(c("time", "values", "maxValues", "minValues", "caption"))

  obsData <- exampleDataTimeProfile %>%
    dplyr::filter(SetID %in% c("DataSet1", "DataSet2")) %>%
    dplyr::filter(Type == "observed") %>%
    dplyr::select(c("time", "values", "maxValues", "minValues", "caption"))

  metaData <- attr(exampleDataTimeProfile, "metaData")


  vdiffr::expect_doppelganger(
    title = "basic",
    fig = plotTimeProfile(
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
    )
  )
})



test_that("plotTimeProfile works mapping observed plot", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")


  simData <- exampleDataTimeProfile %>%
    dplyr::filter(SetID %in% c("DataSet1", "DataSet2")) %>%
    dplyr::filter(Type == "simulated") %>%
    dplyr::select(c("time", "values", "minValues", "maxValues", "caption"))


  obsData <- exampleDataTimeProfile %>%
    dplyr::filter(SetID %in% c("DataSet1", "DataSet2")) %>%
    dplyr::filter(Type == "observed") %>%
    dplyr::select(c("time", "values", "sd", "maxValues", "minValues", "caption"))

  metaData <- attr(exampleDataTimeProfile, "metaData")


  mapSimulatedAndObserved <- data.frame(
    simulated = rev(unique(simData$caption)),
    observed = unique(obsData$caption)
  )


  vdiffr::expect_doppelganger(
    title = "mapped-observed-and-simulated",
    fig = plotTimeProfile(
      data = simData,
      observedData = obsData,
      metaData = metaData,
      mapping <- aes(
        x = time,
        y = values,
        groupby = caption
      ),
      mapSimulatedAndObserved = mapSimulatedAndObserved
    )
  )



  obsData <- exampleDataTimeProfile %>%
    dplyr::filter(SetID %in% c("DataSet1")) %>%
    dplyr::filter(Type == "observed") %>%
    dplyr::select(c("time", "values", "sd", "maxValues", "minValues", "caption"))


  mapSimulatedAndObserved <- data.frame(
    simulated = unique(simData$caption),
    observed = c(unique(obsData$caption), "")
  )


  vdiffr::expect_doppelganger(
    title = "mapped-observed-and-simulated",
    fig = plotTimeProfile(
      data = simData,
      observedData = obsData,
      metaData = metaData,
      mapping <- aes(
        x = time,
        y = values,
        groupby = caption
      ),
      mapSimulatedAndObserved = mapSimulatedAndObserved
    )
  )
})

test_that("plotTimeProfile works lloq", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")



  simData <- exampleDataTimeProfile %>%
    dplyr::filter(SetID == c("DataSet3")) %>%
    dplyr::filter(Type == "simulated") %>%
    dplyr::filter(dimension == "concentration") %>%
    dplyr::select(c("time", "values", "caption"))

  obsData <- exampleDataTimeProfile %>%
    dplyr::filter(SetID == "DataSet3") %>%
    dplyr::filter(Type == "observed") %>%
    dplyr::filter(dimension == "concentration") %>%
    dplyr::select(c("time", "values", "caption", "lloq", "error_relative"))

  obsData$lloq[3] <- NA
  obsData$lloq[4] <- obsData$lloq[4] / 2

  metaData <- attr(exampleDataTimeProfile, "metaData")

  vdiffr::expect_doppelganger(
    title = "with lloq",
    fig = plotTimeProfile(
      data = simData,
      observedData = obsData,
      metaData = metaData,
      mapping = aes(
        x = time,
        y = values,
        groupby = caption,
        error_relative = error_relative,
        lloq = lloq
      ),
      groupAesthetics = c("color", "shape", "fill"),
      yscale = "log",
      yscale.args = list(limits = c(0.01, NA)),
      geomLineAttributes = list(color = "black")
    )
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

  simData <- exampleDataTimeProfile %>%
    dplyr::filter(SetID == "DataSet3") %>%
    dplyr::filter(Type == "simulated") %>%
    dplyr::select(c("time", "values", "dimension", "caption"))


  obsData <- exampleDataTimeProfile %>%
    dplyr::filter(SetID == "DataSet3") %>%
    dplyr::filter(Type == "observed") %>%
    dplyr::select(c("time", "values", "dimension", "caption", "lloq", "error_relative"))


  vdiffr::expect_doppelganger(
    title = "secAxis linLog",
    fig = plotTimeProfile(
      data = simData,
      observedData = obsData,
      mapping = mapping <- aes(
        x = time,
        y = values,
        shape = caption,
        error_relative = error_relative,
        lloq = lloq,
        y2axis = (dimension == "fraction"),
        color = dimension,
        linetype = dimension
      ),
      metaData = metaData,
      yscale = "log",
      yscale.args = list(limits = c(0.01, NA)),
      y2scale = "linear",
      y2scale.args = list(limits = c(0, 1)),
    ) +
      theme(axis.title.y.right = element_text(angle = 90))
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


  vdiffr::expect_doppelganger(
    title = "secAxis logLin",
    fig = plotTimeProfile(
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
      y2scale = "log",
      y2scale.args = list(limits = c(0.01, NA)),
      yscale = "linear",
      yscale.args = list(limits = c(0, 1)),
    ) +
      theme(axis.title.y.right = element_text(angle = 90))
  )
})

ospsuite.plots::resetDefaults(oldDefaults)
