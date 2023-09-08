# setDefaults
oldDefaults <- ospsuite.plots::setDefaults()
theme_update(legend.position = "top")
theme_update(legend.title = element_blank())


test_that("plot histogram works for frequency distribution fit", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  histData <- exampleDataCovariates %>%
    dplyr::filter(SetID == "DataSet1") %>%
    dplyr::select(c("ID", "Sex", "Age", "AgeBin", "Ratio"))

  metaData <- attr(exampleDataCovariates, "metaData")
  metaData <- metaData[intersect(names(histData), names(metaData))]


  vdiffr::expect_doppelganger(
    title = "frequencyDistribution",
    fig = plotHistogram(
      data = histData,
      mapping = aes(
        x = Ratio,
        fill = Sex,
      ),
      metaData = metaData,
      geomHistAttributes = utils::modifyList(
        getDefaultGeomAttributes("Bar"),
        list(position = "stack")
      ),
      distribution = "normal"
    )
  )
})



test_that("plot histogram works for absoulte distribution fit on logscale", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")


  histData_distr <- exampleDataCovariates %>%
    dplyr::filter(SetID == "DataSet2") %>%
    dplyr::select(c("ID", "AgeBin", "Sex", "Obs"))

  metaData <- attr(exampleDataCovariates, "metaData")
  metaData_distr <- metaData[intersect(names(histData_distr), names(metaData))]


  vdiffr::expect_doppelganger(
    title = "logScaleDistribution",
    fig = plotHistogram(
      data = histData_distr,
      mapping = aes(
        x = Obs,
        fill = Sex,
      ),
      metaData = metaData_distr,
      xscale = "log",
      distribution = "norm",
      meanFunction = "none"
    )
  )
})


test_that("plot histogram works for categoricalData", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  histData <- exampleDataCovariates %>%
    dplyr::filter(SetID == "DataSet1") %>%
    dplyr::select(c("ID", "Sex", "Age", "AgeBin", "Ratio"))

  metaData <- attr(exampleDataCovariates, "metaData")
  metaData <- metaData[intersect(names(histData), names(metaData))]


  vdiffr::expect_doppelganger(
    title = "barPlot",
    fig = plotHistogram(
      data = histData,
      mapping = mapping <- aes(
        x = AgeBin,
        fill = Sex
      ),
      metaData = metaData
    )
  )
})
