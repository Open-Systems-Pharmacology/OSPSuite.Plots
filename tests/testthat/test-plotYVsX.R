# setDefaults
# Set watermark option before calling setDefaults since it's no longer in defaults
options(ospsuite.plots.watermark_enabled = TRUE)
oldDefaults <- ospsuite.plots::setDefaults()

test_that("plot Residuals vs Covariate works", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  data <- exampleDataCovariates |>
    dplyr::filter(SetID == "DataSet2") |>
    dplyr::select(c("ID", "Age", "Obs", "gsd", "Pred", "Sex"))

  metaData <- attr(exampleDataCovariates, "metaData")
  metaData <- metaData[intersect(names(data), names(metaData))]

  fig <- plotResVsCov(
    data = data, mapping = aes(
      x = Age,
      predicted = Pred,
      observed = Obs,
      groupby = Sex
    ),
    addRegression = TRUE
  )

  vdiffr::expect_doppelganger(
    title = "plotResVsCov",
    fig = fig
  )
})

test_that("plot Observed vs Predicted works", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  data <- exampleDataCovariates |>
    dplyr::filter(SetID == "DataSet2") |>
    dplyr::select(c("ID", "Obs", "gsd", "Pred", "Sex"))

  lloqData <- signif(quantile(data$Obs, probs = 0.1), 1)

  data <- data |>
    dplyr::mutate(lloq = lloqData) |>
    dplyr::mutate(Obs = ifelse(Obs <= lloq, lloq / 2, Obs))


  metaData <- attr(exampleDataCovariates, "metaData")
  metaData <- metaData[intersect(names(data), names(metaData))]

  fig <- plotPredVsObs(
    data = data,
    mapping = aes(
      x = Obs,
      y = Pred,
      lloq = lloq,
      groupby = Sex,
      error_relative = gsd,
      ymin = Pred / 1.1,
      ymax = Pred * 1.1
    ),
    metaData = metaData,
    addRegression = TRUE
  )

  vdiffr::expect_doppelganger(
    title = "res",
    fig = fig
  )
})

test_that("plotRatioVsCov works", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  dDIdata <- exampleDataCovariates |>
    dplyr::filter(SetID == "DataSet3") |>
    dplyr::select(c("ID", "Obs", "Pred"))


  dDImetaData <- list(
    Obs = list(
      dimension = "DDI AUC Ratio",
      unit = ""
    ),
    Pred = list(
      dimension = "DDI AUC Ratio",
      unit = ""
    )
  )

  fig <- plotRatioVsCov(
    data = dDIdata,
    mapping = aes(
      x = Obs,
      predicted = Pred,
      observed = Obs,
      groupby = as.character(ID)
    ),
    metaData = dDImetaData,
    addGuestLimits = TRUE,
    deltaGuest = 1.2
  )

  vdiffr::expect_doppelganger(
    title = "plotRatioVsCov",
    fig = fig
  )
})

test_that("getCountsWithin works for Ratio", {
  pkRatioData <- exampleDataCovariates |>
    dplyr::filter(SetID == "DataSet1") |>
    dplyr::select(-c("SetID"))

  metaData <- attr(exampleDataCovariates, "metaData")
  metaData <- metaData[intersect(names(pkRatioData), names(metaData))]

  plotObject <- plotRatioVsCov(
    data = pkRatioData,
    mapping = aes(
      x = Age,
      y = Ratio,
      groupby = Sex
    ),
    metaData = metaData,
  )


  vdiffr::expect_doppelganger(
    title = "pk Ratio plot",
    fig = plotObject
  )

  expect_equal(as.vector(unlist(plotObject$countsWithin[1, c(4, 6)])), expected = c(0.48, 0.64))

  expect_equal(plotObject$countsWithin$`1.5 fold Fraction`, expected = c(0.48, 0.44, 0.52))
})

test_that("getCountsWithin works for Guest Criteria", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  # Load example
  dDIdata <- exampleDataCovariates |>
    dplyr::filter(SetID == "DataSet3") |>
    dplyr::select(c("ID", "Obs", "Pred")) |>
    dplyr::mutate(Type = ifelse(ID <= 5, "A", "B"))

  dDImetaData <- list(
    Obs = list(
      dimension = "DDI AUC Ratio",
      unit = ""
    ),
    Pred = list(
      dimension = "DDI AUC Ratio",
      unit = ""
    )
  )

  plotObject <- plotRatioVsCov(
    data = dDIdata,
    mapping = aes(
      x = Obs,
      predicted = Pred,
      observed = Obs,
      groupby = Type
    ),
    addGuestLimits = TRUE,
    comparisonLineVector = getFoldDistanceList(2),
    deltaGuest = 1
  )

  expect_equal(plotObject$countsWithin$`guest criteria Fraction`, expected = c(0.6, 0.6, 0.6))

  plotObjectDiag <- plotPredVsObs(
    data = dDIdata,
    metaData = dDImetaData,
    mapping = aes(
      predicted = Pred,
      observed = Obs,
      groupby = Type
    ),
    addGuestLimits = TRUE,
    comparisonLineVector = getFoldDistanceList(2)
  )

  expect_equal(plotObject$countsWithin, plotObjectDiag$countsWithin)

  plotObjectUngrouped <- plotPredVsObs(
    data = dDIdata,
    metaData = dDImetaData,
    mapping = aes(
      predicted = Pred,
      observed = Obs
    ),
    addGuestLimits = TRUE,
    comparisonLineVector = getFoldDistanceList(2)
  )

  expect_equal(plotObjectUngrouped$countsWithin$Fraction, c(1, 0.6, 1))
})
test_that("adjust lines works withot error", {
  data <- exampleDataCovariates |>
    dplyr::filter(SetID == "DataSet2") |>
    dplyr::select(c("ID", "Age", "Obs", "gsd", "Pred", "Sex"))

  # case with lines which are no interval
  expect_no_error(plotResVsCov(
    data = data,
    mapping = aes(
      x = Age,
      predicted = Pred,
      observed = Obs,
      groupby = Sex
    ),
    comparisonLineVector = list(zero = 0, "lower limit" = -0.25, "upper limit" = 0.25)
  ))

  # case with unnamed intervals
  expect_no_error(plotPredVsObs(
    data = data,
    mapping = aes(
      x = Obs,
      y = Pred,
      groupby = Sex
    ),
    comparisonLineVector = unname(getFoldDistanceList(c(1.2, 1.5))),
    geomComparisonLineAttributes = list(linetype = "dotted")
  ))
})

ospsuite.plots::resetDefaults(oldDefaults)
