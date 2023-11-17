# setDefaults
oldDefaults <- ospsuite.plots::setDefaults()

test_that("plot Residuals vs Covariate works", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  data <- exampleDataCovariates %>%
    dplyr::filter(SetID == "DataSet2") %>%
    dplyr::select(c("ID", "Age", "Obs", "gsd", "Pred", "Sex"))

  metaData <- attr(exampleDataCovariates, "metaData")
  metaData <- metaData[intersect(names(data), names(metaData))]


  vdiffr::expect_doppelganger(
    title = "plotResVsCov",
    fig = plotResVsCov(
      data = data, mapping = aes(
        x = Age,
        predicted = Pred,
        observed = Obs,
        groupby = Sex
      ),
      addRegression = TRUE
    )
  )
})



test_that("plot Observed vs Predicted works", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  data <- exampleDataCovariates %>%
    dplyr::filter(SetID == "DataSet2") %>%
    dplyr::select(c("ID", "Obs", "gsd", "Pred", "Sex"))

  LLOQ <- signif(quantile(data$Obs, probs = 0.1), 1)

  data <- data %>%
    dplyr::mutate(lloq = LLOQ) %>%
    dplyr::mutate(Obs = ifelse(Obs <= lloq, lloq / 2, Obs))


  metaData <- attr(exampleDataCovariates, "metaData")
  metaData <- metaData[intersect(names(data), names(metaData))]


  vdiffr::expect_doppelganger(
    title = "res",
    fig = plotPredVsObs(
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
  )
})




test_that("plotRatioVsCov works", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  DDIdata <- exampleDataCovariates %>%
    dplyr::filter(SetID == "DataSet3") %>%
    dplyr::select(c("ID", "Obs", "Pred"))


  DDImetaData <- list(
    Obs = list(
      dimension = "DDI AUC Ratio",
      unit = ""
    ),
    Pred = list(
      dimension = "DDI AUC Ratio",
      unit = ""
    )
  )


  vdiffr::expect_doppelganger(
    title = "plotRatioVsCov",
    fig = plotRatioVsCov(
      data = DDIdata,
      mapping = aes(
        x = Obs,
        predicted = Pred,
        observed = Obs,
        groupby = as.character(ID)
      ),
      metaData = DDImetaData,
      addGuestLimits = TRUE,
      deltaGuest = 1.2
    )
  )
})



test_that("getCountsWithin works for Ratio", {
  pkRatioData <- exampleDataCovariates %>%
    dplyr::filter(SetID == "DataSet1") %>%
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

  expect_equal(plotObject$countsWithin$Fraction[c(2, 3)], expected = c(0.48, 0.64))

  PKRatioMeasure_Sex <- getCountsWithin(
    data = pkRatioData,
    yColumn = "Ratio",
    groups = c("Sex")
  )

  expect_equal(PKRatioMeasure_Sex$`1.5 fold Fraction`, expected = c(0.44, 0.52))
})



test_that("getCountsWithin works for Guest Criteria", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")


  # Load example
  DDIdata <- exampleDataCovariates %>%
    dplyr::filter(SetID == "DataSet3") %>%
    dplyr::select(c("ID", "Obs", "Pred")) %>%
    dplyr::mutate(Type = ifelse(ID <= 5, 'A','B'))

  plotObject <- plotRatioVsCov(
    data = DDIdata,
    mapping = aes(
      x = Obs,
      predicted = Pred,
      observed = Obs,
      groupby = Type
    ),
    addGuestLimits = TRUE,
    comparisonLineVector = NULL,
    deltaGuest = 1
  )

  # check overall
  expect_equal(plotObject$countsWithin$Fraction, expected = c(1, 0.6))

  # check by group
  countsGroup = getCountsWithin(
    data = plotObject$data,
    xColumn = 'Obs',
    yColumn = 'residuals.i',
    groups = c("Type"),
    addGuestLimits = TRUE,
    deltaGuest = 1,
    foldDistance = NULL
  )

  expect_equal(countsGroup$`guest criteria Fraction`, expected = c(0.6, 0.6))

})




ospsuite.plots::resetDefaults(oldDefaults)
