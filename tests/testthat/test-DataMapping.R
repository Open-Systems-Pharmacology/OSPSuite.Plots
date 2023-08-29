test_that("adjustGroupAesthetics works", {
  simData1 <- exampleDataTimeProfile %>%
    dplyr::filter(SetID == "DataSet1") %>%
    dplyr::filter(Type == "simulated") %>%
    dplyr::select(c("time", "values", "maxValues", "minValues", "caption"))


  simAes <- aes(
    x = time,
    y = values,
    group = caption
  )

  simDataMatch <- MappedData$new(
    data = simData1,
    mapping = simAes,
    groupAesthetics = c("colour", "fill", "linetype", "shape")
  )

  expect_named(simDataMatch$mapping,
    expected = c("x", "y", "group", "colour", "fill", "linetype", "shape")
  )
})


test_that("getAestheticsForGeom works", {
  simData1 <- exampleDataTimeProfile %>%
    dplyr::filter(SetID == "DataSet1") %>%
    dplyr::filter(Type == "simulated") %>%
    dplyr::select(c("time", "values", "maxValues", "minValues", "caption"))

  simAes <- aes(
    x = time,
    y = values,
    group = caption
  )

  lineMapping <- MappedData$new(
    data = simData1,
    mapping = simAes,
    groupAesthetics = c("colour", "fill", "linetype", "shape")
  )$getAestheticsForGeom(
    geom = "line",
    geomAttributes = list()
  )

  expect_named(lineMapping,
    expected = c("colour", "group", "linetype", "x", "y")
  )
})


test_that("adjustForLLOQMatch works", {
  # Observed data 1
  obsData <- exampleDataTimeProfile %>%
    dplyr::filter(SetID == "DataSet3") %>%
    dplyr::filter(Type == "observed") %>%
    dplyr::filter(dimension == "concentration") %>%
    dplyr::select(c("time", "values", "caption", "lloq", "error_relativ"))


  obsAes <- aes(
    x = time,
    y = values,
    group = caption,
    error = error_relativ,
    lloq = lloq
  )



  obsDataMatch <- MappedData$new(
    data = obsData,
    mapping = obsAes,
    groupAesthetics = c("colour", "fill", "linetype", "shape")
  )


  expect_true("isLLOQ.i" %in% names(obsDataMatch$data))

  expect_true(obsDataMatch$hasLLOQMatch)



  lloqMapping <- obsDataMatch$getAestheticsForGeom(
    geom = "hvline",
    geomAttributes = list()
  )

  expect_equal(names(lloqMapping), expected = c("colour", "linetype", "yintercept"))
})


test_that("adjustDataForMDV works", {
  # Observed data 1
  obsData <- exampleDataTimeProfile %>%
    dplyr::filter(SetID == "DataSet3") %>%
    dplyr::filter(Type == "observed") %>%
    dplyr::filter(dimension == "concentration") %>%
    dplyr::select(c("time", "values", "caption"))


  obsData$mdv <- obsData$values > 2

  # Define Data Mapping
  obsAes <- aes(
    x = time,
    y = values,
    mdv = mdv
  )


  obsDataMatch <- MappedData$new(
    data = obsData,
    mapping = obsAes,
    groupAesthetics = c("colour", "fill", "linetype", "shape")
  )

  expect_equal(nrow(obsDataMatch$data), expected = 12)
})



test_that("adjust secondary y axis scaling works lin to lin", {
  obsData <- data.table(
    x = c(1, 2, 3, 1, 2, 3),
    y = c(200, 300, 400, 10, 11, 12),
    y2axis = c(0, 0, 0, 1, 1, 1)
  )


  mapping <- aes(
    x = x,
    y = y,
    y2axis = as.logical(y2axis)
  )


  dataMatch <- MappedDataTimeProfile$new(
    mapping = mapping,
    data = obsData, scaleOfDirection = "linear", scaleOfSecondaryAxis = "linear"
  )
  expect_true(dataMatch$requireDualAxis)
  expect_equal(dataMatch$ylimits,
    expected = range(obsData[y2axis == 0]$y)
  )
  expect_equal(dataMatch$y2limits,
    expected = range(obsData[y2axis == 1]$y)
  )



  dataMatch <- dataMatch$scaleDataForSecondaryAxis()

  expect_equal(dataMatch$dataForPlot[dataMatch$dataForPlot$y2axis == 1, ]$y,
    expected = dataMatch$dataForPlot[dataMatch$dataForPlot$y2axis == 0, ]$y
  )
})



test_that("adjust secondary y axis scaling works log to log", {
  obsData <- data.table(
    x = c(1, 2, 3, 1, 2, 3),
    y = c(2, 20, 200, 0.5, 50, 5000),
    y2axis = c(0, 0, 0, 1, 1, 1)
  )


  mapping <- aes(
    x = x,
    y = y,
    y2axis = as.logical(y2axis)
  )


  dataMatch <- MappedDataTimeProfile$new(
    mapping = mapping,
    data = obsData, scaleOfDirection = "log", scaleOfSecondaryAxis = "log"
  )
  expect_true(dataMatch$requireDualAxis)
  expect_equal(dataMatch$ylimits,
    expected = range(obsData[y2axis == 0]$y)
  )
  expect_equal(dataMatch$y2limits,
    expected = range(obsData[y2axis == 1]$y)
  )


  dataMatch <- dataMatch$scaleDataForSecondaryAxis()

  expect_equal(dataMatch$dataForPlot[dataMatch$dataForPlot$y2axis == 1, ]$y,
    expected = dataMatch$dataForPlot[dataMatch$dataForPlot$y2axis == 0, ]$y
  )
})




test_that("adjust secondary y axis scaling works lin to log", {
  obsData <- data.table(
    x = c(1, 2, 3, 1, 2, 3),
    y = c(2, 20, 200, 10, 11, 12),
    y2axis = c(0, 0, 0, 1, 1, 1)
  )


  mapping <- aes(
    x = x,
    y = y,
    y2axis = as.logical(y2axis)
  )


  dataMatch <- MappedDataTimeProfile$new(
    mapping = mapping,
    data = obsData, scaleOfDirection = "log", scaleOfSecondaryAxis = "linear"
  )
  expect_true(dataMatch$requireDualAxis)
  expect_equal(dataMatch$ylimits,
    expected = range(obsData[y2axis == 0]$y)
  )
  expect_equal(dataMatch$y2limits,
    expected = range(obsData[y2axis == 1]$y)
  )


  dataMatch <- dataMatch$scaleDataForSecondaryAxis()

  expect_equal(dataMatch$dataForPlot[dataMatch$dataForPlot$y2axis == 1, ]$y,
    expected = dataMatch$dataForPlot[dataMatch$dataForPlot$y2axis == 0, ]$y
  )
})

test_that("adjust secondary y axis scaling works log to lin", {
  obsData <- data.table(
    x = c(1, 2, 3, 1, 2, 3),
    y = c(10, 11, 12, 2, 20, 200),
    y2axis = c(0, 0, 0, 1, 1, 1)
  )


  mapping <- aes(
    x = x,
    y = y,
    y2axis = as.logical(y2axis)
  )


  dataMatch <- MappedDataTimeProfile$new(
    mapping = mapping,
    data = obsData, scaleOfDirection = "linear", scaleOfSecondaryAxis = "log"
  )
  expect_true(dataMatch$requireDualAxis)
  expect_equal(dataMatch$ylimits,
    expected = range(obsData[y2axis == 0]$y)
  )
  expect_equal(dataMatch$y2limits,
    expected = range(obsData[y2axis == 1]$y)
  )


  dataMatch <- dataMatch$scaleDataForSecondaryAxis()

  expect_equal(dataMatch$dataForPlot[dataMatch$dataForPlot$y2axis == 1, ]$y,
    expected = dataMatch$dataForPlot[dataMatch$dataForPlot$y2axis == 0, ]$y
  )
})
