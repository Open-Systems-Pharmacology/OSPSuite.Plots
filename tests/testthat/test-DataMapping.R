test_that("adjustGroupAesthetics works", {
  simData1 <- exampleDataTimeProfile %>%
    dplyr::filter(SetID == "DataSet1") %>%
    dplyr::filter(Type == "simulated") %>%
    dplyr::select(c("time", "values", "maxValues", "minValues", "caption"))


  mapping <- aes(
    x = time,
    y = values,
    groupby = caption
  )

  simDataMatch <- MappedData$new(
    data = simData1,
    mapping = mapping,
    groupAesthetics = c("colour", "fill", "linetype", "shape")
  )

  expect_named(simDataMatch$mapping,
    expected = c("x", "y", "colour", "fill", "linetype", "shape", "group")
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
    groupby = caption
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
    dplyr::select(c("time", "values", "caption", "lloq", "error_relative"))


  obsAes <- aes(
    x = time,
    y = values,
    groupby = caption,
    error_relative = error_relative,
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
    data = obsData, scaleOfPrimaryAxis = "linear", scaleOfSecondaryAxis = "linear"
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
    data = obsData, scaleOfPrimaryAxis = "log", scaleOfSecondaryAxis = "log"
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
    data = obsData, scaleOfPrimaryAxis = "log", scaleOfSecondaryAxis = "linear"
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
    data = obsData, scaleOfPrimaryAxis = "linear", scaleOfSecondaryAxis = "log"
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

test_that("grouping for simulation and observed works", {
  simData <- exampleDataTimeProfile %>%
    dplyr::filter(Type == "simulated") %>%
    dplyr::select(c("time", "values", "minValues", "maxValues", "caption", "dimension"))



  simMappedData <- MappedDataTimeProfile$new(
    data = simData,
    mapping = mapping <- aes(
      x = time,
      y = values,
      groupby = caption
    ),
    groupAesthetics = c("colour", "fill", "linetype", "shape"),
    direction = "y",
    isObserved = FALSE,
    groupOrder = c("Simulated Data 2", "Simulated Data 1", "simulated"),
    scaleOfPrimaryAxis = "linear",
    scaleOfSecondaryAxis = "linear",
    ylimits = list(),
    y2limits = list()
  )

  expect_true(is.factor(simMappedData$dataForPlot$groupBy.i))
  expect_equal(
    object = as.character(rlang::quo_get_expr(simMappedData$mapping[["group"]])),
    expected = "groupBy.i"
  )


  expect_error(
    simMappedData <- MappedDataTimeProfile$new(
      data = simData,
      mapping = mapping <- aes(
        x = time,
        y = values,
      ),
      groupAesthetics = c("colour", "fill", "linetype", "shape"),
      direction = "y",
      isObserved = FALSE,
      groupOrder = c("Simulated Data 2", "Simulated Data 1", "simulated"),
      scaleOfPrimaryAxis = "linear",
      scaleOfSecondaryAxis = "linear",
      ylimits = list(),
      y2limits = list()
    )
  )


  simMappedData <- MappedDataTimeProfile$new(
    data = simData,
    mapping = mapping <- aes(
      x = time,
      y = values,
      groupby = interaction(caption, dimension)
    ),
    groupAesthetics = c("colour", "fill", "linetype", "shape"),
    direction = "y",
    isObserved = FALSE,
    groupOrder = c("Simulated Data 2.concentration", "Simulated Data 1.concentration", "simulated.concentration", "simulated.fraction"),
    scaleOfPrimaryAxis = "linear",
    scaleOfSecondaryAxis = "linear",
    ylimits = list(),
    y2limits = list()
  )

  expect_true(is.factor(simMappedData$dataForPlot$groupBy.i))
})
