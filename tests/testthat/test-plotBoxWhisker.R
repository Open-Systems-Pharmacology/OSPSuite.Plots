# setDefaults
oldDefaults <- ospsuite.plots::setDefaults()

test_that("plotWhisker works", {
  pkRatioData <- exampleDataCovariates %>%
    dplyr::filter(SetID == "DataSet1") %>%
    dplyr::mutate(SetID = NULL) %>%
    dplyr::group_by(Country) %>%
    dplyr::mutate(meanAge = round(mean(Age), 2))

  pkRatioMetaData <- attr(exampleDataCovariates, "metaData")
  pkRatioMetaData[["meanAge"]] <- pkRatioMetaData[["Age"]]
  pkRatioMetaData <- pkRatioMetaData[intersect(names(pkRatioData), names(pkRatioMetaData))]


  myStatFun <- function(y) {
    r <- stats::quantile(y, probs = c(0.1, 0.25, 0.5, 0.75, 0.9), names = FALSE, na.rm = TRUE)
    names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
    return(r)
  }


  myStatFunOutlier <-
    function(x) {
      q <- stats::quantile(x, probs = c(0.1, 0.9), names = FALSE, na.rm = TRUE)
      pp <- subset(x, x < q[1] |
        x > q[2])
      if (length(pp) < 1) {
        return(as.double(NA))
      } else {
        return(pp)
      }
    }


  vdiffr::expect_doppelganger(
    title = "with outlier",
    fig = plotBoxWhisker(
      data = pkRatioData,
      metaData = pkRatioMetaData,
      mapping = aes(
        x = Sex,
        y = Ratio,
        fill = Country
      ),
      outliers = TRUE,
      statFun = myStatFun,
      statFunOutlier = myStatFunOutlier
    ) +
      labs(
        tag = "B",
        caption = "Whisker indicate 80% range (10th - 90th percentile)
       and outlier indicate all measurements outside whiskers"
      )
  )


  vdiffr::expect_doppelganger(
    title = "numeric",
    fig = plotBoxWhisker(
      mapping = aes(
        x = meanAge,
        groupby = meanAge,
        y = Ratio,
        fill = Country
      ),
      data = pkRatioData,
      metaData = pkRatioMetaData,
      xscale.args = list(limits = c(29, 31))
    )
  )
})




ospsuite.plots::resetDefaults(oldDefaults)
