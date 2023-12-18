#' @title generates residual plots vs covariate
#' @description
#' This functions is a wrapper  for `plotYVsX` with adjusted input parameter:
#'
#'
#'  parameters  fixed and not settable
#'  * `observedDataDirection = 'y'`
#'  * `addLinesDiagnonal = TRUE`
#'  * `addGuestLimits = FALSE` (use `plotRatio()` if needed)
#'
#'
#' For details and examples see the vignettes:
#' * \code{vignette("Goodness of fit", package = "ospsuite.plots")}
#' * \code{vignette("ospsuite.plots", package = "ospsuite.plots")}
#'
#' @param ...  passed on to plotYVsX
#' @inheritParams plotYVsX
#' @inheritDotParams plotYVsX
#'
#'
#' @return A `ggplot` object
#' @export
#' @family plot functions
plotResVsCov <- function(data,
                         mapping,
                         residualScale = "log",
                         comparisonLineVector = 0,
                         yscale = "linear",
                         ...) {
  # Validation

  plotObject <- plotYVsX(
    data = data,
    mapping = mapping,
    addGuestLimits = FALSE,
    residualScale = residualScale,
    yscale = yscale,
    comparisonLineVector = comparisonLineVector,
    observedDataDirection = "y",
    addLinesDiagnonal = FALSE,
    ...
  )

  return(plotObject)
}

#' @title generates plots of ratios vs covariate
#' @description
#'  This functions is a wrapper  for `plotYVsX` with adjusted input parameter:
#'
#'
#'  parameters below are fixed and not settable
#'  * `residualScale = "ratio"`
#'  * `observedDataDirection = 'y'`
#'  * `addLinesDiagnonal = FALSE`
#'
#'
#' For details and examples see the vignettes:
#' * \code{vignette("Goodness of fit", package = "ospsuite.plots")}
#' * \code{vignette("ospsuite.plots", package = "ospsuite.plots")}
#'
#' @inheritParams plotYVsX
#' @inheritDotParams plotYVsX
#'
#'
#' @return A `ggplot` object
#' @export
#' @family plot functions
plotRatioVsCov <- function(data = NULL,
                           mapping = NULL,
                           addGuestLimits = FALSE,
                           yscale = "log",
                           xscale = ifelse(addGuestLimits, "log", "linear"),
                           comparisonLineVector = getFoldDistanceList(c(1.5, 2)),
                           deltaGuest = 1,
                           ...) {
  plotObject <- plotYVsX(
    data = data,
    mapping = mapping,
    comparisonLineVector = comparisonLineVector,
    xscale = xscale,
    yscale = yscale,
    addGuestLimits = addGuestLimits,
    deltaGuest = deltaGuest,
    observedDataDirection = "y",
    addLinesDiagnonal = FALSE,
    residualScale = "ratio",
    ...
  )


  # do quantification
  if (requireNamespace("data.table", quietly = TRUE) &
    (addGuestLimits | !is.null(names(comparisonLineVector)))) {
    pb <- ggplot_build(plotObject)
    iData <- which(unlist(lapply(pb$data, function(x) {
      return(all(c("x", "y", "shape") %in% names(x)))
    })))

    if (length(iData) == 0) stop("Could not find data for counting within limits")
    if (length(iData) > 1) iData <- iData[1]

    if (length(iData) > 0) {
      if (xscale == "log") {
        pb$data[[iData]]$x <- 10^(pb$data[[iData]]$x)
      }
      if (yscale == "log") {
        pb$data[[iData]]$y <- 10^(pb$data[[iData]]$y)
      }

      plotObject$countsWithin <- getCountsWithin(
        data = pb$data[[iData]],
        xColumn = "x",
        yColumn = "y",
        comparisonLineVector = comparisonLineVector,
        addGuestLimits = addGuestLimits,
        deltaGuest = deltaGuest
      )
    } else {
      warning("no datapoints available for quantification")
    }
  }


  return(plotObject)
}


#' @title generates predicted vs observed plots
#' @description
#'  This functions is a wrapper for function `plotYVsX` with adjusted input parameter.
#'
#'  * `residualScale` is fixed to NULL,
#'  * `observedDataDirection` is fixed to 'x'
#'
#' For details and examples see the vignettes:
#' * \code{vignette("Goodness of fit", package = "ospsuite.plots")}
#' * \code{vignette("ospsuite.plots", package = "ospsuite.plots")}
#'
#' @param xyscale  either "linear" or "log" scale of x and y axis
#' @inheritParams plotYVsX
#' @inheritDotParams plotYVsX
#'
#'
#' @return A `ggplot` object
#' @export
#' @family plot functions
plotPredVsObs <- function(data = NULL,
                          mapping = NULL,
                          xyscale = "log",
                          comparisonLineVector = getFoldDistanceList(c(1.5, 2)),
                          asSquarePlot = TRUE,
                          ...) {
  if (!("x" %in% names(mapping)) & "observed" %in% names(mapping)) {
    names(mapping)[names(mapping) == "observed"] <- "x"
  }
  if (!("y" %in% names(mapping)) & "predicted" %in% names(mapping)) {
    names(mapping)[names(mapping) == "predicted"] <- "y"
  }

  plotObject <- plotYVsX(
    data = data,
    mapping = mapping,
    residualScale = NULL,
    comparisonLineVector = comparisonLineVector,
    xscale = xyscale,
    yscale = xyscale,
    observedDataDirection = "x",
    addLinesDiagnonal = TRUE,
    asSquarePlot = asSquarePlot,
    ...
  )

  plotObject$labels$x <-
    paste(c(plotObject$labels$x, "observed"), collapse = "\n")
  plotObject$labels$y <-
    paste(c(plotObject$labels$y, "predicted"), collapse = "\n")


  return(plotObject)
}




#' @title base plot for `plotResVsCov()`,`plotRatiVsCov()` and`plotPredVsObs()`
#' @description
#'
#' For details and examples see the vignettes:
#' \code{vignette("Goodness of fit", package = "ospsuite.plots")}
#' \code{vignette("ospsuite.plots", package = "ospsuite.plots")}
#'
#' @inheritParams plotTimeProfile
#' @param data  A `data.frame` with data to plot
#' @param mapping  a list of aesthetic mappings to use for plot
#' @param geomComparisonLineAttributes A `list` with arguments which are passed on to the
#'             call `ggplot2::hline`  or `ggplot2::abline` to display comparison lines
#' @param geomGuestLineAttributes A `list` with arguments which are passed on to the
#'              call `ggplot2::geom_function` to display guest criteria
#' @param residualScale either "linear","log" or "ratio" scale residuals,
#'        * linear:  residuals are calculated observed - predicted
#'        * log: residuals are calculated log(observed) - log(predicted)
#'        * ratio: residuals are calculated as observed/predicted
#' @param comparisonLineVector either a double vector or a list of double values
#'    if add `addLinesDiagnonal = FALSE` lines will be added as horizontal lines with the
#'    intercept at values of `comparisonLineVector`
#'    If add `addLinesDiagnonal = TRUE` lines will be added as fold distance lines to the identity.
#' @param addLinesDiagnonal A `boolean`which defines direction of comparison lines
#' @param addRegression A `boolean` which activates insertion of regression line
#' @param addGuestLimits A `boolean` which activates insertion of regression line
#' @param deltaGuest Numeric value parameter of Guest function
#' @param labelGuestCriteria label used in legend for guest criteria (default guest criteria)
#' @param asSquarePlot A `boolean` if true plot is returned as square plot with aspect.ratio = 1 and fixed ratios
#' @param observedDataDirection either 'x' or 'y', defines direction of observed data. relevant for
#' aesthetics `lloq`, `error`end `error_relative`
#'
#' @return A `ggplot` object
#' @export
#' @family plot functions
plotYVsX <- function(data,
                     mapping,
                     metaData = NULL,
                     geomPointAttributes = getDefaultGeomAttributes("Point"),
                     geomErrorbarAttributes = getDefaultGeomAttributes("Errorbar"),
                     geomLineAttributes = getDefaultGeomAttributes("Line"),
                     geomGuestLineAttributes = getDefaultGeomAttributes("GuestLine"),
                     geomComparisonLineAttributes = getDefaultGeomAttributes("ComparisonLine"),
                     geomLLOQAttributes = getDefaultGeomAttributes("LLOQ"),
                     groupAesthetics = c("colour", "fill", "shape"),
                     comparisonLineVector = NULL,
                     addRegression = FALSE,
                     addGuestLimits = FALSE,
                     deltaGuest = 1,
                     labelGuestCriteria = "guest criteria",
                     residualScale = NULL,
                     asSquarePlot = FALSE,
                     xscale = "linear",
                     xscale.args = list(),
                     yscale = "log",
                     yscale.args = list(),
                     observedDataDirection = "y",
                     addLinesDiagnonal = TRUE) {
  # Check validity
  checkmate::assertDataFrame(data)
  checkmate::assertList(metaData, types = "list", null.ok = TRUE)

  checkmate::assertList(geomPointAttributes, null.ok = FALSE, min.len = 0)
  checkmate::assertList(geomErrorbarAttributes, null.ok = FALSE, min.len = 0)
  checkmate::assertList(geomLineAttributes, null.ok = FALSE, min.len = 0)
  checkmate::assertList(geomGuestLineAttributes, null.ok = FALSE, min.len = 0)
  checkmate::assertList(geomComparisonLineAttributes, null.ok = FALSE, min.len = 0)
  checkmate::assertList(geomLLOQAttributes, null.ok = FALSE, min.len = 0)

  checkmate::assertCharacter(groupAesthetics, min.len = 0, all.missing = TRUE, null.ok = TRUE)

  if (is.double(comparisonLineVector)) comparisonLineVector <- as.list(comparisonLineVector)
  checkmate::assertList(comparisonLineVector, types = "double", any.missing = FALSE, null.ok = TRUE, min.len = 1)

  checkmate::assertFlag(addRegression)

  checkmate::assertFlag(addGuestLimits)
  checkmate::assertDouble(deltaGuest, lower = 1, len = 1, null.ok = !addGuestLimits)

  checkmate::assertChoice(residualScale, choices = c("linear", "log", "ratio"), null.ok = TRUE)

  checkmate::assertFlag(asSquarePlot)
  checkmate::assertChoice(xscale, choices = c("linear", "log"), null.ok = TRUE)
  checkmate::assertList(xscale.args, null.ok = FALSE, min.len = 0)
  checkmate::assertChoice(yscale, choices = c("linear", "log"), null.ok = TRUE)
  checkmate::assertList(yscale.args, null.ok = FALSE, min.len = 0)

  checkmate::assertChoice(observedDataDirection, choices = c("x", "y"), null.ok = TRUE)


  # data match --------------
  mappedData <- MappedData$new(
    data = data,
    mapping = mapping,
    xlimits = xscale.args$limits,
    ylimits = yscale.args$limits,
    direction = observedDataDirection,
    isObserved = TRUE,
    groupAesthetics = groupAesthetics,
    residualScale = residualScale,
    residualAesthetic = "y"
  )
  mappedData$addMetaData(metaData = metaData)

  #-  initialize plot
  plotObject <- initializePlot(mappedData = mappedData)
  if (mappedData$hasResidualMapping) {
    plotObject <- plotObject +
      labs(y = mappedData$residualLabel)
  }

  #----- Build layers -----
  # Each new layer is added on top of previous
  # Thus, scatter points are added as last layer to prevent them being hidden by lines or errorbars
  # 1- Horizontal lines

  # add Horizontal  or diagonal lines
  if (is.list(comparisonLineVector)) {
    plotObject <- addComparisonLines(
      plotObject = plotObject,
      comparisonLineVector = comparisonLineVector,
      addLinesDiagnonal = addLinesDiagnonal,
      geomLineAttributes = geomComparisonLineAttributes,
      xyscale = xscale
    )
  }

  # add DDI ratio limits by Guest criteria
  if (addGuestLimits) {
    plotObject <- addGuestLayer(
      plotObject = plotObject,
      deltaGuest = deltaGuest,
      labelGuestCriteria = labelGuestCriteria,
      addLinesDiagnonal = addLinesDiagnonal,
      geomGuestLineAttributes = geomGuestLineAttributes
    )
  }


  # add Error bars
  if (all(c("xmin", "xmax") %in% names(mappedData$mapping))) {
    plotObject <- plotObject +
      do.call(
        what = ggplot2::geom_errorbar,
        args = c(
          list(
            na.rm = TRUE,
            orientation = "y"
          ),
          geomErrorbarAttributes
        )
      )
  }

  if (all(c("ymin", "ymax") %in% names(mappedData$mapping))) {
    plotObject <- plotObject +
      do.call(
        what = ggplot2::geom_errorbar,
        args = c(
          list(
            na.rm = TRUE,
            orientation = "x"
          ),
          geomErrorbarAttributes
        )
      )
  }



  # Scatter points
  plotObject <- addLayer(
    mappedData = mappedData,
    geom = "point",
    geomAttributes = geomPointAttributes,
    plotObject = plotObject,
    layerToCall = geom_point
  )

  # regression
  if (addRegression) {
    plotObject <- addLayer(
      mappedData = mappedData,
      geom = "smooth",
      geomAttributes = list(
        inherit.aes = FALSE,
        method = "lm",
        formula = "y ~ x"
      ),
      plotObject = plotObject,
      layerToCall = geom_smooth
    )
  }





  # add lloq lines
  if (mappedData$hasLLOQMatch) {
    plotObject <- addLayer(
      mappedData = mappedData,
      geom = "hvline",
      geomAttributes = geomLLOQAttributes,
      plotObject = plotObject,
      layerToCall = geom_vline
    )
  }





  if (asSquarePlot) {
    plotObject <- plotObject +
      theme(aspect.ratio = 1) +
      coord_fixed(ratio = 1)

    xscale.args$limits <- range(mappedData$xlimits, mappedData$ylimits)
    yscale.args$limits <- range(mappedData$xlimits, mappedData$ylimits)
  }


  # set scales ----

  xscale.args <- mappedData$updateScaleArgumentsForTimeUnit(
    scale.args = xscale.args,
    scaleDirection = "x"
  )


  plotObject <- addXYScale(
    plotObject = plotObject,
    xscale = xscale,
    xscale.args = xscale.args,
    yscale = yscale,
    yscale.args = yscale.args
  )


  plotObject <- plotObject +
    guides(linetype = guide_legend(title = NULL, order = 1))

  # fix order of linetype,

  # first comparsion lines, then guest criteria, then any other
  plotObjectBuild <- ggplot_build(plotObject)

  if (any(plotObjectBuild$plot$scales$find("linetype"))) {

    iScale <- which(plotObjectBuild$plot$scales$find("linetype"))
    linetypeLabels <- plotObjectBuild$plot$scales$scales[[iScale]]$get_labels()

    linetypes <- plotObjectBuild$plot$scales$scales[[iScale]]$palette(length(linetypeLabels))


    lineTypeNames <- names(comparisonLineVector)
    if (addGuestLimits) {
      lineTypeNames <- c(lineTypeNames, labelGuestCriteria)
    }
    lineTypeNames <- c(
      lineTypeNames,
      setdiff(
        linetypeLabels,
        lineTypeNames
      )
    )
    names(linetypes) <- lineTypeNames
    plotObject <- plotObject +
      scale_linetype_manual(values = linetypes, breaks = names(linetypes))
  }



  return(plotObject)
}

#' add horizontal or diagonal comparison lines
#' @inheritParams plotYVsX
#' @inheritParams plotPredVsObs
#' @param geomLineAttributes line attributes e.g. color,linetype passed to `ggplot2::geom_hline` or `ggplot2::geom_abline`
#'
#' @keywords internal
#' @return The updated `ggplot` object
addComparisonLines <- function(plotObject,
                               comparisonLineVector,
                               addLinesDiagnonal,
                               geomLineAttributes,
                               xyscale) {
  # initialize  to avoid warnings in check()
  value <- name <- NULL

  # get mapping
  if (addLinesDiagnonal) {
    lineMapping <- switch(xyscale,
      "log" = aes(
        intercept = log10(value),
        slope = 1
      ),
      "linear" = aes(
        intercept = 0,
        slope = value
      )
    )
  } else {
    lineMapping <- aes(yintercept = value)
  }


  # get data for lines
  if (any(!is.null(names(comparisonLineVector)))) {
    lineData <- data.frame()
    for (n in names(comparisonLineVector)) {
      lineData <- rbind(
        lineData,
        data.frame(
          value = comparisonLineVector[[n]],
          name = n
        )
      )
    }
    lineData$name <- factor(lineData$name, levels = names(comparisonLineVector), ordered = TRUE)

    lineMapping <- structure(c(lineMapping, aes(linetype = name)), class = "uneval")

    # use linetype for legend
    if ("linetype" %in% names(geomLineAttributes)) geomLineAttributes$linetype <- NULL
  } else {
    lineData <- data.frame(value = unlist(comparisonLineVector))
  }


  plotObject <- plotObject +
    do.call(
      what = ifelse(addLinesDiagnonal,
        ggplot2::geom_abline,
        ggplot2::geom_hline
      ),
      args = c(
        list(
          data = lineData,
          mapping = lineMapping,
          key_glyph = "path",
          na.rm = TRUE
        ),
        geomLineAttributes
      )
    )

  return(plotObject)
}

#' adds limits calculates by Guest function to ggplot object
#'
#' @param plotObject object to be updated
#' @inheritParams getGuestLimits
#' @param geomGuestLineAttributes  list of arguments passed to geom_fun
#'
#' @keywords internal
#' @return The updated `ggplot` object
addGuestLayer <- function(plotObject,
                          deltaGuest,
                          labelGuestCriteria,
                          addLinesDiagnonal,
                          geomGuestLineAttributes) {
  if ("linetype" %in% names(geomGuestLineAttributes)) {
    geomGuestLineAttributes$linetype <- NULL
  }

  plotObject <- plotObject +
    do.call(
      what = geom_function,
      args = c(
        list(
          mapping = aes(linetype = labelGuestCriteria),
          fun = getGuestLimits,
          args = list(
            deltaGuest = deltaGuest,
            addLinesDiagnonal = addLinesDiagnonal,
            asLower = TRUE
          ),
          inherit.aes = FALSE,
          key_glyph = "path",
          na.rm = TRUE
        ),
        geomGuestLineAttributes
      )
    ) +
    do.call(
      what = geom_function,
      args = c(
        list(
          mapping = aes(linetype = "guest criteria"),
          fun = getGuestLimits,
          args = list(
            deltaGuest = deltaGuest,
            addLinesDiagnonal = addLinesDiagnonal,
            asLower = FALSE
          ),
          inherit.aes = FALSE,
          key_glyph = "path",
          na.rm = TRUE
        ),
        geomGuestLineAttributes
      )
    )

  return(plotObject)
}

#' calculates limits for DDI ratio
#'
#' @inheritParams plotYVsX
#' @param x Numeric values input of Guest function
#' @param asLower function returns lower limit
#'
#' @references
#' <https://dmd.aspetjournals.org/content/39/2/170>
#'
#' @return limit of guest function for x
#' @keywords internal
#'
getGuestLimits <- function(x, deltaGuest = 1, addLinesDiagnonal = FALSE, asLower = TRUE) {
  xSym <- x
  xSym[x < 1] <- 1 / x[x < 1]
  limit <- (deltaGuest + 2 * (xSym - 1)) / xSym
  if (asLower) limit <- 1 / limit

  if (addLinesDiagnonal) limit <- limit * x

  return(limit)
}



#' Counts entries within specific limits
#'
#' @inheritParams plotYVsX
#' @param yColumn y column name for values to count
#' @param xColumn x column name for values to count
#' @param groups  column names to group
#'
#' @return data table with summary
#' @export
getCountsWithin <- function(data,
                            yColumn,
                            xColumn = NULL,
                            comparisonLineVector = getFoldDistanceList(c(1.5, 2)),
                            addGuestLimits = FALSE,
                            deltaGuest = 1,
                            groups = NULL) {
  # initialize variables to avoid warning in check()
  Description <- value <- Number <- name <- NULL # nolint

  checkmate::assertDataFrame(data, null.ok = FALSE, min.rows = 1)
  checkmate::assertNames(names(data), disjunct.from = c("yColumn", "xColumn"))
  checkmate::assertFlag(addGuestLimits, null.ok = FALSE)
  checkmate::assertCharacter(yColumn, null.ok = FALSE, len = 1)
  checkmate::assertCharacter(xColumn, null.ok = !addGuestLimits, len = 1)
  if (is.double(comparisonLineVector)) comparisonLineVector <- as.list(comparisonLineVector)
  checkmate::assertList(comparisonLineVector, types = "double", any.missing = FALSE, null.ok = TRUE, min.len = 1)
  checkmate::assertDouble(deltaGuest, null.ok = !addGuestLimits, len = 1)


  # use data.table functionality
  data.table::setDT(data)

  # define auxiliary function
  countEntriesInBetween <- function(yColumn, xColumn, comparisonLineVector, deltaGuest, addGuestLimits) {
    counts <- list()

    if (addGuestLimits) {
      guestLimits <- c(
        getGuestLimits(
          xColumn,
          deltaGuest = deltaGuest,
          addLinesDiagnonal = FALSE,
          asLower = TRUE
        )
      )
      counts[["guest criteria"]] <- sum(yColumn >= pmin(guestLimits, 1 / guestLimits) &
        yColumn <= pmax(guestLimits, 1 / guestLimits))
    }

    if (!is.null(names(comparisonLineVector))) {
      for (fd in names(comparisonLineVector)) {
        if (length(comparisonLineVector[[fd]]) > 1) {
          counts[[fd]] <- sum(yColumn >= min(comparisonLineVector[[fd]]) &
            yColumn <= max(comparisonLineVector[[fd]]))
        }
      }
    }
    return(counts)
  }


  # if grouping provide one row per group
  if (!is.null(groups)) {
    tmp <- merge(data[, .("Points total" = .N), by = groups],
      data[, as.list(
        countEntriesInBetween(
          xColumn = get(xColumn),
          yColumn = get(yColumn),
          comparisonLineVector = comparisonLineVector,
          addGuestLimits = addGuestLimits,
          deltaGuest = deltaGuest
        )
      ),
      by = groups
      ],
      by = groups
    )

    countsWithin <- tidyr::pivot_longer(
      data = tmp,
      cols = intersect(names(tmp), c(names(comparisonLineVector), "guest criteria")),
      names_to = "Description", values_to = "Number"
    ) %>%
      dplyr::mutate(Fraction = Number / get("Points total")) %>%
      tidyr::pivot_longer(cols = c("Number", "Fraction")) %>%
      dplyr::mutate(Description = paste(Description, name)) %>%
      dplyr::mutate(name = NULL) %>%
      tidyr::pivot_wider(names_from = Description, values_from = value)
  } else {
    # if provide one row per 'fold'

    totalNumber <- data[, .(
      Number = sum(!is.na(get(yColumn))),
      Fraction = 1
    )] %>%
      dplyr::mutate(Description = "Points total") %>%
      data.table::setcolorder("Description")

    tmp <-
      data[, as.list(
        countEntriesInBetween(
          xColumn = get(xColumn),
          yColumn = get(yColumn),
          comparisonLineVector = comparisonLineVector,
          addGuestLimits = addGuestLimits,
          deltaGuest = deltaGuest
        )
      )]
    tmp <- tidyr::pivot_longer(data = tmp, cols = names(tmp), names_to = "Description", values_to = "Number") %>%
      dplyr::mutate(Fraction = Number / totalNumber$Number) %>%
      dplyr::mutate(Description = paste("Points within", Description))


    countsWithin <- rbind(totalNumber,
      tmp,
      fill = TRUE
    )
  }

  return(countsWithin)
}
