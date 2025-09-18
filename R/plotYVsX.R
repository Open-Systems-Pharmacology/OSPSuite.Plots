#' @title Generate Residual Plots vs Covariate
#' @description
#' This function is a wrapper for `plotYVsX` with adjusted input parameters.
#'
#' The following parameters are fixed and cannot be set:
#' * `observedDataDirection = 'y'`
#' * `yDisplayAsAbsolute = TRUE`
#' * `addGuestLimits = FALSE` (use `plotRatio()` if needed)
#'
#' For details and examples, see the vignettes:
#' * \code{vignette("Goodness of fit", package = "ospsuite.plots")}
#' * \code{vignette("ospsuite.plots", package = "ospsuite.plots")}
#'
#' @param ... Additional arguments passed to `plotYVsX`.
#' @inheritParams plotYVsX
#' @inheritDotParams plotYVsX
#'
#' @return A `ggplot` object representing the residual plots.
#' @export
#' @family plot functions
plotResVsCov <- function(data,
                         mapping,
                         residualScale = ResidualScales$log,
                         comparisonLineVector = 0,
                         yscale = AxisScales$linear,
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
    yDisplayAsAbsolute = FALSE,
    ...
  )

  return(plotObject)
}
#' @title Generate Plots of Ratios vs Covariate
#' @description
#' This function is a wrapper for `plotYVsX` with adjusted input parameters.
#'
#' The following parameters are fixed and cannot be set:
#' * `residualScale = "ratio"`
#' * `observedDataDirection = 'y'`
#' * `yDisplayAsAbsolute = FALSE`
#'
#' For details and examples, see the vignettes:
#' * \code{vignette("Goodness of fit", package = "ospsuite.plots")}
#' * \code{vignette("ospsuite.plots", package = "ospsuite.plots")}
#'
#' @inheritParams plotYVsX
#' @inheritDotParams plotYVsX
#'
#' @return A `ggplot` object representing the ratio plots.
#' @export
#' @family plot functions
plotRatioVsCov <- function(data = NULL,
                           mapping = NULL,
                           addGuestLimits = FALSE,
                           yscale = AxisScales$log,
                           xscale = ifelse(addGuestLimits, AxisScales$log, AxisScales$linear),
                           comparisonLineVector = getFoldDistanceList(c(1.5, 2)),
                           deltaGuest = 1,
                           ...) {
  yDisplayAsAbsolute <- FALSE

  plotObject <- plotYVsX(
    data = data,
    mapping = mapping,
    comparisonLineVector = comparisonLineVector,
    xscale = xscale,
    yscale = yscale,
    addGuestLimits = addGuestLimits,
    deltaGuest = deltaGuest,
    observedDataDirection = "y",
    yDisplayAsAbsolute = yDisplayAsAbsolute,
    residualScale = ResidualScales$ratio,
    ...
  )

  return(plotObject)
}
#' @title Generate Predicted vs Observed Plots
#' @description
#' This function is a wrapper for `plotYVsX` with adjusted input parameters.
#'
#' The following parameters are fixed:
#' * `residualScale` is fixed to NULL,
#' * `observedDataDirection` is fixed to 'x'
#'
#' For details and examples, see the vignettes:
#' * \code{vignette("Goodness of fit", package = "ospsuite.plots")}
#' * \code{vignette("ospsuite.plots", package = "ospsuite.plots")}
#'
#' @param xyscale Either "linear" or "log" scale for the X and Y axes.
#' @inheritParams plotYVsX
#' @inheritDotParams plotYVsX
#'
#' @return A `ggplot` object representing the predicted vs observed plots.
#' @export
#' @family plot functions
plotPredVsObs <- function(data = NULL,
                          mapping = NULL,
                          xyscale = AxisScales$log,
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
    yDisplayAsAbsolute = TRUE,
    asSquarePlot = asSquarePlot,
    ...
  )

  plotObject$labels$x <-
    paste(c(plotObject$labels$x, "observed"), collapse = "\n")
  plotObject$labels$y <-
    paste(c(plotObject$labels$y, "predicted"), collapse = "\n")

  return(plotObject)
}

#' @title Base Plot for Residuals and Predictions vs Covariates
#' @description
#' This function creates a base plot for `plotResVsCov()`, `plotRatioVsCov()`, and `plotPredVsObs()`.
#'
#' For details and examples, see the vignettes:
#' * \code{vignette("Goodness of fit", package = "ospsuite.plots")}
#' * \code{vignette("ospsuite.plots", package = "ospsuite.plots")}
#'
#' @inheritParams plotTimeProfile
#' @param data A `data.frame` containing the data to plot.
#' @param mapping A list of aesthetic mappings to use for the plot.
#' @param geomComparisonLineAttributes A `list` of arguments passed to `ggplot2::hline` or `ggplot2::abline` to display comparison lines.
#' @param geomGuestLineAttributes A `list` of arguments passed to `ggplot2::geom_function` to display guest criteria.
#' @param residualScale Either "linear", "log", or "ratio" scale for residuals.
#' @param comparisonLineVector A vector defining the comparison lines.
#' @param yDisplayAsAbsolute A boolean that defines the direction of comparison lines.
#' @param addRegression A boolean that activates the insertion of a regression line.
#' @param addGuestLimits A boolean that activates the insertion of guest limits.
#' @param deltaGuest Numeric value parameter for the Guest function.
#' @param labelGuestCriteria Label used in the legend for guest criteria (default: "guest criteria").
#' @param asSquarePlot A boolean; if true, the plot is returned as a square plot with aspect ratio = 1 and fixed ratios.
#' @param observedDataDirection Either 'x' or 'y', defining the direction of observed data.
#'
#' @return A `ggplot` object representing the plotted data.
#' @export
#' @family plot functions
plotYVsX <- function(data,
                     mapping,
                     metaData = NULL,
                     geomPointAttributes = getDefaultGeomAttributes("Point"),
                     geomErrorbarAttributes = getDefaultGeomAttributes("Errorbar"),
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
                     xscale = AxisScales$linear,
                     xscale.args = list(),
                     yscale = AxisScales$log,
                     yscale.args = list(),
                     observedDataDirection = "y",
                     yDisplayAsAbsolute = TRUE) {
  if (is.double(comparisonLineVector)) comparisonLineVector <- as.list(comparisonLineVector)
  .validatePlotYXsXInputs(
    data = data,
    metaData = metaData,
    geomPointAttributes = geomPointAttributes,
    geomErrorbarAttributes = geomErrorbarAttributes,
    geomGuestLineAttributes = geomGuestLineAttributes,
    geomComparisonLineAttributes = geomComparisonLineAttributes,
    geomLLOQAttributes = geomLLOQAttributes,
    groupAesthetics = groupAesthetics,
    comparisonLineVector = comparisonLineVector,
    addRegression = addRegression,
    addGuestLimits = addGuestLimits,
    deltaGuest = deltaGuest,
    residualScale = residualScale,
    asSquarePlot = asSquarePlot,
    xscale = xscale,
    xscale.args = xscale.args,
    yscale = yscale,
    yscale.args = yscale.args,
    observedDataDirection = observedDataDirection
  )

  mappedData <- MappedData$new(
    data = data,
    mapping = mapping,
    xlimits = xscale.args$limits,
    ylimits = yscale.args$limits,
    direction = observedDataDirection,
    isObserved = TRUE,
    groupAesthetics = groupAesthetics,
    residualScale = residualScale,
    residualAesthetic = "y",
    xscale = xscale,
    yscale = yscale
  )
  mappedData$addMetaData(metaData = metaData)

  #-  initialize plot
  plotObject <- initializePlot(mappedData = mappedData)
  if (mappedData$hasResidualMapping) {
    plotObject <- plotObject +
      labs(y = mappedData$residualLabel)
  }

  #----- Build layers
  # Each new layer is added on top of previous
  # Thus, scatter points are added as last layer to prevent them being hidden by lines or errorbars
  # 1- Horizontal lines

  # add Horizontal  or diagonal lines
  if (is.list(comparisonLineVector)) {
    plotObject <- addComparisonLines(
      plotObject = plotObject,
      comparisonLineVector = comparisonLineVector,
      yDisplayAsAbsolute = yDisplayAsAbsolute,
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
      yDisplayAsAbsolute = yDisplayAsAbsolute,
      geomGuestLineAttributes = geomGuestLineAttributes
    )
  }


  # add Error bars
  if (all(c("xmin", "xmax") %in% names(mappedData$mapping))) {
    plotObject <- plotObject +
      do.call(
        what = ggplot2::geom_errorbar,
        args =
          utils::modifyList(
            x = list(
              na.rm = TRUE,
              orientation = "y"
            ),
            val = geomErrorbarAttributes
          )
      )
  }

  if (all(c("ymin", "ymax") %in% names(mappedData$mapping))) {
    plotObject <- plotObject +
      do.call(
        what = ggplot2::geom_errorbar,
        args = utils::modifyList(
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
    mappedDataAboveLLOQ <- mappedData$clone()
    if (mappedDataAboveLLOQ$hasLLOQMatch) {
      mappedDataAboveLLOQ$data <-
        mappedDataAboveLLOQ$data[mappedDataAboveLLOQ$data$isLLOQ.i == FALSE, ]
    }
    plotObject <- addLayer(
      mappedData = mappedDataAboveLLOQ,
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
  plotObject <- addLLOQLayer(
    plotObject = plotObject,
    mappedData = mappedData,
    layerToCall = geom_vline,
    useLinetypeAsAttribute = "lloq" %in% names(mappedData$mapping),
    geomLLOQAttributes = geomLLOQAttributes
  )


  if (asSquarePlot) {
    plotObject <- plotObject +
      theme(aspect.ratio = 1) +
      coord_fixed(ratio = 1)

    xscale.args$limits <- range(mappedData$xlimits, mappedData$ylimits)
    yscale.args$limits <- range(mappedData$xlimits, mappedData$ylimits)
  }


  # set scales

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
      scale_linetype_manual(
        values = linetypes, breaks = names(linetypes),
        guide = guide_legend(order = 10, title = NULL)
      )
  }

  # do quantification
  if (requireNamespace("data.table", quietly = TRUE)) {
    plotObject$countsWithin <- getCountsWithin(
      data = mappedData$dataForPlot,
      mapping = mappedData$mapping,
      comparisonLineVector = comparisonLineVector,
      addGuestLimits = addGuestLimits,
      deltaGuest = deltaGuest,
      yDisplayAsAbsolute = yDisplayAsAbsolute
    )
  }

  return(plotObject)
}
#' @title Add Comparison Lines to Plot
#' @description
#' This function adds horizontal or diagonal comparison lines to the given ggplot object.
#'
#' @inheritParams plotYVsX
#' @inheritParams plotPredVsObs
#' @param geomLineAttributes Line attributes, e.g., `color`, `linetype`, passed to `ggplot2::geom_hline` or `ggplot2::geom_abline`.
#'
#' @return The updated `ggplot` object with comparison lines added.
#' @keywords internal
addComparisonLines <- function(plotObject,
                               comparisonLineVector,
                               yDisplayAsAbsolute,
                               geomLineAttributes,
                               xyscale) {
  # initialize  to avoid warnings in check()
  value <- name <- NULL

  # get mapping
  if (yDisplayAsAbsolute) {
    if (xyscale == AxisScales$log) {
      lineMapping <- aes(
        intercept = log10(value),
        slope = 1
      )
    } else if (xyscale == AxisScales$linear) {
      lineMapping <- aes(
        intercept = 0,
        slope = value
      )
    }
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
      what = ifelse(yDisplayAsAbsolute,
        ggplot2::geom_abline,
        ggplot2::geom_hline
      ),
      args = utils::modifyList(
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
                          yDisplayAsAbsolute,
                          geomGuestLineAttributes) {
  if ("linetype" %in% names(geomGuestLineAttributes)) {
    geomGuestLineAttributes$linetype <- NULL
  }

  plotObject <- plotObject +
    do.call(
      what = geom_function,
      args = utils::modifyList(
        list(
          mapping = aes(linetype = labelGuestCriteria),
          fun = getGuestLimits,
          args = list(
            deltaGuest = deltaGuest,
            yDisplayAsAbsolute = yDisplayAsAbsolute,
            asLower = TRUE
          ),
          data = data.table(x = NA, y = NA), # dummy data to avoid messages
          inherit.aes = FALSE,
          key_glyph = "path",
          na.rm = TRUE
        ),
        geomGuestLineAttributes
      )
    ) +
    do.call(
      what = geom_function,
      args = utils::modifyList(
        list(
          mapping = aes(linetype = "guest criteria"),
          fun = getGuestLimits,
          args = list(
            deltaGuest = deltaGuest,
            yDisplayAsAbsolute = yDisplayAsAbsolute,
            asLower = FALSE
          ),
          data = data.table(x = NA, y = NA), # dummy data to avoid messages
          inherit.aes = FALSE,
          key_glyph = "path",
          na.rm = TRUE
        ),
        geomGuestLineAttributes
      )
    )

  return(plotObject)
}
#' Calculate Limits for DDI Ratio according to Guest et al.
#'
#' This function calculates the limits according to Guest et al.
#' for the DDI ratio based on the provided parameters.
#'
#' @param x A numeric vector representing the observed values.
#' @param deltaGuest Numeric value parameter for the Guest function.
#' @param asLower A logical value indicating whether to calculate lower limits (default is TRUE).
#' @param yDisplayAsAbsolute A logical value if FALSE the limits are calculated for the ratio predicted/observed
#'  if TRUE limits are calculated for observed
#'
#' @references
#' <https://pubmed.ncbi.nlm.nih.gov/21036951>
#'
#' @return A numeric vector representing the calculated limits for the DDI ratio.
#' @keywords internal
getGuestLimits <- function(x, deltaGuest = 1, yDisplayAsAbsolute = FALSE, asLower = TRUE) {
  xSym <- x
  xSym[x < 1] <- 1 / x[x < 1]
  limit <- (deltaGuest + 2 * (xSym - 1)) / xSym
  if (asLower) limit <- 1 / limit

  if (yDisplayAsAbsolute) limit <- limit * x

  return(limit)
}
#' @title Count Entries Within Specific Limits
#' @description
#' This function counts entries within specific limits defined by the comparison lines and guest limits.
#'
#' @inheritParams plotYVsX
#'
#' @return A data table summarizing the counts within the specified limits.
#' @export
getCountsWithin <- function(data,
                            mapping,
                            comparisonLineVector = getFoldDistanceList(c(1.5, 2)),
                            addGuestLimits = FALSE,
                            deltaGuest = 1,
                            yDisplayAsAbsolute) {
  # initialize variables to avoid warning in check()
  Description <- value <- Number <- name <- x <- y <- NULL # nolint

  # Check for limit lines
  if (!addGuestLimits &&
    is.null(comparisonLineVector)) {
    return(NULL)
  }

  checkmate::assertDataFrame(data, null.ok = FALSE, min.rows = 1)
  checkmate::assertNames(names(mapping), must.include = c("y", "x"))
  checkmate::assertFlag(addGuestLimits, null.ok = FALSE)
  if (is.double(comparisonLineVector)) comparisonLineVector <- as.list(comparisonLineVector)
  checkmate::assertList(comparisonLineVector, types = "double", any.missing = FALSE, null.ok = TRUE, min.len = 1)
  checkmate::assertDouble(deltaGuest, null.ok = !addGuestLimits, len = 1)

  lineVectorFiltered <- comparisonLineVector[lapply(comparisonLineVector, length) == 2]
  if (length(lineVectorFiltered) == 0 && !addGuestLimits) {
    return(NULL)
  }
  if (is.null(names(lineVectorFiltered))) {
    names(lineVectorFiltered) <- sapply(lineVectorFiltered, function(x) {
      paste(x, collapse = " - ")
    })
  }

  # use data.table functionality
  data.table::setDT(data)

  fixedData <- list()
  for (aesthetic in intersect(c("x", "y", "group"), names(mapping))) {
    fixedData[[aesthetic]] <-
      tryCatch(
        {
          rlang::eval_tidy(
            expr = rlang::get_expr(mapping[[aesthetic]]),
            data = data,
            env = rlang::get_env(mapping[[aesthetic]])
          )
        },
        error = function(cond) {
          warning("It was not possible to derive the data with the mapping")
          return(NULL)
        }
      )
  }
  fixedData <- data.table::as.data.table(fixedData)

  # if grouping provide one row per group
  if ("group" %in% names(fixedData)) {
    tmp <- merge(fixedData[, .("Points total" = .N), by = "group"],
      fixedData[, as.list(
        countEntriesInBetween(
          xColumn = x,
          yColumn = y,
          comparisonLineVector = lineVectorFiltered,
          addGuestLimits = addGuestLimits,
          deltaGuest = deltaGuest,
          yDisplayAsAbsolute = yDisplayAsAbsolute
        )
      ),
      by = "group"
      ],
      by = "group"
    )

    tmp <- rbind(
      data.table(
        group = "all Groups",
        tmp[, lapply(.SD, sum), .SDcols = setdiff(names(tmp), "group")]
      ),
      tmp
    )

    countsWithin <- tidyr::pivot_longer(
      data = tmp,
      cols = intersect(names(tmp), c(names(lineVectorFiltered), "guest criteria")),
      names_to = "Description", values_to = "Number"
    ) %>%
      dplyr::mutate(Fraction = Number / get("Points total")) %>%
      tidyr::pivot_longer(cols = c("Number", "Fraction")) %>%
      dplyr::mutate(Description = paste(Description, name)) %>%
      dplyr::mutate(name = NULL) %>%
      tidyr::pivot_wider(names_from = Description, values_from = value)
  } else {
    # if provide one row per 'fold'

    totalNumber <- fixedData[, .(
      Number = sum(!is.na(y)),
      Fraction = 1
    )] %>%
      dplyr::mutate(Description = "Points total") %>%
      data.table::setcolorder("Description")

    tmp <-
      fixedData[, as.list(
        countEntriesInBetween(
          xColumn = x,
          yColumn = y,
          comparisonLineVector = lineVectorFiltered,
          addGuestLimits = addGuestLimits,
          deltaGuest = deltaGuest,
          yDisplayAsAbsolute = yDisplayAsAbsolute
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
#' @title Count Entries Between Specified Limits
#' @description
#' This function counts the number of entries within specified limits for the given X and Y columns.
#' It calculates the counts based on the comparison line vector and guest limits, if applicable.
#'
#' @param yColumn A numeric vector containing the Y values to count.
#' @param xColumn A numeric vector containing the X values to count.
#' @param comparisonLineVector A list of numeric values defining the comparison limits.
#' @param deltaGuest A numeric value parameter for the Guest function.
#' @param addGuestLimits A boolean indicating whether to include guest limits in the counting.
#' @param yDisplayAsAbsolute A boolean indicating whether to consider absolute values for Y.
#'
#' @return A list containing counts of entries that fall within the specified limits, including guest criteria if applicable.
#' @keywords internal
countEntriesInBetween <- function(yColumn, xColumn, comparisonLineVector,
                                  deltaGuest, addGuestLimits, yDisplayAsAbsolute) {
  counts <- list()
  if (addGuestLimits) {
    lower <-
      getGuestLimits(
        xColumn,
        deltaGuest = deltaGuest,
        yDisplayAsAbsolute = yDisplayAsAbsolute,
        asLower = TRUE
      )
    upper <-
      getGuestLimits(
        xColumn,
        deltaGuest = deltaGuest,
        yDisplayAsAbsolute = yDisplayAsAbsolute,
        asLower = FALSE
      )
    counts[["guest criteria"]] <- sum(yColumn >= lower &
      yColumn <= upper)
  }

  if (!is.null(names(comparisonLineVector))) {
    if (yDisplayAsAbsolute) {
      ratio <- yColumn / xColumn
    } else {
      ratio <- yColumn
    }
    for (fd in names(comparisonLineVector)) {
      if (length(comparisonLineVector[[fd]]) > 1) {
        counts[[fd]] <- sum(ratio >= min(comparisonLineVector[[fd]]) &
          ratio <= max(comparisonLineVector[[fd]]))
      }
    }
  }
  return(counts)
}
#' @title Validate Plot Inputs
#' @description
#' This internal function validates the inputs for plotting functions to ensure proper data format and parameter values.
#'
#' @inheritParams plotYVsX
#'
#' @return Invisible NULL if validation is successful; otherwise, an error is raised.
#' @keywords internal
.validatePlotYXsXInputs <- function(
    data,
    metaData,
    geomPointAttributes,
    geomErrorbarAttributes,
    geomGuestLineAttributes,
    geomComparisonLineAttributes,
    geomLLOQAttributes,
    groupAesthetics,
    comparisonLineVector,
    addRegression,
    addGuestLimits,
    deltaGuest,
    residualScale,
    asSquarePlot,
    xscale,
    xscale.args,
    yscale,
    yscale.args,
    observedDataDirection) {
  checkmate::assertDataFrame(data)
  checkmate::assertList(metaData, types = "list", null.ok = TRUE)

  checkmate::assertList(geomPointAttributes, null.ok = FALSE, min.len = 0)
  checkmate::assertList(geomErrorbarAttributes, null.ok = FALSE, min.len = 0)
  checkmate::assertList(geomGuestLineAttributes, null.ok = FALSE, min.len = 0)
  checkmate::assertList(geomComparisonLineAttributes, null.ok = FALSE, min.len = 0)
  checkmate::assertList(geomLLOQAttributes, null.ok = FALSE, min.len = 0)

  checkmate::assertCharacter(groupAesthetics, min.len = 0, all.missing = TRUE, null.ok = TRUE)

  checkmate::assertList(comparisonLineVector, types = "double", any.missing = FALSE, null.ok = TRUE, min.len = 1)

  checkmate::assertFlag(addRegression)

  checkmate::assertFlag(addGuestLimits)
  checkmate::assertDouble(deltaGuest, lower = 1, len = 1, null.ok = !addGuestLimits)

  checkmate::assertChoice(residualScale, choices = c(
    ResidualScales$linear,
    ResidualScales$log, ResidualScales$ratio
  ), null.ok = TRUE)

  checkmate::assertFlag(asSquarePlot)
  checkmate::assertChoice(xscale, choices = c(ResidualScales$linear, ResidualScales$log), null.ok = TRUE)
  checkmate::assertList(xscale.args, null.ok = FALSE, min.len = 0)
  checkmate::assertChoice(yscale, choices = c(ResidualScales$linear, ResidualScales$log), null.ok = TRUE)
  checkmate::assertList(yscale.args, null.ok = FALSE, min.len = 0)

  checkmate::assertChoice(observedDataDirection, choices = c("x", "y"), null.ok = TRUE)

  return(invisible())
}
