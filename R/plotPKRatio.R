#' @title plotPKRatio
#' @description
#' Producing PK Ratio plots
#'
#' @inheritParams addScatter
#' @param foldDistance Numeric values of fold distance lines to display in log plots.
#' This argument is internally translated into `lines` field of `dataMapping`.
#' __Caution__: this argument is meant for log scaled plots and since fold distance is a ratio it is expected positive.
#' In particular, line of identity corresponds to a `foldDistance` of `1`.
#' @param dataMapping
#' A `PKRatioDataMapping` object mapping `x`, `y` and aesthetic groups to their variable names of `data`.
#' @param plotConfiguration
#' An optional `PKRatioPlotConfiguration` object defining labels, grid, background and watermark.
#' @return A `ggplot` object
#'
#' @references For examples, see:
#' <https://www.open-systems-pharmacology.org/TLF-Library/articles/pk-ratio-vignette.html>
#'
#' @export
#' @family molecule plots
#' @examples
#' # Produce PK Ratio plot
#' pkData <- data.frame(x = c(1, 2, 1, 2, 3), y = c(5, 0.2, 2, 3, 4))
#'
#' plotPKRatio(data = pkData, dataMapping = PKRatioDataMapping$new(x = "x", y = "y"))
#'
#' # Produce PK Ratio plot with user-defined horizontal lines
#' plotPKRatio(
#'   data = pkData,
#'   dataMapping = PKRatioDataMapping$new(x = "x", y = "y"),
#'   foldDistance = c(1, 10)
#' )
#'
plotPKRatio <- function(data,
                        mapping = NULL,
                        metaData = NULL,
                        foldDistance = list(
                          identity = 1,
                          '1.5 fold' = c(1.5, 1 / 1.5),
                          '2 fold' = c(2, 1 / 2)
                        ),
                        geomPointAttributes =  getDefaultGeomAttributes("Point"),
                        geomErrorbarAttributes = getDefaultGeomAttributes("Errorbar"),
                        geomRatioLineAttributes = getDefaultGeomAttributes("Line"),
                        xscale = "linear",
                        xscale.args = list(),
                        yscale = 'log',
                        yscale.args = list(),
                        groupAesthetics = c("colour", "fill",  "shape")
) {
  #----- Validation and formatting of input arguments -----

  checkmate::assertDataFrame(data,null.ok = FALSE,min.rows = 1)
  checkmate::assertList(metaData, types = "list", null.ok = TRUE)
  checkmate::assertList(foldDistance,types = 'double',any.missing = FALSE,names = 'named',null.ok = TRUE,min.len = 1)

  checkmate::assertChoice(xscale, choices = c("linear", "log"), null.ok = TRUE)
  checkmate::assertList(xscale.args, null.ok = FALSE, min.len = 0)
  checkmate::assertChoice(yscale, choices = c("linear", "log"), null.ok = TRUE)
  checkmate::assertList(yscale.args, null.ok = FALSE, min.len = 0)

  checkmate::assertList(geomPointAttributes, null.ok = FALSE, min.len = 0)
  checkmate::assertList(geomErrorbarAttributes, null.ok = FALSE, min.len = 0)
  checkmate::assertList(geomRatioLineAttributes, null.ok = FALSE, min.len = 0)

  checkmate::assertCharacter(groupAesthetics, min.len = 0, all.missing = TRUE, null.ok = TRUE)


  # data match --------------
  mappedData <- MappedData$new(
    data = data,
    mapping = mapping,
    groupAesthetics = groupAesthetics
  )

  #-  initialize plot
  plotObject <- initializePlot(
    metaData = metaData,
    mapping = mappedData$mapping,
    data = mappedData$dataForPlot
  )

  #----- Build layers -----
  # Each new layer is added on top of previous
  # Thus, scatter points are added as last layer to prevent them being hidden by lines or errorbars
  # 1- Horizontal lines

  # add Horizontal lines
  if (!is.null(foldDistance)){

    dt_foldDistance = data.frame()
    for (n in names(foldDistance)){
      dt_foldDistance = rbind(dt_foldDistance,
                              data.frame(value = foldDistance[[n]],
                                         name = n))
    }
    dt_foldDistance$name = factor(dt_foldDistance$name,levels = names(foldDistance),ordered = TRUE)

    plotObject <- plotObject +
      do.call(
        what = ggplot2::geom_hline,
        args = c(
          list(
            data = dt_foldDistance,
            mapping = aes(yintercept = value,
                          linetype = name),
            na.rm = TRUE
          ),
          geomRatioLineAttributes
        )
      )
  }

  # add Error bars
  if (all(c('ymin','ymax') %in% names(mappedData$mapping))){
    plotObject <- plotObject +
      do.call(
        what = ggplot2::geom_errorbar,
        args = c(
          list(
            na.rm = TRUE
          ),
          geomErrorbarAttributes
        )
      )

  }

  # Scatter points
  plotObject <- plotObject +
    do.call(
      what = ggplot2::geom_point,
      args = c(
        list(
          na.rm = TRUE
        ),
        geomPointAttributes
      )
    )


  # set scales ----
  plotObject = addXYScale(
    plotObject = plotObject,
    xscale = xscale,
    xscale.args = xscale.args,
    yscale = yscale,
    yscale.args = yscale.args
  )


  plotObject = plotObject +
    guides(linetype=guide_legend(title = NULL,order = 1))


  # do quantification ----------
  if (require('data.table',quietly = TRUE)) {
    pb = ggplot_build(plotObject)
    iData = which(unlist(lapply(pb$data,function(x){return(all(c('x','y','shape') %in% names(x)))})))

    if (length(iData) >0) {
      if (yscale == 'log'){
        pb$data[[iData]]$y = 10^(pb$data[[iData]]$y)
      }

      plotObject$PKRatioMeasure = getPKRatioMeasure(data = pb$data[[iData]],
                                                    y = 'y',
                                                    foldDistance = foldDistance)
    } else {
      warning('no datapoints available for quantification')
    }

  }

  return(plotObject)
}



#' Counts entries within specific limits
#'
#' @inheritParams plotPKRatio
#' @param yColumn column name for values to count
#' @param groups  column names to group
#'
#' @return data table with summary
#' @export
getPKRatioMeasure = function(data,
                             yColumn,
                             foldDistance = list(
                               identity = 1,
                               '1.5 fold' = c(1.5, 1 / 1.5),
                               '2 fold' = c(2, 1 / 2)
                             ),
                             groups = NULL){

  require('data.table',quietly = TRUE)

  checkmate::assertDataFrame(data,null.ok = FALSE,min.rows = 1)
  checkmate::assertCharacter(yColumn,null.ok = FALSE,len = 1)
  checkmate::assertNames(names(data),disjunct.from = 'yColumn')
  checkmate::assertList(foldDistance,types = 'double',any.missing = FALSE,names = 'named',null.ok = TRUE,min.len = 1)

  # use data.table functionality
  data.table::setDT(data)

  # define auxiliary function
  countEntriesInBetweenFoldDistance = function(yColumn,foldDistance){

    counts = list()
    for (fd in names(foldDistance)){
      if (length(foldDistance[[fd]]) > 1){
        counts[[fd]] = sum(yColumn >= min(foldDistance[[fd]]) &
                             yColumn <= max(foldDistance[[fd]]))
      }
    }

    return(counts)
  }

  # if grouping provide one row per group
  if (!is.null(groups)){
    tmp <- merge(data[,.('Points total' = .N), by = groups],
                 data[,as.list(countEntriesInBetweenFoldDistance(get(yColumn),foldDistance)),
                             by = groups],
                 by = groups)

    PKRatioMeasure <- tidyr::pivot_longer(data = tmp,
                               cols = intersect(names(tmp),names(foldDistance)),
                               names_to = 'description',values_to = 'Number') %>%
      dplyr::mutate(Ratio = Number/get('Points total')) %>%
      pivot_longer(cols = c('Number','Ratio')) %>%
      dplyr::mutate(description = paste(description , name)) %>%
      dplyr::mutate(name = NULL) %>%
      pivot_wider(names_from = description,values_from = value)

  } else{
    # if provide one row per 'fold'

    TotalNumber = data[,.('Number' = sum(!is.na(get(yColumn))))] %>%
      dplyr::mutate(description = "Points total") %>%
      data.table::setcolorder('description')

    tmp = data[,as.list(countEntriesInBetweenFoldDistance(get(yColumn),foldDistance))]
    tmp <- tidyr::pivot_longer(data = tmp, cols = names(tmp),names_to = 'description',values_to = 'Number') %>%
      dplyr::mutate(Ratio = Number/TotalNumber$Number) %>%
      dplyr::mutate(description = paste('Points within',description))


    PKRatioMeasure <- rbind(TotalNumber,
                            tmp,
                            fill = TRUE)

  }

  return(PKRatioMeasure)
}
