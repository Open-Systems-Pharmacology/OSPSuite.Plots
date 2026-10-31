# Test legend-wrapping.R functionality

longLabel <- "Median and [5th-95th] percentiles for Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"

makeTimeProfilePlot <- function() {
  df <- data.frame(
    time = rep(seq(1, 24), 2),
    concentration = c(dexp(1:24, 0.2), dexp(1:24, 0.1)),
    output = rep(c(longLabel, paste(longLabel, "observed")), each = 24)
  )
  return(plotTimeProfile(
    data = df,
    mapping = aes(x = time, y = concentration, color = output)
  ))
}

# helper: collect legend text grob labels of a built plot
legendTextLabels <- function(plotObject, pattern = "percentiles") {
  gtable <- ggplot2::ggplotGrob(plotObject)
  labels <- character(0)
  collect <- function(grob) {
    if (grid::is.grob(grob)) {
      if (inherits(grob, "text") && length(grob$label) == 1) {
        labels <<- c(labels, grob$label)
      }
      if (!is.null(grob$children)) invisible(lapply(grob$children, collect))
      if (!is.null(grob$grobs)) invisible(lapply(grob$grobs, collect))
    } else if (is.list(grob)) {
      invisible(lapply(grob, collect))
    }
  }
  collect(gtable)
  return(labels[grepl(pattern, labels)])
}

# .wrapLegendText --------------------------

test_that(".wrapLegendText wraps text at word boundaries", {
  wrapped <- ospsuite.plots:::.wrapLegendText("some longer legend text entry", maxWidth = 10)
  lines <- strsplit(wrapped, "\n")[[1]]
  expect_true(all(nchar(lines) <= 10))
  expect_equal(paste(gsub("\n", " ", wrapped)), "some longer legend text entry")
})

test_that(".wrapLegendText hard-splits tokens without spaces", {
  wrapped <- ospsuite.plots:::.wrapLegendText(longLabel, maxWidth = 20)
  lines <- strsplit(wrapped, "\n")[[1]]
  expect_true(all(nchar(lines) <= 20))
  # only whitespace at the wrap positions is dropped, no other character changes
  expect_equal(gsub("[[:space:]]", "", wrapped), gsub("[[:space:]]", "", longLabel))
  # the long path token is split
  expect_true(any(grepl("Organism\\|PeripheralV", lines)))
})

test_that(".wrapLegendText keeps short text and NA unchanged", {
  expect_equal(ospsuite.plots:::.wrapLegendText("short", maxWidth = 20), "short")
  expect_equal(ospsuite.plots:::.wrapLegendText(NA_character_, maxWidth = 20), NA_character_)
  expect_equal(ospsuite.plots:::.wrapLegendText(character(0), maxWidth = 20), character(0))
})

# legendWidthFromPlotDimensions --------------------------

test_that("legendWidthFromPlotDimensions calculates width for side legend", {
  p <- makeTimeProfilePlot()
  # 8 cm = 3.1496 in; 40% of it at 8.8 pt font with 0.55 ratio = 18.74 characters
  expect_equal(
    legendWidthFromPlotDimensions(p, plotWidth = 8, unit = "cm"),
    18L
  )
})

test_that("legendWidthFromPlotDimensions gives full width for top/bottom legends", {
  p <- makeTimeProfilePlot() + theme(legend.position = "bottom")
  expect_equal(
    legendWidthFromPlotDimensions(p, plotWidth = 8, unit = "cm"),
    46L
  )
  pTop <- makeTimeProfilePlot() + theme(legend.position = "top")
  expect_equal(
    legendWidthFromPlotDimensions(pTop, plotWidth = 8, unit = "cm"),
    46L
  )
})

test_that("legendWidthFromPlotDimensions handles units and dpi", {
  p <- makeTimeProfilePlot()
  expect_equal(legendWidthFromPlotDimensions(p, plotWidth = 3.15, unit = "in"), 18L)
  expect_equal(legendWidthFromPlotDimensions(p, plotWidth = 80, unit = "mm"), 18L)
  # 1008 px at 320 dpi = 3.15 in
  expect_equal(legendWidthFromPlotDimensions(p, plotWidth = 1008, unit = "px", dpi = 320), 18L)
  expect_error(
    legendWidthFromPlotDimensions(p, plotWidth = 8, unit = "m"),
    regexp = "Must be element of"
  )
})

test_that("legendWidthFromPlotDimensions uses font size and fraction arguments", {
  p <- makeTimeProfilePlot()
  # 1.2598 in / (0.55 * 12 / 72 in) = 13.74
  expect_equal(
    legendWidthFromPlotDimensions(p, plotWidth = 8, unit = "cm", fontSize = 12),
    13L
  )
  # theme-based font size: legend.text size 14 -> 1.2598 / (0.55*14/72) = 11.78
  expect_equal(
    legendWidthFromPlotDimensions(
      p + theme(legend.text = element_text(size = 14)),
      plotWidth = 8, unit = "cm"
    ),
    11L
  )
  # 3.1496 in * 0.3 / (0.55*8.8/72) = 14.05
  expect_equal(
    legendWidthFromPlotDimensions(p, plotWidth = 8, unit = "cm", legendFraction = 0.3),
    14L
  )
  expect_error(
    legendWidthFromPlotDimensions(p, plotWidth = 8, legendFraction = 2),
    regexp = "legendFraction"
  )
})

test_that("legendWidthFromPlotDimensions returns at least 1 and validates input", {
  p <- makeTimeProfilePlot()
  expect_equal(legendWidthFromPlotDimensions(p, plotWidth = 0.001, unit = "cm"), 1L)
  expect_error(
    legendWidthFromPlotDimensions(list()),
    regexp = "gg"
  )
})

# wrapLegendLabels --------------------------

test_that("wrapLegendLabels wraps legend labels of a time profile plot", {
  p <- makeTimeProfilePlot()
  pWrapped <- wrapLegendLabels(p, maxWidth = 20)

  expect_s3_class(pWrapped, "ggplot")
  labels <- legendTextLabels(pWrapped)
  expect_length(labels, 2)
  expect_true(all(grepl("\n", labels)))
  lineWidths <- nchar(strsplit(labels[1], "\n")[[1]])
  expect_true(all(lineWidths <= 20))
})

test_that("wrapLegendLabels does not modify the input plot", {
  p <- makeTimeProfilePlot()
  colourScaleBefore <- p$scales$get_scales("colour")
  pWrapped <- wrapLegendLabels(p, maxWidth = 20)

  # original plot has no explicit colour scale and renders unwrapped
  expect_null(p$scales$get_scales("colour"))
  expect_length(legendTextLabels(p)[grepl("\n", legendTextLabels(p))], 0)
  expect_identical(colourScaleBefore, p$scales$get_scales("colour"))
  rm(pWrapped)
})

test_that("wrapLegendLabels preserves plot colors and shapes", {
  p <- makeTimeProfilePlot() +
    aes(shape = output)
  pWrapped <- wrapLegendLabels(p, maxWidth = 20)

  builtP <- ggplot_build(p)
  builtWrapped <- ggplot_build(pWrapped)
  colorsP <- unlist(lapply(builtP$data, function(d) if ("colour" %in% names(d)) unique(d$colour) else NULL))
  colorsWrapped <- unlist(lapply(builtWrapped$data, function(d) if ("colour" %in% names(d)) unique(d$colour) else NULL))
  expect_setequal(colorsP, colorsWrapped)

  shapesP <- unlist(lapply(builtP$data, function(d) if ("shape" %in% names(d)) unique(d$shape) else NULL))
  shapesWrapped <- unlist(lapply(builtWrapped$data, function(d) if ("shape" %in% names(d)) unique(d$shape) else NULL))
  expect_setequal(shapesP, shapesWrapped)
})

test_that("wrapLegendLabels wraps labels of scales already present on the plot", {
  breaks <- c(longLabel, paste(longLabel, "observed"))
  p <- makeTimeProfilePlot() +
    scale_colour_osp(
      breaks = breaks,
      labels = stats::setNames(
        c("Renamed simulated output", "Renamed observed output"),
        breaks
      )
    )
  pWrapped <- wrapLegendLabels(p, maxWidth = 12)

  labels <- legendTextLabels(pWrapped, pattern = "Renamed")
  # named labels are resolved before wrapping, so the renamed text is wrapped
  expect_length(labels, 2)
  expect_true(all(grepl("\n", labels)))
  expect_true(all(nchar(strsplit(labels[1], "\n")[[1]]) <= 12))
})

test_that("wrapLegendLabels keeps manual linetype values and wraps their labels", {
  p <- makeTimeProfilePlot() +
    geom_hline(
      data = data.frame(yintercept = 0.1),
      aes(yintercept = yintercept, linetype = "LLOQ of a very long legend entry")
    ) +
    scale_linetype_manual(values = c("LLOQ of a very long legend entry" = "dashed"))
  pWrapped <- wrapLegendLabels(p, maxWidth = 10)

  linetypeScale <- pWrapped$scales$get_scales("linetype")
  expect_s3_class(linetypeScale, "ScaleDiscrete")
  expect_true(is.function(linetypeScale$labels))
  # manual palette preserved
  expect_identical(unname(linetypeScale$palette(1)), "dashed")
  expect_identical(names(linetypeScale$palette(1)), "LLOQ of a very long legend entry")
  # label wrapped
  expect_match(linetypeScale$labels("LLOQ of a very long legend entry"), "\n")
  # scale of the input plot is untouched
  expect_false(is.function(p$scales$get_scales("linetype")$labels))
})

test_that("wrapLegendLabels composes with existing label functions", {
  p <- makeTimeProfilePlot() +
    scale_colour_osp(labels = function(x) paste("Entry:", x))
  pWrapped <- wrapLegendLabels(p, maxWidth = 15)

  labels <- legendTextLabels(pWrapped)
  expect_true(all(grepl("Entry:", labels)))
  expect_true(all(nchar(strsplit(labels[1], "\n")[[1]]) <= 15))
})

test_that("wrapLegendLabels does not add scales for continuous colour mappings", {
  df <- data.frame(x = 1:10, y = 1:10, value = seq(0.1, 1, 0.1))
  p <- ggplot(df, aes(x, y, color = value)) +
    geom_point() +
    theme_osp()
  pWrapped <- wrapLegendLabels(p, maxWidth = 20)

  expect_null(pWrapped$scales$get_scales("colour"))
  expect_s3_class(ggplot_build(pWrapped)$plot$scales$get_scales("colour"), "ScaleContinuous")
})

test_that("wrapLegendLabels works on plain ggplot objects", {
  df <- data.frame(x = 1:5, y = 1:5, group = rep(longLabel, 5))
  p <- ggplot(df, aes(x, y, color = group)) +
    geom_point()
  pWrapped <- wrapLegendLabels(p, maxWidth = 20)

  labels <- legendTextLabels(pWrapped)
  expect_true(all(grepl("\n", labels)))
  # plain ggplot keeps the default hue palette
  expect_s3_class(pWrapped$scales$get_scales("colour"), "ScaleDiscrete")
})

test_that("wrapLegendLabels supports CombinedPlot objects", {
  p <- makeTimeProfilePlot()
  combinedPlot <- CombinedPlot$new(plotObject = p)
  combinedWrapped <- wrapLegendLabels(combinedPlot, maxWidth = 20)

  expect_s3_class(combinedWrapped, "CombinedPlot")
  labels <- legendTextLabels(combinedWrapped$plotObject)
  expect_true(all(grepl("\n", labels)))
  # input CombinedPlot is not modified
  expect_length(
    legendTextLabels(combinedPlot$plotObject)[grepl("\n", legendTextLabels(combinedPlot$plotObject))],
    0
  )
})

test_that("wrapLegendLabels validates its inputs", {
  p <- makeTimeProfilePlot()
  expect_error(wrapLegendLabels(list(), maxWidth = 20), regexp = "gg")
  expect_error(wrapLegendLabels(p, maxWidth = 0), regexp = "maxWidth")
  expect_error(wrapLegendLabels(p, maxWidth = "a"), regexp = "maxWidth")
})

# wrapLegend --------------------------

test_that("wrapLegend combines width calculation and wrapping", {
  p <- makeTimeProfilePlot() + theme(legend.position = "bottom")
  pWrapped <- wrapLegend(p, plotWidth = 8, unit = "cm")

  expectedWidth <- legendWidthFromPlotDimensions(p, plotWidth = 8, unit = "cm")
  labels <- legendTextLabels(pWrapped)
  expect_true(all(nchar(strsplit(labels[1], "\n")[[1]]) <= expectedWidth))
})

test_that("wrapLegend passes font size and fraction arguments through", {
  p <- makeTimeProfilePlot()
  pWrapped <- wrapLegend(p, plotWidth = 8, unit = "cm", fontSize = 12)

  labels <- legendTextLabels(pWrapped)
  expect_true(all(nchar(strsplit(labels[1], "\n")[[1]]) <= 13))
})
