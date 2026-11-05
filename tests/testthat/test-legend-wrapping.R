# Test legend-wrapping.R functionality

longLabel <- "Median and [5th-95th] percentiles for Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
# prevent lint warnings
output <- NULL
concentration <- NULL
time <- NULL

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
      if (!is.null(grob$children)) {
        invisible(lapply(grob$children, collect))
      }
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
  wrapped <- ospsuite.plots:::.wrapLegendText(
    "some longer legend text entry",
    maxWidth = 10
  )
  lines <- strsplit(wrapped, "\n")[[1]]
  expect_true(all(nchar(lines) <= 10))
  expect_equal(paste(gsub("\n", " ", wrapped)), "some longer legend text entry")
})

test_that(".wrapLegendText use string::str_wrap appropriately", {
  wrapped <- ospsuite.plots:::.wrapLegendText(longLabel, maxWidth = 20)
  expect_equal(
    wrapped,
    stringr::str_wrap(longLabel, width = 20, whitespace_only = FALSE)
  )
})

test_that(".wrapLegendText keeps short text and NA unchanged", {
  expect_equal(
    ospsuite.plots:::.wrapLegendText("short", maxWidth = 20),
    "short"
  )
  expect_equal(
    ospsuite.plots:::.wrapLegendText(NA_character_, maxWidth = 20),
    NA_character_
  )
  expect_equal(
    ospsuite.plots:::.wrapLegendText(character(0), maxWidth = 20),
    character(0)
  )
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
  expect_equal(
    legendWidthFromPlotDimensions(p, plotWidth = 3.15, unit = "in"),
    18L
  )
  expect_equal(
    legendWidthFromPlotDimensions(p, plotWidth = 80, unit = "mm"),
    18L
  )
  # 1008 px at 320 dpi = 3.15 in
  expect_equal(
    legendWidthFromPlotDimensions(p, plotWidth = 1008, unit = "px", dpi = 320),
    18L
  )
  expect_error(
    legendWidthFromPlotDimensions(p, plotWidth = 8, unit = "m"),
    regexp = "Must be element of"
  )
})

test_that("legendWidthFromPlotDimensions uses legendFraction arguments", {
  p <- makeTimeProfilePlot()
  # 1.2598 in / (0.55 * 12 / 72 in) = 13.74
  expect_equal(
    legendWidthFromPlotDimensions(
      p + theme(legend.text = element_text(size = 12)),
      plotWidth = 8,
      unit = "cm"
    ),
    13L
  )
  # theme-based font size: legend.text size 14 -> 1.2598 / (0.55*14/72) = 11.78
  expect_equal(
    legendWidthFromPlotDimensions(
      p + theme(legend.text = element_text(size = 14)),
      plotWidth = 8,
      unit = "cm"
    ),
    11L
  )
  # 3.1496 in * 0.3 / (0.55*8.8/72) = 14.05
  expect_equal(
    legendWidthFromPlotDimensions(
      p,
      plotWidth = 8,
      unit = "cm",
      legendFractionHorizontal = 0.3
    ),
    14L
  )

  expect_equal(
    legendWidthFromPlotDimensions(
      p + theme(legend.position = "right"),
      plotWidth = 8,
      unit = "cm",
      legendFractionHorizontal = 0.5
    ),
    legendWidthFromPlotDimensions(
      p + theme(legend.position = "bottom"),
      plotWidth = 8,
      unit = "cm",
      legendFractionVertical = 0.5
    )
  )
})

test_that("legendWidthFromPlotDimensions returns at least 1 and validates input", {
  p <- makeTimeProfilePlot()
  expect_equal(
    legendWidthFromPlotDimensions(p, plotWidth = 0.001, unit = "cm"),
    1L
  )
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
  colorsP <- unlist(lapply(builtP$data, function(d) {
    if ("colour" %in% names(d)) unique(d$colour) else NULL
  }))
  colorsWrapped <- unlist(lapply(builtWrapped$data, function(d) {
    if ("colour" %in% names(d)) unique(d$colour) else NULL
  }))
  expect_setequal(colorsP, colorsWrapped)

  shapesP <- unlist(lapply(builtP$data, function(d) {
    if ("shape" %in% names(d)) unique(d$shape) else NULL
  }))
  shapesWrapped <- unlist(lapply(builtWrapped$data, function(d) {
    if ("shape" %in% names(d)) unique(d$shape) else NULL
  }))
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
      aes(
        yintercept = yintercept,
        linetype = "LLOQ of a very long legend entry"
      )
    ) +
    scale_linetype_manual(
      values = c("LLOQ of a very long legend entry" = "dashed")
    )
  pWrapped <- wrapLegendLabels(p, maxWidth = 10)

  linetypeScale <- pWrapped$scales$get_scales("linetype")
  expect_s3_class(linetypeScale, "ScaleDiscrete")
  expect_true(is.function(linetypeScale$labels))
  # manual palette preserved
  expect_identical(unname(linetypeScale$palette(1)), "dashed")
  expect_identical(
    names(linetypeScale$palette(1)),
    "LLOQ of a very long legend entry"
  )
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
})

test_that("wrapLegendLabels does not add scales for continuous colour mappings", {
  df <- data.frame(x = 1:10, y = 1:10, value = seq(0.1, 1, 0.1))
  p <- ggplot(df, aes(x, y, color = value)) +
    geom_point() +
    theme_osp()
  pWrapped <- wrapLegendLabels(p, maxWidth = 20)

  expect_null(pWrapped$scales$get_scales("colour"))
  expect_s3_class(
    ggplot_build(pWrapped)$plot$scales$get_scales("colour"),
    "ScaleContinuous"
  )
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
    legendTextLabels(combinedPlot$plotObject)[grepl(
      "\n",
      legendTextLabels(combinedPlot$plotObject)
    )],
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

# Snapshot tests --------------------------

# Simulation output paths without spaces - wrapped after `|` and `-`
spaceFreePathLabels <- c(
  "Organism|PeripheralVenousBlood|Aciclovir|Plasma-Peripheral-Venous-Blood",
  "Organism|Liver|Aciclovir|Intracellular-Unclear-Binding"
)

makeSpaceFreePathsPlot <- function() {
  df <- data.frame(
    time = rep(seq(1, 24), 2),
    concentration = c(dexp(1:24, 0.2), dexp(1:24, 0.1)),
    output = rep(spaceFreePathLabels, each = 24)
  )
  return(plotTimeProfile(
    data = df,
    mapping = aes(x = time, y = concentration, color = output)
  ))
}

test_that("wrapLegendLabels wraps long legend entries of an OSP plot", {
  skip_if_not_installed("vdiffr")

  fig <- wrapLegendLabels(makeTimeProfilePlot(), maxWidth = 20)
  vdiffr::expect_doppelganger("wrapped-legend-osp-plot", fig)
})

test_that("wrapLegendLabels breaks space-free output paths", {
  skip_if_not_installed("vdiffr")

  fig <- wrapLegendLabels(makeSpaceFreePathsPlot(), maxWidth = 12)
  vdiffr::expect_doppelganger("wrapped-legend-space-free-paths", fig)
})

test_that("wrapLegendLabels wraps colour, shape and linetype legends", {
  skip_if_not_installed("vdiffr")

  p <- makeTimeProfilePlot() +
    aes(shape = output) +
    geom_hline(
      data = data.frame(yintercept = 0.1),
      aes(
        yintercept = yintercept,
        linetype = "LLOQ of a very long legend entry"
      )
    ) +
    scale_linetype_manual(
      values = c("LLOQ of a very long legend entry" = "dashed")
    )
  fig <- wrapLegendLabels(p, maxWidth = 12)
  vdiffr::expect_doppelganger("wrapped-legend-multiple-aesthetics", fig)
})

test_that("wrapLegendLabels wraps labels of scales already on the plot", {
  skip_if_not_installed("vdiffr")

  breaks <- c(longLabel, paste(longLabel, "observed"))
  p <- makeTimeProfilePlot() +
    scale_colour_osp(
      breaks = breaks,
      labels = stats::setNames(
        c(
          "Renamed simulated output with a long label",
          "Renamed observed output with a long label"
        ),
        breaks
      )
    )
  fig <- wrapLegendLabels(p, maxWidth = 12)
  vdiffr::expect_doppelganger("wrapped-legend-renamed-scale-labels", fig)
})

test_that("wrapLegendLabels works on plain ggplot objects", {
  skip_if_not_installed("vdiffr")

  df <- data.frame(x = 1:5, y = (1:5)^2, group = rep(longLabel, 5))
  p <- ggplot(df, aes(x, y, color = group)) +
    geom_point()
  fig <- wrapLegendLabels(p, maxWidth = 20)
  vdiffr::expect_doppelganger("wrapped-legend-plain-ggplot", fig)
})

test_that("wrapLegendLabels works on CombinedPlot objects", {
  skip_if_not_installed("vdiffr")

  combinedPlot <- CombinedPlot$new(plotObject = makeTimeProfilePlot())
  fig <- wrapLegendLabels(combinedPlot, maxWidth = 20)$combined()
  vdiffr::expect_doppelganger("wrapped-legend-combined-plot", fig)
})

test_that("wrapLegend wraps a side legend to the plot width", {
  skip_if_not_installed("vdiffr")

  # legend may use 40% of a 6 cm wide plot -> 13 characters per line
  fig <- wrapLegend(makeTimeProfilePlot(), plotWidth = 6, unit = "cm")
  vdiffr::expect_doppelganger("wrap-legend-side-6cm-plot", fig)
})

test_that("wrapLegend wraps a bottom legend to the full plot width", {
  skip_if_not_installed("vdiffr")

  p <- makeTimeProfilePlot() + theme(legend.position = "bottom")
  fig <- wrapLegend(p, plotWidth = 8, unit = "cm")
  vdiffr::expect_doppelganger("wrap-legend-bottom-8cm-plot", fig)
})
