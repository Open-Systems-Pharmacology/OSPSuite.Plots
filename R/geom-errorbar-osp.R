#' @title OSP Errorbar Layer
#' @description
#' A geom that renders error bars with cap `width` specified in **mm** units,
#' keeping it visually consistent with the `linewidth` aesthetic, which is
#' also expressed in **mm**. Unlike [ggplot2::geom_errorbar()],
#' the cap width is independent of the data coordinate range or axis scale.
#'
#' Vertical orientation (`aes(x, ymin, ymax)`) is used by default.
#' Pass `orientation = "x"` for horizontal error bars (`aes(y, xmin, xmax)`).
#'
#' @inheritParams ggplot2::geom_errorbar
#' @param width Width of the error bar caps in mm units. Default: `1.5`.
#' @param orientation Orientation of the layer. `"x"` produces horizontal
#'   error bars (range along the x-axis). Any other value, including `NA`
#'   (default) and `"y"`, produces vertical error bars (range along the
#'   y-axis).
#' @param na.rm Missing values in the range aesthetics are dropped (the
#'   affected cap is simply not drawn) for both `TRUE` and `FALSE`. No warning
#'   is emitted in either case. The argument is kept for consistency with the
#'   [ggplot2::geom_errorbar()] interface.
#' @return A ggplot2 layer that can be added to a plot.
#' @export
#' @family setDefault functions
#' @examples
#' library(ggplot2)
#' df <- data.frame(
#'   x    = 1:3,
#'   y    = c(1, 2, 3),
#'   ymin = c(0.5, 1.5, 2.5),
#'   ymax = c(1.5, 2.5, 3.5)
#' )
#' ggplot(df, aes(x, y, ymin = ymin, ymax = ymax)) +
#'   geom_errorbar_osp(width = 2, linewidth = 0.8)
# nolint start: object_name_linter
geom_errorbar_osp <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  orientation = NA,
  width = 1.5,
  lineend = "butt",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  # nolint end
  ggplot2::layer(
    geom = GeomErrorbarOsp,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      orientation = orientation,
      width = width,
      lineend = lineend,
      na.rm = na.rm,
      ...
    )
  )
}

# ggproto object ----

#' @title GeomErrorbarOsp
#' @description
#' ggproto object for OSP error bars with cap width in mm units.
#' Use [geom_errorbar_osp()] to add this geom to a ggplot.
#' @format NULL
#' @usage NULL
#' @export
# nolint start: object_name_linter
GeomErrorbarOsp <- ggplot2::ggproto(
  # nolint end
  "GeomErrorbarOsp",
  ggplot2::Geom,
  required_aes = c("x|y", "ymin|xmin", "ymax|xmax"),
  default_aes = ggplot2::aes(
    colour = "black",
    linewidth = 0.5,
    linetype = 1,
    alpha = NA
  ),
  draw_key = ggplot2::draw_key_path,
  extra_params = c("na.rm", "orientation", "width", "lineend"),
  setup_data = function(data, params) {
    # Suppress rows where the bar has zero length to avoid orphan caps.
    # Both range columns are set to NA_real_ so the cap is drawn at an NA
    # coordinate, i.e. not rendered.
    # Respect orientation when provided so only the relevant axis is checked.
    orientation <- params$orientation %||% NA
    horizontal <- identical(orientation, "x")

    if (!horizontal && all(c("ymin", "ymax") %in% names(data))) {
      zeroRange <- !is.na(data$ymin) &
        !is.na(data$ymax) &
        data$ymin == data$ymax
      data$ymin[zeroRange] <- NA_real_
      data$ymax[zeroRange] <- NA_real_
    }
    if (horizontal && all(c("xmin", "xmax") %in% names(data))) {
      zeroRange <- !is.na(data$xmin) &
        !is.na(data$xmax) &
        data$xmin == data$xmax
      data$xmin[zeroRange] <- NA_real_
      data$xmax[zeroRange] <- NA_real_
    }
    return(data)
  },
  # nolint start: object_name_linter
  draw_panel = function(
    self,
    data,
    panel_params,
    coord,
    lineend = "butt",
    orientation = NA,
    width = 1.5
  ) {
    # nolint end
    if (nrow(data) == 0) return(grid::nullGrob())
    coords <- coord$transform(data, panel_params)
    if (nrow(coords) == 0) return(grid::nullGrob())

    alpha <- ifelse(is.na(coords$alpha), 1, coords$alpha)
    col <- scales::alpha(coords$colour, alpha)
    # linewidth aesthetic is in mm (ggplot2 >= 3.4); grid lwd is in pt
    lwd <- coords$linewidth * ggplot2::.pt
    lty <- coords$linetype

    halfWidthMm <- grid::unit(width / 2, "mm")

    # orientation = "x" → horizontal bars; everything else ("y", NA) → vertical.
    horizontal <- identical(orientation, "x")

    if (horizontal) {
      if (
        length(coords$y) == 0 ||
          length(coords$xmin) == 0 ||
          length(coords$xmax) == 0
      ) {
        return(grid::nullGrob())
      }
      center <- grid::unit(coords$y, "npc")
      rangeMin <- grid::unit(coords$xmin, "npc")
      rangeMax <- grid::unit(coords$xmax, "npc")

      mainSeg <- grid::segmentsGrob(
        x0 = rangeMin,
        x1 = rangeMax,
        y0 = center,
        y1 = center,
        gp = grid::gpar(col = col, lwd = lwd, lty = lty, lineend = lineend)
      )
      minCap <- grid::segmentsGrob(
        x0 = rangeMin,
        x1 = rangeMin,
        y0 = center - halfWidthMm,
        y1 = center + halfWidthMm,
        gp = grid::gpar(col = col, lwd = lwd, lty = lty, lineend = lineend)
      )
      maxCap <- grid::segmentsGrob(
        x0 = rangeMax,
        x1 = rangeMax,
        y0 = center - halfWidthMm,
        y1 = center + halfWidthMm,
        gp = grid::gpar(col = col, lwd = lwd, lty = lty, lineend = lineend)
      )
    } else {
      if (
        length(coords$x) == 0 ||
          length(coords$ymin) == 0 ||
          length(coords$ymax) == 0
      ) {
        return(grid::nullGrob())
      }
      center <- grid::unit(coords$x, "npc")
      rangeMin <- grid::unit(coords$ymin, "npc")
      rangeMax <- grid::unit(coords$ymax, "npc")

      mainSeg <- grid::segmentsGrob(
        x0 = center,
        x1 = center,
        y0 = rangeMin,
        y1 = rangeMax,
        gp = grid::gpar(col = col, lwd = lwd, lty = lty, lineend = lineend)
      )
      minCap <- grid::segmentsGrob(
        x0 = center - halfWidthMm,
        x1 = center + halfWidthMm,
        y0 = rangeMin,
        y1 = rangeMin,
        gp = grid::gpar(col = col, lwd = lwd, lty = lty, lineend = lineend)
      )
      maxCap <- grid::segmentsGrob(
        x0 = center - halfWidthMm,
        x1 = center + halfWidthMm,
        y0 = rangeMax,
        y1 = rangeMax,
        gp = grid::gpar(col = col, lwd = lwd, lty = lty, lineend = lineend)
      )
    }

    grid::grobTree(mainSeg, minCap, maxCap)
  }
)
