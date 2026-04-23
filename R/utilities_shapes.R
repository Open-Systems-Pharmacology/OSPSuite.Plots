#' @title OSP Shape Names
#' @description Character vector of all available OSP shape names.
#' @export
ospShapeNames <- c(
  # Filled shapes first (for distinct visual differentiation)
  "circle",
  "diamond",
  "square",
  "triangle",
  "invertedTriangle",
  "pentagon",
  "hexagon",
  "star",
  "plus",
  "cross",
  "asterisk",
  # Open shapes
  "circleOpen",
  "diamondOpen",
  "squareOpen",
  "triangleOpen",
  "invertedTriangleOpen",
  "pentagonOpen",
  "hexagonOpen",
  "starOpen",
  "thinPlus",
  "thinCross",
  "blank"
)

#' @title Shapes
#' @description Named list of OSP shape names for backward compatibility.
#' Use `Shapes$circle` to get the shape name "circle".
#' @family setDefault functions
#' @export
Shapes <- stats::setNames(as.list(ospShapeNames), ospShapeNames)

# Calibration constants (visually matched against ggplot2::geom_point pch=21)
.kSizeMultiplier <- 0.92
.kStrokeLwdMultiplier <- 1.9


#' Validate shape name, falling back to "circle" for unknown shapes
#' @param name shape name to validate
#' @return valid shape name (original if known, "circle" otherwise)
#' @keywords internal
.validateShapeName <- function(name) {
  name <- as.character(name)
  if (!name %in% ospShapeNames) "circle" else name
}

#' Regular n-gon vertices on unit circle
#' @param n number of vertices
#' @param angleDeg angle of first vertex in degrees
#' @return list with x and y coordinate vectors
#' @keywords internal
.polyVertices <- function(n, angleDeg = 0) {
  theta <- seq(0, 2 * pi, length.out = n + 1)[-(n + 1)] + angleDeg * pi / 180
  list(x = cos(theta), y = sin(theta))
}

#' Star vertices with alternating outer/inner radius
#' @param points number of star points
#' @param innerRatio ratio of inner to outer radius
#' @return list with x and y coordinate vectors
#' @keywords internal
.starVertices <- function(points = 5, innerRatio = 0.4) {
  n <- 2 * points
  theta <- seq(pi / 2, pi / 2 + 2 * pi, length.out = n + 1)[-(n + 1)]
  r <- rep(c(1, innerRatio), length.out = n)
  list(x = r * cos(theta), y = r * sin(theta))
}

#' Shape specifications for OSP shapes
#' @keywords internal
.ospShapeSpec <- list(
  circle = list(kind = "polygon", n = 64, angle = 0, open = FALSE),
  circleOpen = list(kind = "polygon", n = 64, angle = 0, open = TRUE),
  square = list(kind = "polygon", n = 4, angle = 45, open = FALSE, scale = 1.41),
  squareOpen = list(kind = "polygon", n = 4, angle = 45, open = TRUE, scale = 1.41),
  diamond = list(kind = "polygon", n = 4, angle = 0, open = FALSE),
  diamondOpen = list(kind = "polygon", n = 4, angle = 0, open = TRUE),
  triangle = list(kind = "polygon", n = 3, angle = 90, open = FALSE, scale = 1.23, yOffset = -0.25),
  triangleOpen = list(kind = "polygon", n = 3, angle = 90, open = TRUE, scale = 1.23, yOffset = -0.25),
  invertedTriangle = list(kind = "polygon", n = 3, angle = -90, open = FALSE, scale = 1.23, yOffset = 0.25),
  invertedTriangleOpen = list(kind = "polygon", n = 3, angle = -90, open = TRUE, scale = 1.23, yOffset = 0.25),
  pentagon = list(kind = "polygon", n = 5, angle = 90, open = FALSE, scale = 1.08),
  pentagonOpen = list(kind = "polygon", n = 5, angle = 90, open = TRUE, scale = 1.08),
  hexagon = list(kind = "polygon", n = 6, angle = 90, open = FALSE, scale = 1.07),
  hexagonOpen = list(kind = "polygon", n = 6, angle = 90, open = TRUE, scale = 1.07),
  star = list(kind = "star", points = 5, open = FALSE, scale = 1.08),
  starOpen = list(kind = "star", points = 5, open = TRUE, scale = 1.08),
  plus = list(kind = "stroke", glyph = "plus", thick = TRUE),
  thinPlus = list(kind = "stroke", glyph = "plus", thick = FALSE),
  cross = list(kind = "stroke", glyph = "cross", thick = TRUE),
  thinCross = list(kind = "stroke", glyph = "cross", thick = FALSE),
  asterisk = list(kind = "stroke", glyph = "asterisk", thick = TRUE),
  blank = list(kind = "blank")
)

#' Build grid grob for OSP shape
#' @param name shape name
#' @param cx center x (grid unit)
#' @param cy center y (grid unit)
#' @param half half-size (grid unit)
#' @param fill fill color
#' @param colour stroke color
#' @param stroke stroke width
#' @param alpha alpha value
#' @return grid grob
#' @keywords internal
.ospGrob <- function(name, cx, cy, half, fill, colour, stroke, alpha) {
  spec <- .ospShapeSpec[[name]]
  if (is.null(spec)) {
    return(grid::nullGrob())
  }

  # Convert stroke to lwd matching ggplot2's geom_point rendering
  # (calibrated visually against geom_point pch=21 at stroke=0.5)
  strokeLwd <- stroke * .kStrokeLwdMultiplier

  # Apply shape-specific scaling factor (some shapes need to be larger to match ggplot2)
  shapeScale <- spec$scale %||% 1
  scaledHalf <- half * shapeScale

  switch(
    spec$kind,

    polygon = {
      # Filled shapes: use colour for fill; Open shapes: use fill aesthetic
      fg <- if (isTRUE(spec$open)) fill else colour
      v <- .polyVertices(spec$n, spec$angle)
      # Apply y-offset for shapes that need bounding-box centering (e.g., triangles)
      yOff <- (spec$yOffset %||% 0) * scaledHalf
      grid::polygonGrob(
        x = cx + v$x * scaledHalf,
        y = cy + v$y * scaledHalf + yOff,
        gp = grid::gpar(fill = fg, col = colour, lwd = strokeLwd, alpha = alpha)
      )
    },

    star = {
      # Filled shapes: use colour for fill; Open shapes: use fill aesthetic
      fg <- if (isTRUE(spec$open)) fill else colour
      v <- .starVertices(spec$points)
      grid::polygonGrob(
        x = cx + v$x * scaledHalf,
        y = cy + v$y * scaledHalf,
        gp = grid::gpar(fill = fg, col = colour, lwd = strokeLwd, alpha = alpha)
      )
    },

    stroke = {
      # Thick strokes (plus, cross, asterisk) need heavier weight for visibility
      lwd <- if (isTRUE(spec$thick)) strokeLwd * 2.2 else strokeLwd
      gpStroke <- grid::gpar(col = colour, lwd = lwd, alpha = alpha, lineend = "butt")
      switch(
        spec$glyph,
        plus = grid::segmentsGrob(
          x0 = grid::unit.c(cx - scaledHalf, cx),
          y0 = grid::unit.c(cy, cy - scaledHalf),
          x1 = grid::unit.c(cx + scaledHalf, cx),
          y1 = grid::unit.c(cy, cy + scaledHalf),
          gp = gpStroke
        ),
        cross = grid::segmentsGrob(
          x0 = grid::unit.c(cx - scaledHalf, cx - scaledHalf),
          y0 = grid::unit.c(cy - scaledHalf, cy + scaledHalf),
          x1 = grid::unit.c(cx + scaledHalf, cx + scaledHalf),
          y1 = grid::unit.c(cy + scaledHalf, cy - scaledHalf),
          gp = gpStroke
        ),
        asterisk = {
          theta <- c(pi / 2, pi / 2 + pi / 3, pi / 2 + 2 * pi / 3)
          grid::segmentsGrob(
            x0 = cx - cos(theta) * scaledHalf,
            y0 = cy - sin(theta) * scaledHalf,
            x1 = cx + cos(theta) * scaledHalf,
            y1 = cy + sin(theta) * scaledHalf,
            gp = gpStroke
          )
        },
        grid::nullGrob()
      )
    },

    blank = grid::nullGrob(),

    grid::nullGrob()
  )
}

#' @title GeomPointOsp
#' @description ggproto object for OSP point shapes.
#' @format NULL
#' @usage NULL
#' @export
GeomPointOsp <- ggplot2::ggproto(
  "GeomPointOsp",
  ggplot2::Geom,

  required_aes = c("x", "y"),

  default_aes = ggplot2::aes(
    colour = "black",
    fill = NA,
    size = 1.5,
    stroke = 0.5,
    alpha = NA,
    shape = "circle"
  ),

  draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
    coords <- coord$transform(data, panel_params)

    # Warn once per unique unknown shape
    unknownShapes <- setdiff(unique(as.character(coords$shape)), ospShapeNames)
    if (length(unknownShapes) > 0) {
      warning(
        "Unknown shape(s): ", paste(shQuote(unknownShapes), collapse = ", "),
        ". Using 'circle' instead.",
        call. = FALSE
      )
    }

    grobs <- lapply(seq_len(nrow(coords)), function(i) {
      name <- .validateShapeName(coords$shape[i])
      .ospGrob(
        name = name,
        cx = grid::unit(coords$x[i], "native"),
        cy = grid::unit(coords$y[i], "native"),
        half = grid::unit(coords$size[i] * .kSizeMultiplier / 2, "mm"),
        fill = scales::alpha(coords$fill[i], coords$alpha[i] %||% 1),
        colour = scales::alpha(coords$colour[i], coords$alpha[i] %||% 1),
        stroke = coords$stroke[i],
        alpha = 1
      )
    })

    do.call(grid::grobTree, grobs)
  },

  draw_key = function(data, params, size) {
    name <- .validateShapeName(data$shape %||% "circle")
    pointSize <- data$size %||% 1.5
    keyAlpha <- if (is.na(data$alpha)) 1 else data$alpha %||% 1
    .ospGrob(
      name = name,
      cx = grid::unit(0.5, "npc"),
      cy = grid::unit(0.5, "npc"),
      half = grid::unit(pointSize * .kSizeMultiplier / 2, "mm"),
      fill = scales::alpha(data$fill %||% NA, keyAlpha),
      colour = scales::alpha(data$colour %||% "black", keyAlpha),
      stroke = data$stroke %||% 0.5,
      alpha = 1
    )
  }
)

#' @title OSP Point Layer
#' @description
#' A geom that renders OSP shapes using grid primitives.
#' Uses shape names from `ospShapeNames`. Automatically applies
#' `scale_shape_osp()` when added to a plot (unless a shape scale
#' is already present).
#'
#' @inheritParams ggplot2::geom_point
#' @param na.rm If `FALSE` (default), missing values are removed with a warning.
#'   If `TRUE`, missing values are silently removed.
#' @export
#' @family setDefault functions
#' @examples
#' library(ggplot2)
#' df <- data.frame(x = 1:5, y = 1:5, shape = ospShapeNames[1:5])
#' ggplot(df, aes(x, y, shape = shape)) +
#'   geom_point_osp(size = 4) +
#'   scale_shape_osp_identity()
geom_point_osp <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  layer <- ggplot2::layer(
    geom = GeomPointOsp,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
  class(layer) <- c("geom_point_osp_layer", class(layer))
  layer
}

#' @export
#' @method ggplot_add geom_point_osp_layer
ggplot_add.geom_point_osp_layer <- function(object, plot, ...) {
  class(object) <- setdiff(class(object), "geom_point_osp_layer")
  plot <- plot + object
  class(plot) <- unique(c("osp_ggplot", class(plot)))
  plot
}

#' @export
#' @method ggplot_build osp_ggplot
ggplot_build.osp_ggplot <- function(plot, ...) {
  hasShapeScale <- any(vapply(
    plot$scales$scales,
    function(s) "shape" %in% s$aesthetics,
    logical(1)
  ))
  if (!hasShapeScale) {
    plot <- plot + scale_shape_osp()
  }
  class(plot) <- setdiff(class(plot), "osp_ggplot")
  ggplot2::ggplot_build(plot)
}

#' @title OSP Shape Scale
#' @description
#' Discrete shape scale that automatically assigns shapes from
#' `ospShapeNames` in order based on the number of factor levels.
#' Equivalent to `ggplot2::scale_shape()`.
#'
#' If there are more levels than available shapes, shapes are recycled
#' and a warning is issued.
#'
#' @param ... Passed to `ggplot2::discrete_scale`.
#' @export
#' @family setDefault functions
#' @examples
#' library(ggplot2)
#' df <- data.frame(x = 1:3, y = 1:3, group = c("A", "B", "C"))
#' ggplot(df, aes(x, y, shape = group)) +
#'   geom_point_osp(size = 4) +
#'   scale_shape_osp()
scale_shape_osp <- function(...) {
  ggplot2::discrete_scale(
    aesthetics = "shape",
    palette = .ospShapePalette,
    ...
  )
}

#' OSP shape palette function
#' @param n Number of shapes needed
#' @return Character vector of shape names
#' @keywords internal
.ospShapePalette <- function(n) {
  visibleShapes <- setdiff(ospShapeNames, "blank")
  nShapes <- length(visibleShapes)
  if (n > nShapes) {
    warning(
      "Number of groups (", n, ") exceeds available shapes (", nShapes, "). ",
      "Shapes will be recycled.",
      call. = FALSE
    )
  }
  visibleShapes[((seq_len(n) - 1) %% nShapes) + 1]
}

#' @title OSP Shape Manual Scale
#' @description
#' Manual shape scale for explicit mapping of factor levels to OSP shape names.
#' Equivalent to `ggplot2::scale_shape_manual()`.
#'
#' @param values Named character vector. Names are factor levels;
#'   values are entries from `ospShapeNames`.
#' @param ... Passed to `ggplot2::scale_shape_manual`.
#' @export
#' @family setDefault functions
#' @examples
#' library(ggplot2)
#' df <- data.frame(x = 1:3, y = 1:3, group = c("A", "B", "C"))
#' ggplot(df, aes(x, y, shape = group)) +
#'   geom_point_osp(size = 4) +
#'   scale_shape_osp_manual(values = c(A = "circle", B = "diamond", C = "star"))
scale_shape_osp_manual <- function(values, ...) {
  bad <- setdiff(values, ospShapeNames)
  if (length(bad) > 0) {
    stop(
      "Values not in ospShapeNames: ",
      paste(shQuote(bad), collapse = ", "),
      call. = FALSE
    )
  }
  ggplot2::scale_shape_manual(values = values, ...)
}

#' @title OSP Shape Identity Scale
#' @description
#' Identity scale for when data already contains OSP shape names.
#' Use this when your shape column contains values from `ospShapeNames`
#' directly (e.g., "circle", "diamond", "star").
#' Equivalent to `ggplot2::scale_shape_identity()`.
#'
#' @param guide Guide for the legend. Use `"legend"` to show a legend,
#'   or `"none"` to hide it.
#' @param ... Passed to `ggplot2::scale_shape_manual`.
#' @export
#' @family setDefault functions
#' @examples
#' library(ggplot2)
#' df <- data.frame(x = 1:3, y = 1:3, shape = c("circle", "diamond", "star"))
#' ggplot(df, aes(x, y, shape = shape)) +
#'   geom_point_osp(size = 4) +
#'   scale_shape_osp_identity(guide = "legend")
scale_shape_osp_identity <- function(guide = "none", ...) {
  values <- stats::setNames(ospShapeNames, ospShapeNames)
  ggplot2::scale_shape_manual(values = values, guide = guide, ...)
}

