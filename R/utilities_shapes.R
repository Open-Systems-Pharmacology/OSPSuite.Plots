#' @title Shapes
#' @description List of some `ggplot2` shapes.
#' The shapes from this list are unicode characters
#' corresponding to their appropriate shapes.
#'
#' @export
Shapes <- list(
  # Usual symbols
  "circle" = "\u25cf",
  "diamond" = "\u25c6",
  "triangle" = "\u25b2",
  "square" = "\u25a0",
  "invertedTriangle" = "\u25bc",
  "cross" = "\ud83d\udfad",
  "thinCross" = "\ud83d\udfa9",
  "plus" = "\ud83d\udfa6",
  "thinPlus" = "\ud83d\udfa2",
  "asterisk" = "\ud83d\udfbc",
  "star" = "\ud83d\udfca",
  "pentagon" = "\u2b1f",
  "hexagon" = "\u2b22",
  # open shapes
  "circleOpen" = "\ud83d\udf85",
  "diamondOpen" = "\u25c7",
  "triangleOpen" = "\u25b3",
  "squareOpen" = "\u25A1", # "\ud83d\udf90",
  "invertedTriangleOpen" = "\u25bd",
  "starOpen" = "\u2729", # "\u2606",
  "pentagonOpen" = "\u2b20",
  "hexagonOpen" = "\u2b21",
  # Emojis
  "male" = "\u2642",
  "female" = "\u2640",
  "man" = "\ud83d\udeb9",
  "woman" = "\ud83d\udeba",
  "baby" = "\ud83d\udebc",
  "mouse" = "\ud83d\udc01",
  "cat" = "\ud83d\udc08",
  "rat" = "\ud83d\udc00",
  "rabbit" = "\ud83d\udc07",
  "dog" = "\ud83d\udc15",
  "pig" = "\ud83d\udc16",
  "sheep" = "\ud83d\udc11",
  "cow" = "\ud83d\udc04",
  "monkey" = "\ud83d\udc12",
  "human" = "\ud83d\udeb6",
  "pill" = "\ud83d\udc8a",
  "syringe" = "\ud83d\udc89",
  "hazard" = "\u2622",
  # No shape displayed
  "blank" = " "
)



#' @title GeomTLFPoint
#' @description
#' Define a Geom using `ggplot2::ggproto()` and based on GeomPoint.
#' The Geom internally uses `textGrob` instead of `pointsGrob` so that fonts leverage for drawing shapes.
#' The `grid` and `scales` packages are supposed to be required by `ggplot2`.
#' So there should not be any issue as installing `ggplot2` should install those 2 packages.
#' @keywords internal
#'
GeomPointUnicode <- ggplot2::ggproto(
  "GeomTLFPoint",
  GeomPoint,
  # This will correspond to the default property displayed in legend
  # if property not used in data mapping
  # this replaces displayed "19" by a colored square
  default_aes = ggplot2::aes(
    shape = "\u2588", colour = "black", size = 1.5, fill = NA,
    alpha = NA, stroke = 0.5
  ),
  draw_panel = function(data, panel_params, coord) {
    coords <- coord$transform(data, panel_params)
    # Replace grid::pointsGrob from geom_point accounting for font family
    grid::textGrob(
      # If shape is included in plot dictionary, use it
      label = .asPlotShape(coords$shape),
      x = coords$x, y = coords$y,
      default.units = "native",
      gp = grid::gpar(
        col = scales::alpha(coords$colour %||% "black", coords$alpha),
        fill = scales::alpha(coords$fill %||% "black", coords$alpha),
        fontsize = coords$size * ggplot2::.pt,
        fontfamily = .selectFontFamily()
      )
    )
  },
  draw_key = function(data, params, size) {
    if (is.null(data$shape)) {
      data$shape <- Shapes$blank
    }
    # NULL means the default stroke size, and NA means no stroke.
    stroke_size <- data$stroke %||% 0.5
    stroke_size[is.na(stroke_size)] <- 0

    # Replace grid::pointsGrob from geom_point accounting for font family
    grid::textGrob(
      # If shape is included in plot dictionary, use it in legend
      # Prevents having "circle" instead of its shape displayed in the legend
      label = .asPlotShape(data$shape),
      x = 0.5, y = 0.5,
      # Code copied from ggplot2 except for font family
      gp = grid::gpar(
        col = scales::alpha(data$colour %||% "black", data$alpha),
        fill = scales::alpha(data$fill %||% "black", data$alpha),
        fontsize = (data$size %||% 1.5) * .pt + stroke_size * .stroke / 2,
        lwd = stroke_size * .stroke / 2,
        fontfamily = .selectFontFamily()
      )
    )
  }
)

#' @title geomTLFPoint
#' @description
#' geom similar to `geom_point()` but that leverage fonts to draw its shapes
#' @param mapping mapping from `ggplot2` package as provided by `aes()`
#' @param data data.frame
#' @param stat stat name from `ggplot2`
#' @param position position name from `ggplot2`
#' @param show.legend = NA,
#' @param na.rm a logical value indicating
#' @param inherit.aes a logical value indicating if aesthetics are inherited
#' @param ... other arguments.
#' @export
#'
geomPointUnicode <- function(mapping = NULL, data = NULL, stat = "identity",
                             position = "identity", na.rm = FALSE, show.legend = NA,
                             inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomPointUnicode, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' @title .selectFontFamily
#' @description
#' Select appropriate font family based on font and `showtext` package availability
#' @param fontfamily default font family
#' @keywords internal
#'
.selectFontFamily <- function(fontfamily = "sans") {
  if (!requireNamespace("showtext", quietly = TRUE)) {
    return(fontfamily)
  }
  # sysfonts is required by showtext
  # thus, installing showtext also installs sysfonts
  # when loading the tlf package,
  # the Symbola font family is added to the sysfont font families
  # however in some environments such as devtools::check(),
  # it seems that the family is removed from that list
  # consequently, this line perform a last check
  # of font availability before displaying the grob
  if (!isIncluded("Symbola", sysfonts::font_families())) {
    return(fontfamily)
  }
  return("Symbola")
}


#' converts shape name to value
#'
#' @param shapes  shape may be valid shape or name entry of list Shapes
#' @keywords internal
#'
#' @return  shapevalue
.asPlotShape <- function(shapes) {
  ggplotShapes <- NULL
  for (shape in shapes) {
    ggplotShape <- as.character(shape)
    if (isIncluded(ggplotShape, names(Shapes))) {
      ggplotShape <- Shapes[[shape]]
    }
    ggplotShapes <- c(ggplotShapes, ggplotShape)
  }
  return(ggplotShapes)
}
