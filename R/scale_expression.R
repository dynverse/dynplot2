# TODO: add rescaling, e.g. quantile

common_colorbar_legend <- guide_colorbar(
  title.position = "top",
  title.hjust = 0.5,

  frame.colour = "#333333",

  ticks = FALSE
)


# from https://carto.com/carto-colors/
ScaleExpressionColour <- ggproto(
  "ScaleExpressionColour",
  scale_colour_gradientn(colours = stringr::str_split("#fcde9c,#faa476,#f0746e,#e34f6f,#dc3977,#b9257a,#7c1d6f", ",")[[1]]),
  # scale_colour_distiller(type = "seq", palette = "OrRd", direction = 1),
  aesthetics = c("colour"),
  oob = scales::squish,
  guide = common_colorbar_legend
)


#' @export
scale_expression_colour <- scale_expression_color <- function(name = "Expression", ...){
  ggproto(
    "ScaleExpressionColour",
    ScaleExpressionColour,
    name = name,
    ...
  )
}


ScaleExpressionFill <- ggproto(
  "ScaleExpressionFill",
  scale_fill_gradientn(colours = stringr::str_split("#fcde9c,#faa476,#f0746e,#e34f6f,#dc3977,#b9257a,#7c1d6f", ",")[[1]]),
  # scale_colour_distiller(type = "seq", palette = "OrRd", direction = 1),
  aesthetics = c("fill"),
  oob = scales::squish,
  guide = common_colorbar_legend
)


#' @export
scale_expression_fill <- function(name = "Expression", ...){
  ggproto(
    "ScaleExpressionFil",
    ScaleExpressionFill,
    name = name,
    ...
  )
}




scale_velocity_fill <- function(name = "Velocity", guide = common_colorbar_legend, ...) {
  center_limits <- function(limits) {
    if (abs(limits[[1]]) > abs(limits[[2]])) {
      limits[[2]] <- abs(limits[[1]])
    } else if (abs(limits[[2]]) > abs(limits[[1]])) {
      limits[[1]] <- -abs(limits[[2]])
    }
    limits
  }

  scale <- scale_color_distiller(
    palette = "RdBu",
    aesthetics = c("fill"),
    limits = center_limits,
    name = name,
    guide = guide,
    ...
  )
}


scale_velocity_color <- function(name = "Velocity", guide = common_colorbar_legend, ...) {
  center_limits <- function(limits) {
    if (abs(limits[[1]]) > abs(limits[[2]])) {
      limits[[2]] <- abs(limits[[1]])
    } else if (abs(limits[[2]]) > abs(limits[[1]])) {
      limits[[1]] <- -abs(limits[[2]])
    }
    limits
  }

  scale <- scale_color_distiller(
    palette = "RdBu",
    aesthetics = c("color"),
    limits = center_limits,
    name = name,
    guide = guide,
    ...
  )
}




ScaleContinuousCentered <- ggproto(
  "ScaleContinuousCentered",
  ScaleContinuous
)
