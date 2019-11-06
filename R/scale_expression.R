# TODO: add rescaling, e.g. quantile

# from https://carto.com/carto-colors/
ScaleExpressionFillColour <- ggproto(
  "ScaleExpressionFillColour",
  scale_colour_gradientn(colours = stringr::str_split("#fcde9c,#faa476,#f0746e,#e34f6f,#dc3977,#b9257a,#7c1d6f", ",")[[1]]),
  # scale_colour_distiller(type = "seq", palette = "OrRd", direction = 1),
  aesthetics = c("fill", "colour"),
  oob = scales::squish
)


#' @export
scale_expression_fillcolour <- function(...){
  ggproto(
    "",
    ScaleExpressionFillColour,
    ...
  )
}






scale_velocity_fillcolor <- function() {
  center_limits <- function(limits) {
    if (abs(limits[[1]] > abs(limits[[2]]))) {
      limits[[2]] <- abs(limits[[1]])
    } else if (abs(limits[[2]]) > abs(limits[[1]])) {
      limits[[1]] <- -abs(limits[[2]])
    }
    limits
  }

  scale <- scale_color_distiller(
    palette = "RdBu",
    aesthetics = c("fill", "color"),
    limits = center_limits
  )
}





ScaleContinuousCentered <- ggproto(
  "ScaleContinuousCentered",
  ScaleContinuous
)
