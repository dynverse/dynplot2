# TODO: add rescaling, e.g. quantile

ScaleExpressionFillColour <- ggproto(
  "ScaleExpressionFillColour",
  scale_colour_distiller(type = "div", palette = "RdBu"),
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






  scale_velocity_color <- function() {scale_color_distiller(palette = "RdBu", breaks = c(-1, 0, 1))}
