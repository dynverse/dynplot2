# TODO: train so that text does not get clipped

ScaleXHeatmap <- ggproto(
  "ScaleXHeatmap",
  scale_x_continuous(expand = c(0.5, 0.5))
)

#' @export
scale_x_heatmap <- function(...){
  ggproto(
    "",
    ScaleXHeatmap,
    ...
  )
}
