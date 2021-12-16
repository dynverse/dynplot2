GeomCellContourLabel <- ggproto(
  "GeomCellContourLabel",
  ggrepel::GeomLabelRepel
)

StatCellContourLabel <- ggproto(
  "StatCellContourLabel",
  StatDensity2d,
  setup_params = function(data, params) {
    xlims <- c(min(data$x), max(data$x))
    ylims <- c(min(data$y), max(data$y))

    # determine bandwidth
    assert_that(!is.null(params$relative_bandwidth))
    params$bandwidth_y <- diff(ylims) * params$relative_bandwidth
    params$bandwidth_x <- diff(xlims) * params$relative_bandwidth

    params$xlims <- xlims
    params$ylims <- ylims

    params
  },
  compute_group = function(data, scales, bandwidth_x, bandwidth_y, xlims, ylims, na.rm = F, relative_bandwidth = NULL) {
    density <- MASS::kde2d(data$x, data$y, h = c(bandwidth_x, bandwidth_y), lims = c(xlims, ylims), n = 100)
    df <- expand.grid(x = density$x, y = density$y)
    df$group <- data$group[1]
    df$z <- as.vector(density$z)

    row <- df[which.max(df$z),]
    row <- cbind(row, data[1, !colnames(data) %in% c("x", "y")])
    row
  }
)

#' Plot contour around cells based on their density
#'
#' @inheritParams ggrepel::geom_label_repel
#' @inheritParams geom_cell_contour
geom_cell_contour_label <- function(
  mapping = NULL,
  data = construct_get_cell_info(),
  relative_bandwidth = 0.2,
  min.segment.length = Inf,
  ...,
  show.legend = NA
) {
  mapping <- aesIntersect(mapping, aes_(x=~x, y=~y))

  layer(
    data = data,
    mapping = mapping,
    stat = StatCellContourLabel,
    geom = GeomCellContourLabel,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = FALSE,
    params = lst(
      na.rm = FALSE,
      relative_bandwidth = relative_bandwidth,
      min.segment.length = min.segment.length,
      ...
    )
  )
}
