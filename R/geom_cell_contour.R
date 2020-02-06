GeomCellContour <- ggproto(
  "GeomCellContour",
  GeomPolygon,
  default_aes = aesIntersect(aes(alpha = 0.2), GeomPolygon$default_aes)
)

StatCellContour <- ggproto(
  "StatCellContour",
  StatDensity2d,
  setup_params = function(data, params) {
    xlims <- c(min(data$x), max(data$x))
    ylims <- c(min(data$y), max(data$y))

    # determine bandwidth
    assert_that(!is.null(params$relative_bandwidth))
    params$bandwidth_y <- diff(ylims) * params$relative_bandwidth
    params$bandwidth_x <- diff(xlims) * params$relative_bandwidth

    # determine padding
    assert_that(!is.null(params$padding))

    xpad <- diff(xlims) * params$padding
    ypad <- diff(ylims) * params$padding

    xlims <- xlims + c(-xpad, xpad)
    ylims <- ylims + c(-ypad, ypad)

    params$xlims <- xlims
    params$ylims <- ylims

    params
  },
  compute_group = function(data, scales, bandwidth_x, bandwidth_y, xlims, ylims, na.rm = F, resolution = 200, relative_density_cutoff = 0.2, relative_bandwidth = NULL, padding = NULL) {
    density <- MASS::kde2d(data$x, data$y, h = c(bandwidth_x, bandwidth_y), lims = c(xlims, ylims), n = resolution)
    df <- expand.grid(x = density$x, y = density$y)
    df$group <- data$group[1]
    df$z <- as.vector(density$z)

    density_cutoff <- min(df$z) + (max(df$z) - min(df$z)) * relative_density_cutoff

    output2 <- ggplot2:::contour_lines(df, breaks = density_cutoff, complete = FALSE)

    output2
  }
)

#' Plot contour around cells based
#'
#' @param relative_density_cutoff At whtat level of density the contour should be drawn, should be between 0 and 1
#' @param relative_bandwidth Bandwidth calculated relative to the x and y limits of the points, should be between 0 and 1
#' @param resolution The higher, the more accurate the polygon will be drawn at the cost of longer computing/drawing time
#' @param padding How much padding to add to the limits, to avoid the contour to be drawn outside the plot
geom_cell_contour <- function(
  mapping = NULL,
  data = construct_get_cell_info(),
  relative_density_cutoff = 0.2,
  relative_bandwidth = 0.2,
  padding = 1,
  resolution = 200,
  ...,
  show.legend = NA
) {
  mapping <- aesIntersect(mapping, aes_(x=~x, y=~y, group=~group_id))

  layer(
    data = data,
    mapping = mapping,
    stat = StatCellContour,
    geom = GeomCellContour,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = FALSE,
    params = lst(
      na.rm = FALSE,
      resolution = resolution,
      padding = padding,
      relative_bandwidth = relative_bandwidth,
      relative_density_cutoff = relative_density_cutoff,
      ...
    )
  )
}
