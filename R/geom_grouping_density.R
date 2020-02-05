GeomGroupingDensity <- ggproto(
  "GeomGroupingDensity",
  GeomPolygon,
  default_aes = aesIntersect(GeomPolygon$default_aes, GeomPolygon$default_aes)
)

StatGroupingDensity <- ggproto(
  "StatGroupingDensity",
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
  compute_group = function(data, scales, bandwidth_x, bandwidth_y, xlims, ylims, na.rm = F,  resolution = 200, density_cutoff = 1, relative_bandwidth = NULL, padding = NULL) {
    density <- MASS::kde2d(data$x, data$y, h = c(bandwidth_x, bandwidth_y), lims = c(xlims, ylims), n = resolution)
    df <- expand.grid(x = density$x, y = density$y)
    df$group <- data$group[1]
    df$z <- as.vector(density$z)

    output2 <- ggplot2:::contour_lines(df, breaks = density_cutoff, complete = TRUE)

    output2
  }
)

geom_grouping_density <- function(
  mapping = NULL,
  data = construct_get_density_info(),
  relative_bandwidth = 0.2,
  padding = 0.2,
  resolution = 200,
  ...,
  show.legend = NA
) {
  mapping <- aesIntersect(mapping, aes_(x=~x, y=~y, group=~group_id))

  layer(
    data = data,
    mapping = mapping,
    stat = StatGroupingDensity,
    geom = GeomGroupingDensity,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = FALSE,
    params = lst(
      na.rm = FALSE,
      resolution = resolution,
      padding = padding,
      relative_bandwidth = relative_bandwidth,
      ...
    )
  )
}


construct_get_density_info <- construct_get_cell_info
