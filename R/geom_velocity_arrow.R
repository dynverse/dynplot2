GeomVelocityArrow <- ggproto(
  "GeomVelocityArrow",
  GeomSegment,
  default_aes = aesIntersect(GeomSegment$default_aes, aes(color = "black", linejoin = "mitre", lineend = "butt", length = 1)),
  draw_panel = function(self, data, panel_params, coord, arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm"))) {
    original_draw_panel <- GeomSegment$draw_panel
#
#     relative_arrow_size <- dynutils::scale_minmax(data$length)
#     arrow <- arrow(
#       length = unit(relative_arrow_size, "cm")
#     )

    original_draw_panel(
      data = data,
      panel_params = panel_params,
      coord = coord,
      arrow = arrow,
      linejoin = data$linejoin[[1]],
      lineend = data$lineend[[1]]
    )
  }
)

#' Plotting velocity
#'
#' @inheritParams ggplot2::geom_segment
#' @param stat Where to place the arrows, such as for every cell ([stat_velocity_cells()]) or using a grid ([stat_velocity_grid()])
#' @param data A function created by [construct_get_velocity_info()]
#'
#'
#' @rdname geom_velocity
#'
#' @export
geom_velocity_arrow <- function(
  mapping = NULL,
  stat = stat_velocity_grid(),
  arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm")),
  ...,
  data = construct_get_velocity_info(stat),
  show.legend = NA
) {
  mapping <- aesIntersect(mapping, aes_(x=~x, y=~y, xend=~x_future, yend=~y_future, length=~length))
  layer(
    data = data,
    mapping = mapping,
    stat = StatIdentity,
    geom = GeomVelocityArrow,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = FALSE,
    params = lst(
      na.rm = FALSE,
      arrow = arrow,
      ...
    )
  )
}


construct_get_velocity_info <- function(position) {
  get_velocity_info <- function(data) {
    cell_positions <- attr(data, "data")$cell_info
    assert_that(
      c("x", "y", "x_future", "y_future") %all_in% colnames(cell_positions),
      msg = "This layout does not contain information on velocity"
    )

    position$data(data)
  }
  get_velocity_info
}


#' @param cell_positions Dataframe contains at least x, y, x_future and y_future
embed_arrows_cells <- function(cell_positions) {
  cell_positions %>%
    mutate(
      length = sqrt((y_future - y)**2 + (x_future - x)**2),
    )
}

#' @param cell_positions Dataframe contains at least x, y, x_future and y_future
embed_arrows_random <- function(cell_positions, n_cells = 100) {
  embed_arrows_cells(cell_positions) %>%
    sample_n(min(nrow(cell_positions), n_cells))
}



#' @param cell_positions The dimensionality reduction which contains at least x, y, x_future and y_future
#' @param grid_n Number of rows and columns in the grid
#' @param grid_bandwidth Bandwidth relative to the spacing of the grid
embed_arrows_grid <- function(
  cell_positions,
  grid_n = c(15, 15),
  grid_bandwidth = 1/3,
  max_arrow_length = NULL,
  filter = rlang::quo(mass > max(mass) * 0.1)
) {
  assert_that(is.data.frame(cell_positions))
  assert_that(c("x", "y", "x_future", "y_future") %all_in% colnames(cell_positions))

  if (length(grid_n) == 1) {
    grid_n <- c(grid_n, grid_n)
  }
  assert_that(length(grid_n) == 2)
  assert_that(all(grid_n > 2))

  grid_w <- grid_n[1]
  grid_h <- grid_n[2]

  # calculate grid points
  range_x <- range(unlist(cell_positions[, c("x", "x_future")]), na.rm = TRUE)
  range_y <- range(unlist(cell_positions[, c("y", "y_future")]), na.rm = TRUE)
  grid_x <- seq(range_x[1],range_x[2],length.out=grid_w)
  grid_y <- seq(range_y[1],range_y[2],length.out=grid_h)

  diff_x <- grid_x[2] - grid_x[1]
  diff_y <- grid_y[2] - grid_y[1]

  grid_sd <- sqrt((diff_x)^2 + (diff_y)^2) * grid_bandwidth
  if(is.null(max_arrow_length)) {
    max_arrow_length <- min(c(diff_x, diff_y))
  }

  cell_positions_difference <- tibble(
    x = cell_positions$x_future - cell_positions$x,
    y = cell_positions$y_future - cell_positions$y
  )

  # calculate for each gaussian the smoothed arrow using a gaussian kernel
  garrows <- map_dfr(grid_x, function(x) {
    # cell distances and weights to each grid point
    cd <- sqrt(outer(cell_positions$y,-grid_y,'+')^2 + (x-cell_positions$x)^2)
    # cw <- dexp(cd, 100)
    # cw <- dnorm(cd,sd=grid_sd)
    cw <- cd < grid_sd

    # calculate the actual arrow
    gw <- Matrix::colSums(cw)
    cws <- pmax(1,Matrix::colSums(cw))
    gxd <- Matrix::colSums(cw*cell_positions_difference$x)/cws
    gyd <- Matrix::colSums(cw*cell_positions_difference$y)/cws

    arrow_length <- sqrt(gxd^2+gyd^2)

    tibble(
      x = x,
      y = grid_y,
      x_difference = gxd,
      y_difference = gyd,
      length = arrow_length,
      angle = atan2(y_difference, x_difference),
      mass = gw
    )
  })

  # postprocess arrow lengths
  garrows <- garrows %>%
    filter(rlang::eval_tidy(filter, data = .)) %>%
    mutate(
      norm = max_arrow_length / max(length),
      length = length * norm,
      x_difference = x_difference * norm,
      y_difference = y_difference * norm,
      x_future = x + x_difference,
      y_future = y + y_difference,
    )

  garrows
}


#' @export
stat_velocity_cells <- dynutils::inherit_default_params(
  list(embed_arrows_cells),
  function(...) {
    list(
      data = function(data) {
        embed_arrows_cells(attr(data, "data")$cell_info)
      }
    )
  }
)
formals(stat_velocity_cells) <- formals(embed_arrows_cells)[2:length(formals(embed_arrows_cells))]

#' @export
stat_velocity_grid <- dynutils::inherit_default_params(
  list(embed_arrows_grid),
  function(...) {
    list(
      data = function(data) {
        embed_arrows_grid(
          attr(data, "data")$cell_info,
          grid_n = grid_n,
          grid_bandwidth = grid_bandwidth,
          max_arrow_length = max_arrow_length,
          filter = filter
        )
      }
    )
  }
)
formals(stat_velocity_grid) <- formals(embed_arrows_grid)[2:length(formals(embed_arrows_grid))]

#' @export
stat_velocity_random <- dynutils::inherit_default_params(
  list(embed_arrows_random),
  function(...) {
    list(
      data = function(data) {
        embed_arrows_random(attr(data, "data")$cell_info, n_cells = n_cells)
      }
    )
  }
)
formals(stat_velocity_random) <- formals(embed_arrows_random)[2:length(formals(embed_arrows_random))]
