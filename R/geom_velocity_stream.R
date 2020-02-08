GeomVelocityStream <- ggproto(
  "GeomVelocityStream",
  GeomPath,
  default_aes = aesIntersect(GeomPath$default_aes, aes(color = "black", linejoin = "mitre", lineend = "butt", length = 1)),
  draw_panel = function(self, data, panel_params, coord, arrow = NULL, arrow_size = 1, shadow = shadow_defaults(),  ...) {
    original_draw_panel <- GeomPath$draw_panel

    # draw path ---------
    grob_path <- original_draw_panel(data = data, panel_params = panel_params, coord = coord, arrow = NULL, lineend = data$lineend[[1]], linejoin = data$linejoin[[1]], ...)

    # draw arrows -------
    # select the rows at draw_arrow and draw_arrow - 1
    data_arrows <- data %>%
      group_by(group) %>%
      filter(row_number() == floor(n()/2) | lead(row_number()) == floor(n()/2)) %>%
      mutate(size = size * arrow_size)

    if (nrow(data_arrows) > 1) {
      grob_arrows <- original_draw_panel(data = data_arrows, panel_params = panel_params, coord = coord, arrow = arrow, lineend = "butt", linejoin = "mitre", ...)
    } else {grob_arrows <- grid::grob()}

    # combine grobs
    grid::gList(
      grob_arrows,
      grob_path
    )
  },

  draw_group = function(data, panel_params, coord, arrow = NULL, arrow_size = 1, shadow = "black") {
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
geom_velocity_stream <- function(
  mapping = NULL,
  stat = stat_velocity_stream(),
  arrow = ggplot2::arrow(length = ggplot2::unit(0.5, "cm"), type = "closed"),
  arrow_size = 1,
  ...,
  data = construct_get_velocity_info(stat),
  show.legend = NA
) {
  mapping <- aesIntersect(mapping, aes_(x=~x, y=~y, group=~line))

  layer(
    data = data,
    mapping = mapping,
    stat = StatIdentity,
    geom = GeomVelocityStream,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = FALSE,
    params = lst(
      arrow,
      arrow_size,
      na.rm = FALSE,
      ...
    )
  )
}

#' @rdname embed_arrows
embed_arrows_stream <- function(
  cell_positions,
  grid_bandwidth = 1/3,
  filter = rlang::quo(mass > max(mass) * 0.1)
) {
  matplotlib <- reticulate::import("matplotlib")

  ax <- matplotlib$pyplot$axes()

  grid_arrows <- embed_arrows_grid(cell_positions, filter = TRUE, grid_n = 20)
  grid_arrows <- grid_arrows %>%
    mutate(
      filter = rlang::eval_tidy(filter, data = .)
    ) %>%
    mutate(
      x_difference = ifelse(filter, x_difference, 0),
      y_difference = ifelse(filter, y_difference, 0)
    )

  streamplot <- matplotlib$axes$mstream$streamplot(
    ax,
    reticulate::np_array(unique(grid_arrows$x)),
    reticulate::np_array(unique(grid_arrows$y)),
    reticulate::np_array(matrix(grid_arrows$x_difference, nrow = length(unique(grid_arrows$x)), byrow = F)),
    reticulate::np_array(matrix(grid_arrows$y_difference, nrow = length(unique(grid_arrows$x)), byrow = F))
  )
  vertices <- streamplot$lines$get_segments()

  streamplot_data <- vertices %>% map_dfr(function(matrix) {
    tibble(
      x = matrix[1],
      y = matrix[3],
      x_future = matrix[2],
      y_future = matrix[4]
    )
  })

  # group into lines and remove duplicate coordinates
  streamplot_data <- streamplot_data %>%
    mutate(diff = (lead(x) - x_future) + (lead(y) - y_future)) %>%
    mutate(line = lag(cumsum(diff != 0), default = 0)) %>%
    mutate(order = row_number()) %>%
    group_by(x, y) %>% # remove duplicate x and y coordinates
    slice(1) %>%
    ungroup() %>%
    arrange(order)

  # calculate other statistics for each line
  streamplot_data <- streamplot_data %>%
    arrange(line) %>%
    group_by(line) %>%
    mutate(
      percentage = row_number() / n(), step = row_number(),

    )

  # streamplot_data <- streamplot_data %>% filter(line == 25)

  streamplot_data
}

#' @export
stat_velocity_stream <- dynutils::inherit_default_params(
  list(embed_arrows_stream),
  function(...) {
    list(
      data = function(data) {
        embed_arrows_stream(
          attr(data, "data")$cell_info,
          grid_bandwidth = grid_bandwidth,
          filter = filter
        )
      }
    )
  }
)
formals(stat_velocity_stream) <- formals(embed_arrows_stream)[2:length(formals(embed_arrows_stream))]
