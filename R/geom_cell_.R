GeomCellPoint <- ggproto(
  "GeomCellPoint",
  GeomPoint,
  default_aes = aesIntersect(GeomPoint$default_aes, aes(color = "grey80"))
)

GeomHexPoint <- ggproto(
  "GeomHexPoint",
  GeomHex,
  default_aes = aesIntersect(GeomHex$default_aes, aes(color = "grey80"))
)

#' Plotting cells
#'
#' @param mapping Set of aesthetic mappings created by aes().
#' @param data A function created by [construct_get_cell_info()].
#' @param show.legend Whether to show a legend for this geom
#'
#' @rdname geom_cell
#'
#' @export
geom_cell_point <- function(
    mapping = NULL,
    data = construct_get_cell_info(),
    ...,
    show.legend = NA
  ) {
  assign("mapping", mapping, envir = environment(data)) # place the mapping in the data environment

  mapping <- aesIntersect(mapping, aes_(x=~x, y=~y))
  layer(
    data = data,
    mapping = mapping,
    stat = StatIdentity,
    geom = GeomCellPoint,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = FALSE,
    params = list(
      na.rm = FALSE,
      ...
    )
  )
}


StatCellHex <- ggproto(
  "StatCellHex",
  Stat,
  compute_group = function(
    data,
    scales,
    binwidth = NULL,
    bins = 30,
    na.rm = FALSE
  ) {
    ggplot2:::try_require("hexbin", "stat_binhex")

    # parts of this code were taken from ggplot2 stat_binhex

    binwidth <- binwidth %||% ggplot2:::hex_binwidth(bins, scales)

    # calculate bounds and bins
    if (length(binwidth) == 1) {
      binwidth <- rep(binwidth, 2)
    }
    xbnds <- ggplot2:::hex_bounds(data$x, binwidth[1])
    xbins <- diff(xbnds)/binwidth[1]
    ybnds <- ggplot2:::hex_bounds(data$y, binwidth[2])
    ybins <- diff(ybnds)/binwidth[2]
    hb <- hexbin::hexbin(data$x, xbnds = xbnds, xbins = xbins, data$y,
                         ybnds = ybnds, shape = ybins/xbins, IDs = TRUE)

    out_coords <- bind_cols(hexbin::hcell2xy(hb)) %>%
      mutate(hexagon_id = hb@cell)

    # get count and density
    out_count <- data %>%
      mutate(hexagon_id = hb@cID) %>%
      group_by(hexagon_id) %>%
      summarise(
        count = n()
      ) %>%
      mutate(
        density = count / max(count)
      )

    # summarise all numeric variables to first value
    out_data <- data %>%
      select(-x, -y, -group, -PANEL) %>%
      mutate(hexagon_id = hb@cID) %>%
      group_by(hexagon_id) %>%
      slice(1) %>%
      ungroup()

    out <- left_join(out_coords, out_data, "hexagon_id") %>%
      left_join(out_count, "hexagon_id")

    out
  }
)


#' @rdname geom_cell
#' @export
geom_cell_hex <- function(
  mapping = NULL,
  data = construct_get_cell_info(),
  bins = 100,
  ...,
  show.legend = NA
) {
  mapping <- aesIntersect(mapping, aes_(x=~x, y=~y))

  layer(
    data = data,
    mapping = mapping,
    stat = StatCellHex,
    geom = GeomHexPoint,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = FALSE,
    params = list(
      na.rm = FALSE,
      bins = bins,
      ...
    )
  )
}

construct_get_cell_info <- function() {
  function(data) {
    # first parse the mapping to know what to put inside the cell info
    out <- attr(data, "data")$cell_info

    # create expression, counts and expression_future
    data_env <- new.env(parent = emptyenv())
    assign("data", data, data_env)

    d <- list(data_env)
    out$d <- d
    out
  }
}
