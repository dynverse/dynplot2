GeomExpressionTile <- ggproto(
  "GeomExpressionTile",
  GeomTile,
  default_aes = aesIntersect(GeomTile$default_aes, aes(color = "grey80"))
)

#' Plotting expression
#'
#' @param mapping Set of aesthetic mappings created by aes().
#' @param data A function created by [construct_get_cell_info()].
#' @param show.legend Whether to show a legend for this geom
#'
#' @rdname geom_cell
#'
#' @export
geom_expression_tile <- function(
  mapping = NULL,
  ...,
  show.legend = NA,
  rescale = dynutils::scale_quantile,
  data = construct_get_expression_info(rescale = rescale)
) {
  assign("mapping", mapping, envir = environment(data)) # place the mapping in the data environment

  mapping <- aesIntersect(mapping, aes_(x=~x, y=~y, fill=~expression))
  layer(
    data = data,
    mapping = mapping,
    stat = StatIdentity,
    geom = GeomExpressionTile,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = FALSE,
    params = list(
      na.rm = FALSE,
      ...
    )
  )
}

construct_get_expression_info <- function(rescale) {
  function(data) {
    feature_info <- attr(data, "data")$feature_info
    cell_info <- attr(data, "data")$cell_info

    # fetch data
    expression <- get_expression(attr(data, "data")$dataset)

    # ensure exact subset and ordering
    expression <- expression[cell_info$cell_id, feature_info$feature_id, drop = FALSE]

    # rescale expression
    expression <- rescale(expression)

    # reshape into data frame
    expression_info <- reshape2::melt(
      as.matrix(expression),
      varnames = c("cell_id", "feature_id"),
      value.name = "expression"
    ) %>%
      mutate(
        cell_id = as.character(.data$cell_id),
        feature_id = as.character(.data$feature_id)
      )

    # add info data frames
    expression_info %>%
      left_join(feature_info, "feature_id") %>%
      left_join(cell_info %>% select(-.data$y), "cell_id")
  }
}
