GeomFeatureText <- ggproto(
  "GeomFeatureText",
  GeomText,
  default_aes = aesIntersect(aes(hjust = 1), GeomText$default_aes)
)

#' Plotting feature text
#'
#' @param mapping Set of aesthetic mappings created by aes().
#' @param data A function created by [get_feature_info_constructor()].
#' @param show.legend Whether to show a legend for this geom
#'
#' @rdname geom_feature
#'
#' @export
geom_feature_text <- function(
  mapping = NULL,
  data = construct_get_feature_info(),
  ...,
  show.legend = TRUE
) {
  mapping <- aesIntersect(mapping, aes_(x=~0, y=~y, label=~feature_id))
  layer(
    data = data,
    mapping = mapping,
    stat = StatIdentity,
    geom = GeomFeatureText,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = FALSE,
    params = lst(
      na.rm = FALSE,
      ...
    )
  )
}



construct_get_feature_info <- function() {
  function(data) {
    attr(data, "data")$feature_info
  }
}

