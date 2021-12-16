GeomMilestoneLabel <- ggproto(
  "GeomMilestoneLabel",
  GeomLabel,
  default_aes = aesIntersect(aes(color = "white", fill = "#111111CC", fontface = "bold"), GeomLabel$default_aes)
)

#' Plotting milestones
#'
#' @param mapping Set of aesthetic mappings created by aes().
#' @param data A function created by [construct_get_cell_info()].
#' @param show.legend Whether to show a legend for this geom
#'
#' @rdname geom_milestone
#'
#' @export
geom_milestone_label <- function(
  mapping = NULL,
  data = get_milestone_info,
  ...,
  show.legend = FALSE
) {
  mapping <- aesIntersect(mapping, aes_(x=~x, y=~y, label=~label))
  layer(
    data = data,
    mapping = mapping,
    stat = StatIdentity,
    geom = GeomMilestoneLabel,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = FALSE,
    params = lst(
      na.rm = FALSE,
      ...
    )
  )
}


GeomMilestonePoint <- ggproto(
  "GeomMilestoneLabel",
  GeomPoint,
  draw_panel = function(self, data, panel_params, coord, shadow, ...) {
    original_draw_panel <- GeomPoint$draw_panel

    # draw point ---------
    grob_point <- original_draw_panel(data = data, coord = coord, panel_params = panel_params, ...)

    # draw shadows --------
    assert_that(is.list(shadow) || isFALSE(shadow), msg = "shadow should be a list created by shadow_defaults() or FALSE")
    if (is.list(shadow)) {
      grob_point_shadow <- original_draw_panel(
        data = data %>% mutate(colour = shadow$color, size = size + shadow$size), panel_params = panel_params, coord = coord, ...)
    } else {
      grob_point_shadow <- grid::grob()
    }

    # combine grobs
    grid::gList(
      grob_point_shadow,
      grob_point
    )
  },
  # this function is just here so that shadow becomes a parameter
  draw_group = function(data, panel_params, coord, shadow) {
  }
)

#' @export
geom_milestone_point <- function(
  mapping = NULL,
  data = get_milestone_info,
  position = "identity",
  show.legend = NA,
  size = 10,
  shadow = shadow_defaults(),
  ...
) {
  mapping <- aesIntersect(mapping, aes_(x=~x, y=~y, color=~milestone_id))
  layer(data = data, mapping = mapping, stat = StatIdentity, geom = GeomMilestonePoint,
        position = position, show.legend = show.legend, inherit.aes = FALSE,
        params = lst(na.rm = FALSE, size = size, shadow = shadow, ...)
  )
}


get_milestone_info <- function(data) {
  attr(data, "data")$milestone_info
}
