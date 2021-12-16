#' Plot a trajectory as a graph
#'
#' @inheritParams dynplot
#'
#' @return A ggplot2 object, with the processed data in `plot$data` and `attr(plot$data, "data")`.
#'
#' @seealso [dynplot_dendro()], [dynplot_dimred()], [dynplot_graph()] or [dynplot_onedim()]
#'
#' @export
#' @examples
#' library(ggplot2)
#' data(example_bifurcating)
#' dataset <- example_bifurcating
#'
#' dynplot_onedim(dataset, equal_cell_width = TRUE, margin = 0.05) +
#'   geom_trajectory_segments() +
#'   geom_trajectory_connection() +
#'   geom_cell_point(aes(colour = select_feature_expression("G2", d = .data))) +
#'   scale_expression_colour() +
#'   new_scale_colour() +
#'   geom_milestone_label(aes(y = -0.1, hjust = as.integer(type == "end")))
#'
#' dynplot_onedim(dataset, equal_cell_width = TRUE, margin = 0.05) +
#'   geom_trajectory_segments(aes(color = milestone_percentages), size = 1, color = "#333333") +
#'   geom_trajectory_connection() +
#'   scale_milestones_colour() +
#'   new_scale_colour() +
#'   geom_cell_point(aes(colour = select_feature_expression("G2", d = .data))) +
#'   scale_expression_colour() +
#'   new_scale_colour() +
#'   geom_milestone_label(aes(y = -0.1, fill = milestone_id, hjust = as.integer(type == "end"))) +
#'   scale_milestones_fill()
dynplot_onedim <- function(dataset, trajectory = dataset, margin = 0.02, equal_cell_width = TRUE) {
  assert_that(dynwrap::is_wrapper_with_trajectory(dataset))

  # reorder
  dataset$milestone_network <- optimize_order(dataset$milestone_network)

  # linearise
  linearised <- linearise_trajectory(
    dataset,
    margin = margin,
    equal_cell_width = equal_cell_width
  )

  # calculate positions of connections
  connection_positions <- calculate_connections(linearised) %>% rename_dimred_xy()

  segments <- calculate_segments_from_edges(linearised$edge_positions)

  segment_progressions <- segments$segment_progressions %>% rename_dimred_xy()
  segment_positions <- segments$segment_positions %>% rename_dimred_xy()

  layout <- lst(
    cell_positions = linearised$cell_positions %>% rename_dimred_xy(),
    milestone_positions = linearised$milestone_positions %>% rename_dimred_xy(),
    edge_positions = linearised$edge_positions %>% rename_dimred_xy(),
    segment_progressions,
    segment_positions,
    connection_positions
  )

  dynplot(
    dataset = dataset,
    trajectory = trajectory,
    layout = layout
  )
}
