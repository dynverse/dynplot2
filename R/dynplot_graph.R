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
#' data(example_bifurcating)
#' dataset <- example_bifurcating
#'
#' dynplot_graph(dataset) +
#'   geom_trajectory_divergence() +
#'   geom_trajectory_segments(size = 2, color = "#333333", arrow_size = 0.5) +
#'   geom_cell_point(colour = "black", size = 3) +
#'   geom_cell_point(aes(colour = milestone_percentages), size = 2.8) +
#'   geom_milestone_label(aes(fill = milestone_id)) +
#'   scale_milestones_fill() +
#'   scale_milestones_color()
dynplot_graph <- function(dataset, trajectory = dataset) {
  assert_that(dynwrap::is_wrapper_with_trajectory(dataset))
  trajectory_dimred <- dynwrap::calculate_trajectory_dimred(dataset)

  segments <- calculate_segments_from_edges(trajectory_dimred$edge_positions)

  segment_progressions <- segments$segment_progressions %>% rename_dimred_xy()
  segment_positions <- segments$segment_positions %>% rename_dimred_xy()

  layout <- lst(
    cell_positions = trajectory_dimred$cell_positions %>% rename_dimred_xy(),
    milestone_positions = trajectory_dimred$milestone_positions %>% rename_dimred_xy(),
    edge_positions = trajectory_dimred$edge_positions %>% rename_dimred_xy(),
    segment_progressions,
    segment_positions,
    divergence_edge_positions = trajectory_dimred$divergence_edge_positions %>% rename_dimred_xy(),
    divergence_polygon_positions = trajectory_dimred$divergence_polygon_positions %>% rename_dimred_xy()
  )

  dynplot(
    dataset = dataset,
    trajectory = trajectory,
    layout = layout
  )
}
