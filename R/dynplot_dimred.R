#' Plot a trajectory based on a dimensionality reduction
#'
#' @inheritParams dynplot
#' @param dimred A dimensionality reduction matrix of the cells. Default is `dataset$dimred`.
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
#' dynplot_dimred(dataset) +
#'   geom_trajectory_segments() +
#'   geom_cell_point(size = 2, colour = "black") +
#'   geom_cell_point(aes(colour = milestone_percentages), size = 1.8) +
#'   geom_milestone_label(aes(fill = milestone_id)) +
#'   scale_milestones_color() +
#'   scale_milestones_fill()
#'
#' dynplot_dimred(dataset) +
#'   geom_cell_point(aes(colour = select_feature_expression("G1", d = .data))) +
#'   scale_expression_color() +
#'   new_scale_color() +
#'   geom_trajectory_segments(aes(colour = milestone_percentages), size = 2) +
#'   geom_milestone_label(aes(fill = milestone_id)) +
#'   scale_milestones_color() +
#'   scale_milestones_fill()
dynplot_dimred <- function(dataset, trajectory = dataset, dimred = dataset$dimred) {
  layout <- list()

  if (is.null(dimred)) {
    message("No dimred specified, calculating it")
    dimred <- dyndimred::dimred_landmark_mds(dynwrap::get_expression(dataset), ndim = 2, distance_method = "spearman")
  }

  # if the dimred originated from the trajectory AND this trajectory already contains a trajectory dimred, we don't calculate the projection. In all other cases we do
  trajectory_dimred <- trajectory$dimred # store this in a variable for pryr::address

  recalculate_traj_dimred <- !(
    dynwrap::is_wrapper_with_dimred(trajectory)
    &&
      !is.null(trajectory_dimred)
    &&
      identical(trajectory_dimred[1, ], dimred[1, ], )
    &&
      all(c("dimred_segment_points", "dimred_segment_progressions") %in% names(trajectory))
  )

  if (!is.null(dataset$dimred_future)) {
    # check whether the dimred_future has to be recalculated, based on the dimred_digest attribute
    if(!is.null(attr(dataset$dimred_future, "dimred_digest")) && attr(dataset$dimred_future, "dimred_digest") == digest::digest(dimred, "md5")) {
      dimred_future <- dataset$dimred_future
    } else {
      message("Embedding velocity in dimensionality reduction")
      dimred_future <- scvelo::embed_velocity(dataset, dimred)
    }
    dimred <- cbind(
      dimred,
      dimred_future %>% {set_colnames(., paste0(colnames(.), "_future"))}
    )
  }
  cell_positions <- dimred %>%
    rename_dimred_xy() %>%
    as.data.frame() %>%
    rownames_to_column("cell_id")

  assert_that(cell_positions$cell_id %all_in% dataset$cell_ids)
  layout$cell_positions <- cell_positions

  # trajectory --------------------------------------------------------------
  if (dynwrap::is_wrapper_with_trajectory(trajectory)) {
    # trajectory dimred
    if (!recalculate_traj_dimred) {
      traj_dimred <- trajectory
    } else {
      message("Projecting trajectory onto dimensionality reduction")
      traj_dimred <- trajectory %>% dynwrap::project_trajectory(dimred)
    }

    # milestone positions
    milestone_positions <- as.data.frame(traj_dimred$dimred_milestones[trajectory$milestone_ids, , drop = FALSE]) %>%
      rename_dimred_xy() %>%
      as.data.frame() %>%
      rownames_to_column("milestone_id")

    edge_positions <- trajectory$edge_positions
    segment_positions <- trajectory$edge_positions
    segment_progressions <- trajectory$edge_positions

    # trajectory edge positions
    edge_positions <- trajectory$milestone_network %>%
      select(from, to) %>%
      left_join(milestone_positions %>% rename_all(~paste0(., "_from")), c("from" = "milestone_id_from")) %>%
      left_join(milestone_positions %>% rename_all(~paste0(., "_to")), c("to" = "milestone_id_to"))

    # trajectory segment positions
    segment_positions <- traj_dimred$dimred_segment_points %>%
      rename_dimred_xy() %>%
      as.data.frame() %>%
      rownames_to_column("point_id")

    segment_progressions <- traj_dimred$dimred_segment_progressions %>%
      mutate(point_id = segment_positions$point_id)

    # add to layout
    layout <- c(layout, lst(
      milestone_positions,
      edge_positions,
      segment_positions,
      segment_progressions
    ))
  }

  dynplot(
    dataset = dataset,
    trajectory = trajectory,
    layout = layout
  )
}
