rename_dimred_xy <- function(df) {
  colnames(df) <- gsub("^comp_1", "x", colnames(df))
  colnames(df) <- gsub("^comp_2", "y", colnames(df))
  df
}

# partitions edges into smaller segments
calculate_segments_from_edges <- function(edge_positions, n_segments_per_edge = 100) {
  n_segments_per_edge <- 100
  segments <- pmap(edge_positions, function(from, to, comp_1_from, comp_2_from, comp_1_to, comp_2_to, ...) {
    segment_progressions <- tibble(
      from = from,
      to = to,
      percentage = seq(0, 1, length.out = n_segments_per_edge),
      point_id = paste0(from, "_", to, "_", seq_len(n_segments_per_edge))
    )
    segment_positions <- tibble(
      comp_1 = seq(comp_1_from, comp_1_to, length.out = n_segments_per_edge),
      comp_2 = seq(comp_2_from, comp_2_to, length.out = n_segments_per_edge),
      point_id = paste0(from, "_", to, "_", seq_len(n_segments_per_edge))
    )
    lst(segment_progressions, segment_positions)
  })

  lst(
    segment_progressions = map_dfr(segments, "segment_progressions"),
    segment_positions = map_dfr(segments, "segment_positions")
  )
}




#' @export
#' @keywords layout
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
    all(c("dimred_edge_positions", "dimred_segment_positions", "dimred_segment_progressions") %in% names(trajectory))
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


#' @export
#' @keywords layout
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

#' @export
#' @keywords layout
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
