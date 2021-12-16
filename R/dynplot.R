#' Create a dynplot with a specified layout
#'
#' It's recommended you use one of the layout-specific dynplot functions such as
#' [dynplot_dendro()], [dynplot_dimred()], [dynplot_graph()] or [dynplot_onedim()]
#' to start creating a new dynplot visualisation. Only use this function if you
#' know how dynplot layouts work.
#'
#' @param dataset A dynwrap dataset object, typically containing a trajectory.
#' @param trajectory The trajectory dynwrap object, if available.
#' @param layout A layout list object. Can contain data frames `cell_positions`, `milestone_positions`, `edge_positions`, `segment_positions`, `segment_progressions`.
#'
#' @return A ggplot2 object, with the processed data in `plot$data` and `attr(plot$data, "data")`.
#'
#' @seealso [dynplot_dendro()], [dynplot_dimred()], [dynplot_graph()] or [dynplot_onedim()]
#'
#' @examples
#' library(ggplot2)
#' dynplot_dimred(example_bifurcating) +
#'   geom_cell_point(aes(colour = select_feature_expression("G1", d = .data))) +
#'   scale_expression_colour() +
#'   new_scale_colour() +
#'   geom_trajectory_segments(aes(colour = edge_id))
#'
#' @export
dynplot <- function(
  dataset,
  trajectory,
  layout
) {
  data <- list(dataset = dataset)

  # cell info ---------------------------------------------------------------
  cell_info <-
    bind_cols(
      dataset$cell_info %||% tibble(cell_id = dataset$cell_ids)
    ) %>%
    left_join(layout$cell_positions, "cell_id")

  # add trajectory cell info
  if (!is.null(trajectory$cell_info)) {
    cell_info <- left_join(
      cell_info,
      trajectory$cell_info[c("cell_id", setdiff(colnames(trajectory$cell_info), colnames(cell_info)))],
      "cell_id"
    )
  }

  # add pseudotime info if available
  if (!is.null(trajectory$pseudotime)) {
    cell_info <- left_join(
      cell_info,
      enframe(trajectory$pseudotime, "cell_id", "pseudotime"),
      "cell_id"
    )
  }

  # grouping ----------------------------------------------------------------
  if ("grouping" %in% names(dataset)) {
    cell_info <- left_join(
      cell_info,
      dataset$grouping %>% enframe("cell_id", "group_id"),
      "cell_id"
    )
  }

  # trajectory --------------------------------------------------------------
  if (dynwrap::is_wrapper_with_trajectory(trajectory)) {
    milestone_id_levels <- trajectory$milestone_ids

    # add milestone percentages to cell info
    cell_info_milestone_percentages <- trajectory$milestone_percentages %>%
      mutate(milestone_id = factor(milestone_id, milestone_id_levels)) %>%
      nest(milestone_percentages = c(milestone_id, percentage)) %>%
      deframe()
    cell_info$milestone_percentages <- unname(cell_info_milestone_percentages[cell_info$cell_id])

    # milestone info
    milestone_info <- tibble(
      milestone_id = trajectory$milestone_ids
    ) %>%
      mutate(
        labelling = trajectory$milestone_labelling[milestone_id] %||% NA_character_,
        label = case_when(is.na(labelling) ~ milestone_id, TRUE ~ labelling)
      ) %>%
      left_join(layout$milestone_positions, "milestone_id") %>%
      mutate(milestone_id = factor(milestone_id, milestone_id_levels))

    # milestone network
    edge_info <-
      bind_cols(
        trajectory$milestone_network
      ) %>%
      mutate(edge_id = paste0(from, "->", to), label = edge_id) %>%
      left_join(layout$edge_positions, c("from", "to"))

    data <- c(data, lst(
      milestone_info,
      edge_info
    ))

    if ("segment_progressions" %in% names(layout)) {
      # segment info (produced by layout)
      segment_info <- layout$segment_progressions %>%
        mutate(edge_id = paste0(from, "->", to)) %>%
        arrange(edge_id, percentage) %>%
        left_join(layout$segment_positions, "point_id") %>%
        left_join(trajectory$milestone_network %>% select(-length), c("from", "to"))

      # get milestone percentages of segments from progressions
      segment_milestone_percentages <- convert_progressions_to_milestone_percentages(
        cell_ids = trajectory$cell_ids,
        milestone_ids = trajectory$milestone_ids,
        milestone_network = trajectory$milestone_network,
        progressions = segment_info %>% mutate(cell_id = point_id)
      ) %>%
        mutate(
          point_id = cell_id,
          milestone_id = factor(milestone_id, milestone_id_levels)
        ) %>%
        select(-cell_id) %>%
        nest(milestone_percentages = c(milestone_id, percentage)) %>%
        deframe()
      segment_info$milestone_percentages <- segment_milestone_percentages[segment_info$point_id]

      data$segment_info <- segment_info
    }

    if (all(c("divergence_edge_positions", "divergence_polygon_positions") %in% names(layout))) {
      data$divergence_edge_info <- layout$divergence_edge_positions
      data$divergence_polygon_info <- layout$divergence_polygon_positions
    }

    if ("connection_positions" %in% names(layout)) {
      data$connection_info <- layout$connection_positions
    }
  }


  # features ----------------------------------------------------------------
  if ("feature_positions" %in% names(layout)) {
    data$feature_info <- left_join(
      layout$feature_positions,
      dataset$feature_info,
      "feature_id"
    )
  } else {
    data$feature_info <- dataset$feature_info
  }


  # finalise ----------------------------------------------------------------
  data$cell_info <- cell_info
  attr(cell_info, "data") <- data


  # plot --------------------------------------------------------------------
  p <- ggplot(data = cell_info) +
    theme_dynplot()
  class(p) <- c("dynplot", class(p))
  p
}

aesIntersect <- function(aes1, aes2) {
  structure(
    c(as.list(aes1), aes2[!names(aes2) %in% names(aes1)]),
    class = 'uneval'
  )
}



#' @export
ggplot_build.dynplot <- function(plot) {
  # do some checks for aesthetics

  # check that milestone_percentages mappings have an associated aesthethic
  milestone_percentage_aesthetics <- plot$layers %>%
    map("mapping") %>%
    purrr::flatten() %>%
    map_chr(rlang::quo_text) %>%
    keep(`==`, "milestone_percentages") %>%
    names()

  milestone_percentage_aesthetics_covered <- plot$scales$scales %>%
    keep(~any(grepl("^ScaleMilestone", class(.)))) %>%
    map(~.$aesthetics) %>%
    unlist()

  assert_that(
    all(milestone_percentage_aesthetics %in% milestone_percentage_aesthetics_covered),
    msg = "Some aesthetics that are mapped to milestone_percentages do not have an associated scale. Use the scale_milestone_* scales"
  )

  NextMethod()
}
