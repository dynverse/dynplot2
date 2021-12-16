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
