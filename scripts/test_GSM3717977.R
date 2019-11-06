library(dyntoy)
library(tidyverse)
library(dynwrap)

dataset <- read_rds("~/dataset.rds")
# dataset <- dataset %>% add_dimred(dimred = dyndimred::dimred_landmark_mds)

feature_names <- c("Dpp4", "Dlk1", "F3", "Icam1")
feature_mapper <- function(x) {(dataset$feature_info %>% select(gene_name, feature_id) %>% deframe())[x]}
feature_revmapper <- function(x) {(dataset$feature_info %>% select(feature_id, gene_name) %>% deframe())[x]}
feature_name <- feature_names[[1]]

dimred <- dataset %>% add_dimred(dyndimred::dimred_landmark_mds)


trajectory <- infer_trajectory(dataset, dynmethods::ti_gng(ndim = 2), verbose = TRUE)

dynplot_dimred(dataset, trajectory) +
  geom_cell_point(aes(color = select_feature_expression(feature_mapper("Dpp4"), .data))) +
  geom_trajectory_segments() +
  geom_velocity_arrow(stat = stat_velocity_cells()) +
  scale_expression_fillcolour()


devtools::load_all("~/thesis/projects/dynverse/dynwrap")

trajectory2 <- orient_topology_to_velocity(trajectory, dataset$expression, dataset$expression_projected)

dynplot_dimred(dataset, trajectory2, dimred = trajectory2$dimred) +
  # geom_cell_point() +
  geom_cell_hex(aes(fill = ..density..)) +
  scale_fill_gradient2(low = "grey20", high = "grey60") +
  geom_trajectory_segments() +
  geom_velocity_arrow()




feature_ids <- feature_mapper(feature_names)
patchwork::wrap_plots(
  dynplot(dataset) +
    geom_cell_hex(aes(fill = ..density..)) +
    scale_fill_gradient2(low = "grey20", high = "grey60") +
    geom_velocity_arrow(),
  patchwork::wrap_plots(
    dynplot(dataset) +
      geom_cell_hex(aes(fill = select_feature_velocity(feature_id_oi, .data))) +
      facet_wrap_data(feature_id_oi = feature_ids) +
      scale_velocity_fillcolor(),
    dynplot(dataset) +
      geom_cell_hex(aes(fill = select_feature_expression(feature_id_oi, .data))) +
      facet_wrap_data(feature_id_oi = feature_ids, labeller = feature_revmapper) +
      scale_expression_fillcolour(),
    ncol = 1
  ),
  nrow = 1
)

patchwork::wrap_plots(
  dynplot(dataset) +
    geom_cell_point(aes(color = select_feature_expression("G1", .data))) +
    scale_expression_fillcolour() +
    geom_trajectory_segments(),
  dynplot(dataset) +
    geom_cell_point(aes(color = select_feature_expression("G1", .data, expression_source = "expression_projected"))) +
    scale_expression_fillcolour() +
    geom_trajectory_segments(),
  dynplot(dataset) +
    geom_cell_point(aes(color = select_feature_velocity("G1", .data))) +
    scale_velocity_fillcolor() +
    geom_trajectory_segments()
)
