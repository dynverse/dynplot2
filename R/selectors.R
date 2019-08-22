#' @export
select_feature_expression <- function(feature_id, expression_source = "expression") {
  # feature_id <- enquo(feature_id)

  # make sure only one feature_id is requested, or the feature_ids are coming from facet_data
  assert_that(length(feature_id) == 1 || length(feature_id) %% nrow(data) == 0)

  assert_that(!is.null(data))
  assert_that(all(feature_id %in% attr(data, "data")$dataset$feature_info$feature_id))

  expression <- get_expression(attr(data, "data")$dataset, expression_source)

  expression[attr(data, "data")$cell_info$cell_id, unique(feature_id)] %>%
    as.matrix() %>%
    as.numeric()
}



#' @export
select_feature_velocity <- function(feature_id) {
  feature_expression <- select_feature_expression(feature_id, "expression")
  feature_expression_projected <- select_feature_expression(feature_id, "expression_projected")

  feature_expression_projected - feature_expression
}
