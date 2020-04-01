#' @export
#' @importFrom stringr str_glue
select_feature_expression <- function(feature_id, d, expression_source = "expression", scale = dynutils::scale_quantile) {
  assert_that(
    !missing(d) && class(d) == "rlang_data_pronoun",
    msg = str_glue("The second argument of select_feature_expression should be {crayon::italic('.data')} or {crayon::italic('d = .data')}")
  )

  # to get the correct expression, we need two additional data objects:
  # - the dataset from which we can get the expression
  # - the order of the cell ids (which may be duplicated, i.e. in the case of facetting)
  data <- get("data", envir = d$d[[1]])
  cell_ids <- d$cell_id

  # make sure only one feature_id is requested, or the feature_ids are coming from facet_data
  assert_that(length(feature_id) == 1 || (length(feature_id) %% nrow(data)) == 0)

  assert_that(!is.null(data))
  assert_that(all(feature_id %in% attr(data, "data")$dataset$feature_info$feature_id))

  expression <- get_expression(attr(data, "data")$dataset, expression_source)[, unique(feature_id), drop=F]

  # scale expression
  assert_that(is.function(scale))
  expression <- scale(expression)

  if (length(feature_id) == 1) {
    feature_id <- rep(feature_id, length(cell_ids))
  }

  expression_molten <- expression %>%
    as.matrix() %>%
    reshape2::melt(varnames = c("cell_id", "feature_id"), value.name = "expression") %>%
    mutate(cell_id = as.character(cell_id), feature_id = as.character(feature_id))

  # make sure feature_id is a multiple of cell_ids
  assert_that((length(feature_id) %% length(cell_ids)) == 0)

  # do the left join to get the required expression
  expression_df <- tibble(cell_id = rep(cell_ids, length(feature_id)/length(cell_ids)), feature_id = feature_id) %>%
    left_join(expression_molten, c("cell_id", "feature_id"))

  # create output with attributes
  out <- expression_df$expression
  out
}

#' @importFrom stringr str_glue
#' @export
select_feature_velocity <- function(feature_id, d) {
  assert_that(
    !missing(d) && class(d) == "rlang_data_pronoun",
    msg = str_glue("The second argument of select_feature_velocity should be {crayon::italic('.data')} or {crayon::italic('d = .data')}")
  )

  feature_expression <- select_feature_expression(feature_id, d, "expression")
  feature_expression_future <- select_feature_expression(feature_id, d, "expression_future")

  feature_expression_future - feature_expression
}
