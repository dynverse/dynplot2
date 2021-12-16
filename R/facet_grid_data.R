#' Grid facetting based on data
#'
#' @param ... Data to facet along.
#' @importFrom stringr str_glue
#'
#' @examples
#' library(ggplot2)
#' cutoff <- function(x, cutoff) {
#'   x > cutoff
#' }
#'
#' d <- data.frame(
#'   x = 1:100,
#'   y = sqrt(1:100)
#' )
#'
#' ggplot(d) +
#'   geom_text(aes(x, y, color = x > cutoff, label = hello)) +
#'   facet_grid_data(hello = c("a", "b", "c"), cutoff = c(30, -100, 10))
#'
#' @export
facet_grid_data <- function(...) {
  dots <- list(...)

  # make sure at least row or columns are provided
  assert_that(length(dots) %in% c(1, 2))

  # generate the layout
  if (names(dots)[[1]] != "") {
    # rows was provided
    row_name <- names(dots)[[1]]
    row_data <- tibble(
      !!row_name := dots[[1]],
      ROW = seq_along(dots[[1]])
    )
    rows <-  vars(!!row_name := !!as.name(row_name))
  } else {
    row_name <- rows <- NULL
    row_data <- tibble(ROW = 1)
  }

  if (length(dots) == 2 && names(dots)[[2]] != "")  {
    # cols was provided
    col_name <- names(dots)[[2]]
    col_data <- tibble(
      !!col_name := dots[[2]],
      COL = seq_along(dots[[2]])
    )
    cols <-  vars(!!col_name := !!as.name(col_name))
  } else {
    col_name <- cols <-NULL
    col_data <- tibble(COL = 1)
  }

  layout <- crossing(col_data, row_data) %>%
    mutate(PANEL = row_number(), SCALE_X = 1, SCALE_Y = 1) %>%
    as.data.frame() # convert to dataframe to avoid warnings when running unique(layout[names(params$cols)])

  # generate the labeller
  labeller <- function(x) {
    if (col_name %in% colnames(x)) {
      x %>%
        mutate_at(vars(!!col_name), ~str_glue("{col_name} = {.}"))
    } else if (row_name %in% colnames(x)) {
      x %>%
        mutate_at(vars(!!row_name), ~str_glue("{row_name} = {.}"))
    }
  }

  ggproto(
    "FacetGridData",
    facet_grid(rows = rows, cols = cols, labeller = labeller),
    compute_layout = function(self, data, params) {
      layout
    },
    map_data = function(self, data, layout, params) {
      results <- pmap(layout, function(PANEL, ROW, COL, SCALE_X, SCALE_Y, ...) {
        variable_values <- list(...)

        for (variable in names(variable_values)) {
          data[[variable]] <- variable_values[[variable]]
        }

        data$PANEL <- PANEL

        data
      })
      result <- bind_rows(results)

      result
    }
  )
}
