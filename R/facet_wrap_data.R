#' Wrap facetting based on data
#'
#' @importFrom stringr str_glue
#'
#' @param ... Data to facet along.
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
#'   geom_text(aes(x, y, color = x > cutoff, label = cutoff)) +
#'   facet_wrap_data(cutoff = c(1, 5, 15))
#'
#' @export
facet_wrap_data <- function(..., nrow = NULL, ncol = 4, labeller = NULL) {
  dots <- list(...)

  # make sure that one dots was provided
  assert_that(length(dots) == 1)

  # prepare the facet data
  facet_name <- names(dots)[[1]]
  facet_data <- tibble(
    !!facet_name := dots[[1]]
  )
  facets <-  vars(!!as.name(facet_name))

  # calculate the layout (number of rows and columns)
  if (is.null(nrow) && is.null(ncol)) {
    ncol <- 4
  }
  if (is.null(nrow)) {
    nrow <- ceiling(nrow(facet_data)/ncol)
  }
  if (is.null(ncol)) {
    ncol <- ceiling(nrow(facet_data)/ncol)
  }

  # generate the layout
  layout <- facet_data %>%
    mutate(PANEL = row_number(), SCALE_X = 1, SCALE_Y = 1, COL = ((PANEL - 1) %% ncol) + 1, ROW = ceiling(PANEL / ncol))

  # generate the labeller
  labeller <- function(x) {
    x %>%
      mutate_at(vars(!!facet_name), ~str_glue("{facet_name} = {.}"))
  }

  ggproto(
    "FacetWrapData",
    facet_wrap(facets = facets, labeller = labeller),
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
