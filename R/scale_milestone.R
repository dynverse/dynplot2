ScaleMilestoneFillColour <- ggproto(
  "ScaleMilestoneFillColour",
  ggplot2::ScaleDiscrete,
  aesthetics = c("colour", "fill"),
  train_df = function(self, df) {
    # get all milestone id levels
    aesthetics <- intersect(self$aesthetics, names(df))
    milestone_ids <- self$milestone_ids
    for (aesthetic in aesthetics) {
      x <- df[[aesthetic]]
      if (is.list(x)) {
        assert_that(is.data.frame(x[[1]]))
        assert_that(is.factor(x[[1]]$milestone_id))
        new_milestone_ids <- map(x, "milestone_id") %>% map(levels) %>% flatten_chr() %>% unique()
      } else if (is.factor(x)) {
        new_milestone_ids <- levels(x)
      } else {
        stop("Invalid milestone colouring")
      }

      milestone_ids <- unique(c(milestone_ids, new_milestone_ids))
    }

    self$milestone_ids <- milestone_ids
    self$milestone_colors <- define_milestone_colors(self$milestone_colors, self$milestone_ids)
  },
  train = function(self, x) {
    print("train")
    self$range <- scales::train_discrete(factor(c(1, 2, 3)))
  },
  map = function(self, x) {
    # we will always work with a dataframe containing milestone_id and percentage
    # if a character is given, convert it to this representation
    if (is.factor(x)) {
      x <- map(x, function(milestone_id) {
        tibble(milestone_id = milestone_id, percentage = 1)
      })
    }

    if (!is.list(x) || !is.data.frame(x[[1]])) {
      stop("Can't color this to milestones")
    }

    y <- map_chr(x, color_milestone_percentages, milestone_colors = self$milestone_colors)

    y
  },
  get_breaks = function(self) {
    milestone_percentage_breaks(self$milestone_ids)
  },
  get_labels = function(self, breaks) {
    self$milestone_ids
  },
  milestone_ids = character(),
  values = NULL
)


ScaleMilestoneFill <- ggproto(
  "ScaleMilestoneFill",
  ScaleMilestoneFillColour,
  aesthetics = c("fill")
)

ScaleMilestoneColor <- ggproto(
  "ScaleMilestoneColor",
  ScaleMilestoneFillColour,
  aesthetics = c("colour")
)

#' Milestone scales
#' @name scale_milestones

#' @rdname scale_milestones
#' @export
scale_milestones_fill <- function(name = "Milestone", milestone_colors = NULL) {
  ggproto(NULL, ScaleMilestoneFill, name = name, milestone_colors = milestone_colors)
}

#' @rdname scale_milestones
#' @export
scale_milestones_color <- function(name = "Milestone", milestone_colors = NULL) {
  ggproto(NULL, ScaleMilestoneColor, name = name, milestone_colors = milestone_colors)
}

#' @rdname scale_milestones
#' @export
scale_milestones_colour <- scale_milestones_color


#' Helper functions for coloring milestones
#' @rdname helpers_milestone_coloring
#' @export
define_milestone_colors <- function(milestone_colors, milestone_ids) {
  if (length(milestone_ids) > 0 && is.null(milestone_colors)) {
    milestone_colors <- milestone_palette(length(milestone_ids)) %>%
      set_names(milestone_ids) %>%
      col2rgb() %>%
      t()
  } else if(length(milestone_ids) > 0 && is.character(milestone_colors)) {
    # convert from color to rgb matrix
    milestone_colors <- milestone_colors %>%
      col2rgb() %>%
      t()
  }

  milestone_colors
}

#' @rdname helpers_milestone_coloring
#' @param milestone_percentages A tibble of milestone percentages of a particular cell
#' @param milestone_colors The matrix linking milestones to RGB, as created by define_milestone_colors
#' @export
color_milestone_percentages <- function(milestone_percentages, milestone_colors) {
  assert_that(!is.null(milestone_colors))

  mix_colors <- function(milid, milpct) {
    color_rgb <- apply(milestone_colors[milid,,drop = FALSE], 2, function(x) sum(x * milpct))
    color_rgb[color_rgb < 0] <- 0
    color_rgb[color_rgb > 256] <- 256
    do.call(rgb, as.list(c(color_rgb, maxColorValue = 256)))
  }

  mix_colors(as.character(milestone_percentages$milestone_id), milestone_percentages$percentage)
}

#' @rdname helpers_milestone_coloring
#' @export
milestone_percentage_breaks <- function(milestone_ids) {
  map(milestone_ids, function(milestone_id) {
    tibble(milestone_id = milestone_id, percentage = 1)
  })
}
