#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices rainbow
#' @importFrom shades hue
milestone_palette_list <- list(
  cubeHelix = function(n) cubeHelix(n = n),
  Set3 = function(n) {
    cols <- RColorBrewer::brewer.pal(max(3, n), "Set3")[seq_len(n)]
    cols[order(shades::hue(cols))]
  },
  rainbow = function(n) grDevices::rainbow(n = n),
  auto = function(n) {
    if (n <= 12) {
      milestone_palette_list$Set3(n)
    } else {
      # milestone_palette_list$cubeHelix(n)
      all_colors <- grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
      all_colors <- all_colors[order(shades::hue(all_colors))][-c(1:2)] # sort and remove white/black
      all_colors[seq(0, length(all_colors), length(all_colors)/(n+1)) %>% ceiling() %>% head(-1)]
    }
  }
)

#' @param name The name of the palette. Must be one of `"cubeHelix"`, `"Set3"`, or `"rainbow"`. `auto` will choose a palette depending on the n
#' @param n The number of colours to be in the palette.
#'
#' @rdname get_milestone_palette_names
milestone_palette <- function(n, name = "auto") {
  milestone_palette_list[[name]](n)
}

#' Get the names of valid color palettes
#'
#' @keywords plot_helpers
#'
#' @export
get_milestone_palette_names <- function() {
  names(milestone_palette_list)
}
