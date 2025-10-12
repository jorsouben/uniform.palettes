#' Plot comparing diagnostics of 2 palettes
#'
#' @description
#' Create a plots of the deltas, visual error and channels with the colors of the
#' palette using `ggplot2`
#'
#' @param pal1 Vector of hex colour codes, matrix or data.frame or ColorMap
#' @param pal2 Vector of hex colour codes, matrix or data.frame or ColorMap
#' @param pal_names Character, name of the palettes to show
#' @returns NULL
#' @export
#'
compare_diagnostics <- function(pal1, pal2, pal_names = NULL) {
  pal1 <- as_colormap(pal1)
  pal2 <- as_colormap(pal2)

  p1 <- diagnostic_plot(pal1, show_channels = FALSE, pal_name = pal_names[1])
  p2 <- diagnostic_plot(pal2, show_channels = FALSE, pal_name = pal_names[2])

  patchwork::wrap_plots(p1, p2) +
    patchwork::plot_annotation(
      glue::glue(
        "Comparison of `{pal_names[1]}` and `{pal_names[2]}` palettes"
      )
    )
}
