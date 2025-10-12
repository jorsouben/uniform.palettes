#' Plot deltas, visual error and channels
#'
#' @description
#' Create a plots of the deltas, visual error and channels with the colors of the
#' palette using `ggplot2`
#'
#' @param pal Vector of hex colour codes, matrix or data.frame or ColorMap
#' @param show_channels Logical, show channels plot or not
#' @param pal_name Character, name of the palette to show
#' @return NULL
#' @export
#'
diagnostic_plot <- function(pal, pal_name = NULL, show_channels = TRUE) {
  pal <- as_colormap(pal)

  # Deltas plot
  p_deltas <-
    gradient_plot(
      values = pal$deltas(),
      pal = pal$get_hex(),
      title = "ΔE2000",
      max = 2.5,
      rescale = FALSE
    )

  # Cummulative Deltas plot
  p_cum_deltas <-
    gradient_plot(
      values = pal$cum_deltas(),
      pal = pal$get_hex(),
      title = "Cummulative ΔE2000",
      max = NULL,
      rescale = TRUE
    )

  # Visual error plot
  p_visual_error <-
    gradient_plot(
      values = pal$visual_error(),
      pal = pal$get_hex(),
      title = "Visual Error %",
      max = 10,
      rescale = FALSE
    )

  # Channels plot
  p_channels <- pal |>
    plot_channels(space = "lab")

  if (show_channels) {
    p_deltas +
      p_cum_deltas +
      p_channels +
      p_visual_error +
      patchwork::plot_layout(nrow = 2) +
      patchwork::plot_annotation(pal_name)
  } else {
    p_deltas +
      p_cum_deltas +
      p_visual_error +
      patchwork::plot_layout(ncol = 1) +
      patchwork::plot_annotation(pal_name)
  }
}
