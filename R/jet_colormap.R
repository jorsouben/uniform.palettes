#' The `jet` colormap
#'
#' @param n Integer, number of output colors
#'
#' @returns Character vector of hex color codes
#' @export
#'
jet_colors <- function(n) {
  colorRampPalette(
    c(
      "#00007F",
      "blue",
      "#007FFF",
      "cyan",
      "#7FFF7F",
      "yellow",
      "#FF7F00",
      "red",
      "#7F0000"
    )
  )(n)
}
