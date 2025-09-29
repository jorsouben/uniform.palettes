#' Gradient palette (provisional, need to adjust L channel and correct
#' the function extend_palette_equal_ciede2000 for fixed extremes)
#'
#' @param n, Integer, number of output colors
#' @param from Character, hex color code of the initial color.
#' @param to Character, hex color code of the end color. Defaults to 'white'.
#'
#' @returns An equalized color gradient palette in hex format
#' @export
mono_gradient <- function(n, from = igepal_hex[2], to = "FFFFFF") {
  extend_palette_equal_ciede2000(
    c(from, to),
    n,
    fixed_hex = from,
    mode = "global" # "anchor_exact"
  )
}
