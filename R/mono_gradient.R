#' Gradient palette (provisional, need to adjust L channel and correct
#' the function extend_palette_equal_ciede2000 for fixed extremes)
#'
#' @param n, Integer, number of output colors
#' @param ref Character, hex color code of the base color. Defaults to 'PANTONE 7461 C'.
#' @param from Character, hex color code of the darkest color. Defaults to 'black'.
#' @param to Character, hex color code of the brightest color. Defaults to 'white'.
#' #param pre_rgb_interpolation Logical, whether to interpolate in RGB space prior to equalization.
#'
#' @returns An equalized color gradient monocolor palette in hex format
#' @export
mono_gradient <- function(
    n,
    from = "#000000",
    ref = igepal_hex[2],
    to = "#FFFFFF",
    pre_rgb_interpolation = TRUE) {
  extend_palette_equal_ciede2000(
    c(from, ref, to),
    n,
    fixed_hex = ref,
    mode = "anchor_exact"
  )
}
