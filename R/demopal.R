#' CIEDE2000 equalized demo hex palette
#'
#' @param n number of output colors.
#' @param init_pal Character vector of initial hex color values to interpolate
#'  and equalize. Defaults to `igepal_hex`.
#' @param fixed_index Integer or NULL. Index of a color to keep unaltered.
#'  Defaults to `2`.
#' @param fixed_hex Character or NULL. Hex color value to keep unaltered.
#'  Must be present in `init_pal`. Defaults to `NULL`.
#'
#' @return Character length-`n` vector of hex color values.
#' @export
demopal <- function(n, init_pal = igepal_hex, fixed_index = 2, fixed_hex = NULL) {
  # --- Ensure only one of fixed_index or fixed_hex is provided ---
  if (!is.null(fixed_index) && !is.null(fixed_hex)) {
    stop("Please provide only one of `fixed_index` or `fixed_hex`, not both.")
  }

  # --- Handle fixed_hex ---
  if (!is.null(fixed_hex)) {
    if (!fixed_hex %in% init_pal) {
      warning("`fixed_hex` not found in `init_pal`. Ignoring and using NULL.")
      fixed_hex <- NULL
    }
  }

  # --- Handle fixed_index ---
  if (!is.null(fixed_index)) {
    if (!is.numeric(fixed_index) || length(fixed_index) != 1) {
      stop("`fixed_index` must be a single integer or NULL.")
    }
    if (fixed_index < 1 || fixed_index > length(init_pal)) {
      stop("`fixed_index` is out of range for `init_pal`.")
    }
    fixed_hex <- init_pal[fixed_index]
  }

  # --- Call the palette extension function ---
  extend_palette_equal_ciede2000(
    init_pal,
    n,
    fixed_hex = fixed_hex,
    mode = "anchor_exact"
  )$hex
}

#' CIEDE2000 equalized demo hex palette
#'
#' @param n number of output colors.
#' @param init_pal Character vector of initial hex color values to interpolate
#'  and equalize. Defaults to `igepal_hex`.
#' @param fixed_index Integer or NULL. Index of a color to keep unaltered.
#'  Defaults to `2`.
#' @param fixed_hex Character or NULL. Hex color value to keep unaltered.
#'  Must be present in `init_pal`. Defaults to `NULL`.
#'
#' @return Character length-`n` vector of hex color values.
#' @export
demopal <- function(n, init_pal = igepal_hex, fixed_index = 2, fixed_hex = NULL) {
  # --- Ensure only one of fixed_index or fixed_hex is provided ---
  if (!is.null(fixed_index) && !is.null(fixed_hex)) {
    stop("Please provide only one of `fixed_index` or `fixed_hex`, not both.")
  }

  # --- Handle fixed_hex ---
  if (!is.null(fixed_hex)) {
    if (!fixed_hex %in% init_pal) {
      warning("`fixed_hex` not found in `init_pal`. Ignoring and using NULL.")
      fixed_hex <- NULL
    }
  }

  # --- Handle fixed_index ---
  if (!is.null(fixed_index)) {
    if (!is.numeric(fixed_index) || length(fixed_index) != 1) {
      stop("`fixed_index` must be a single integer or NULL.")
    }
    if (fixed_index < 1 || fixed_index > length(init_pal)) {
      stop("`fixed_index` is out of range for `init_pal`.")
    }
    fixed_hex <- init_pal[fixed_index]
  }

  # --- Call the palette extension function ---
  extend_palette_equal_ciede2000(
    init_pal,
    n,
    fixed_hex = fixed_hex,
    mode = "anchor_exact"
  )$hex
}

#' CIEDE2000 equalized demo hex palette, making a previous interpolation
#' in RGB space
#'
#' @param n number of output colors.
#' @param m number of colors for the intermediate interpolated palette.
#' @param init_pal Character vector of initial hex color values to interpolate
#'  and equalize. Defaults to `igepal_hex`.
#' @param fixed_index Integer or NULL. Index of a color to keep unaltered.
#'  Defaults to `2`.
#' @param fixed_hex Character or NULL. Hex color value to keep unaltered.
#'  Must be present in `init_pal`. Defaults to `NULL`.
#'
#' @return Character length-`n` vector of hex color values.
#' @export
demopal2 <- function(n, m = 16, init_pal = igepal_hex, fixed_index = 2, fixed_hex = NULL) {
  # --- Ensure only one of fixed_index or fixed_hex is provided ---
  if (!is.null(fixed_index) && !is.null(fixed_hex)) {
    stop("Please provide only one of `fixed_index` or `fixed_hex`, not both.")
  }

  # --- Handle fixed_hex ---
  if (!is.null(fixed_hex)) {
    if (!fixed_hex %in% init_pal) {
      stop("`fixed_hex` not found in `init_pal`.")
    }
    fixed_index <- which(init_pal == fixed_hex)
  }

  # --- Handle fixed_index ---
  if (!is.null(fixed_index)) {
    if (!is.numeric(fixed_index) || length(fixed_index) != 1) {
      stop("`fixed_index` must be a single integer or NULL.")
    }
    if (fixed_index < 1 || fixed_index > length(init_pal)) {
      stop("`fixed_index` is out of range for `init_pal`.")
    }
  }

  # --- We distribute the number of colors left and right according to
  # the cumulative ciede2000 distance between the fixed color and the
  # extremes ---
  deltas <- init_pal |>
    hex2palette_info() |>
    dplyr::pull(cum_delta_2000)

  left_m <- round(m * deltas[fixed_index] / tail(deltas, 1))
  right_m <- m - left_m

  leftramp <-
    init_pal[1:fixed_index] |>
    colorRampPalette()

  rightramp <-
    init_pal[fixed_index:length(init_pal)] |>
    colorRampPalette()

  interp_pal <-
    c(leftramp(left_m), rightramp(right_m)[-1])

  demopal(n, init_pal = interp_pal, fixed_index = left_m)
}
