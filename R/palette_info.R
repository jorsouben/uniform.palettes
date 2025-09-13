#' Prepare RGB data for palette_info
#'
#' @param rgb_data Data frame or matrix with RGB values by row.
#' @param maxvalue Maximum value in the input RGB data (default = 1).
#' @param channel_map Named character vector mapping `"red"`, `"green"`, `"blue"`
#'   to the corresponding column names in `rgb_data`.
#'
#' @return Numeric matrix of RGB values scaled to `[0,1]` with columns
#'  `red`, `green`, `blue`.
#' @export
df_rgb_prepare <- function(
    rgb_data,
    maxvalue = 1,
    channel_map = c(red = "r", green = "g", blue = "b")) {
  rgb_matrix <- as.matrix(rgb_data[, channel_map])
  colnames(rgb_matrix) <- c("red", "green", "blue")
  rgb_matrix / maxvalue
}

#' Generate palette information from RGB matrix
#'
#' @param rgb_matrix Numeric matrix of RGB values in `[0,1]`.
#' @param hexvalues Optional character vector of HEX codes for the colors.
#'
#' @return Tibble with columns: name, hex, red, green, blue,
#'   L, a, b, delta_2000, cum_delta_2000.
#' @export
palette_info <- function(rgb_matrix, hexvalues = NULL) {
  lab_mat <- rgb2lab(rgb_matrix)

  df <- tibble::tibble(
    name  = rownames(rgb_matrix),
    red   = rgb_matrix[, "red"],
    green = rgb_matrix[, "green"],
    blue  = rgb_matrix[, "blue"],
    L     = lab_mat[, "L"],
    a     = lab_mat[, "a"],
    b     = lab_mat[, "b"]
  )

  # Always create hex column
  if (is.null(hexvalues)) {
    df$hex <- grDevices::rgb(df$red, df$green, df$blue)
  } else {
    df$hex <- toupper(hexvalues)
  }

  delta <- c(NA, purrr::map_dbl(2:nrow(df), function(i) {
    ColorNameR::colordiff(
      color     = as.matrix(df[i, c("L", "a", "b")]),
      reference = as.matrix(df[i - 1, c("L", "a", "b")]),
      metric    = "CIEDE2000"
    )
  }))

  df <- df |>
    dplyr::mutate(
      delta_2000     = delta,
      cum_delta_2000 = cumsum(tidyr::replace_na(delta, 0))
    )

  df
}

#' Generate palette information from HEX colors
#'
#' @param hex_vec Character vector of HEX color codes.
#'
#' @return Tibble as returned by palette_info().
#' @export
hex2palette_info <- function(hex_vec) {
  rgb_mat <- hex2rgb(hex_vec, maxvalue = 1)
  palette_info(rgb_mat, hexvalues = hex_vec)
}
