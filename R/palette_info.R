# #' Prepare RGB data for palette_info
# #'
# #' @param rgb_data Data frame or matrix with RGB values by row.
# #' @param maxvalue Maximum value in the input RGB data (default = 1).
# #' @param channel_map Named character vector mapping `"red"`, `"green"`, `"blue"`
# #'   to the corresponding column names in `rgb_data`.
# #'
# #' @return Numeric matrix of RGB values scaled to `[0,1]` with columns
# #'  `red`, `green`, `blue`.
# #' @export
# df_rgb_prepare <- function(
#     rgb_data,
#     maxvalue = 1,
#     channel_map = c(red = "r", green = "g", blue = "b")) {
#   rgb_matrix <- as.matrix(rgb_data[, channel_map])
#   colnames(rgb_matrix) <- c("red", "green", "blue")
#   rgb_matrix / maxvalue
# }

#' Prepare RGB data for palette_info
#'
#' @param rgb_data Data frame or matrix with RGB values by row.
#' @param maxvalue Maximum value in the input RGB data (default = 1).
#' @param channel_map Named character vector mapping "red","green","blue"
#'   to the corresponding column names in rgb_data.
#'
#' @return Numeric matrix of RGB values scaled to `[0,1]` with columns red, green, blue.
#' @export
df_rgb_prepare <- function(rgb_data, maxvalue = 1,
                           channel_map = c(red = "red", green = "green", blue = "blue")) {
  rgb_df <- as.data.frame(rgb_data, stringsAsFactors = FALSE)
  missing_cols <- setdiff(unname(channel_map), colnames(rgb_df))
  if (length(missing_cols)) {
    stop("Missing expected columns in rgb_data: ", paste(missing_cols, collapse = ", "))
  }
  mat <- as.matrix(rgb_df[, channel_map, drop = FALSE])
  colnames(mat) <- c("red", "green", "blue")
  mat / maxvalue
}

#' Generate palette information from RGB matrix
#'
#' @param rgb_matrix Numeric matrix of RGB values in `[0,1]`.
#'
#' @return Tibble with columns: name, hex, red, green, blue,
#'   L, a, b, delta_2000, cum_delta_2000.
#' @export
palette_info <- function(rgb_matrix) {
  lab_mat <- rgb2lab(rgb_matrix)

  # Calculate hex values for all colors
  hex_values <- grDevices::rgb(
    rgb_matrix[, "red"],
    rgb_matrix[, "green"],
    rgb_matrix[, "blue"]
  )

  # Use existing rownames or hex values if missing
  names_to_use <- if (!is.null(rownames(rgb_matrix))) {
    ifelse(rownames(rgb_matrix) == "", hex_values, rownames(rgb_matrix))
  } else {
    hex_values
  }

  df <- tibble::tibble(
    name  = names_to_use,
    red   = rgb_matrix[, "red"],
    green = rgb_matrix[, "green"],
    blue  = rgb_matrix[, "blue"],
    L     = lab_mat[, "L"],
    a     = lab_mat[, "a"],
    b     = lab_mat[, "b"],
    hex   = hex_values
  )

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
  # Keep original names before conversion
  original_names <- names(hex_vec)
  rgb_mat <- hex2rgb(hex_vec, maxvalue = 1)
  # If hex_vec was named, ensure those names are preserved
  if (!is.null(original_names)) {
    rownames(rgb_mat) <- original_names
  }
  palette_info(rgb_mat)
}
