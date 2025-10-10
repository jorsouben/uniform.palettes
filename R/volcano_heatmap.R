#' Volcano heatmap
#' @description
#' Plots a heatmap of the volcano dataset using `ggplot2`
#'
#'
#' @param pal Vector of hex colour codes, matrix or data.frame
#'
#' @returns NULL
#' @export
#'
volcano_heatmap <- function(pal = batlow_df) {
  pal <- as_colormap(pal)$get_hex()

  volcano_df <- datasets::volcano |>
    tibble::as_tibble(.name_repair = ~ as.character(seq_along(.))) |>
    tibble::rowid_to_column(var = "x") |>
    tidyr::pivot_longer(
      cols = -x,
      names_to = "y",
      values_to = "z",
      names_transform = list(y = as.integer)
    ) |>
    dplyr::mutate(x = as.integer(x))

  # Create the heatmap
  ggplot2::ggplot(volcano_df, aes(x = y, y = x, fill = z)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradientn(colors = pal) +
    ggplot2::coord_fixed() + # Ensures correct aspect ratio
    ggplot2::labs(
      fill = "Elevation (m)"
    ) +
    ggplot2::theme_void()
}
