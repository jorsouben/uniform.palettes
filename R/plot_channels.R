#' Plot channels
#'
#' @description
#' Create a line plot of the channels in the selected space using `ggplot2`
#'
#' @param pal Vector of hex colour codes, matrix or data.frame
#' @param space One of "lab" or "rgb"
#'
#' @returns NULL
#' @export
#'
plot_channels <- function(pal, space = c("lab", "rgb")) {
  pal <- as_colormap(pal)
  pal$get_lab() |>
    tibble::as_tibble() |>
    mutate(index = dplyr::row_number()) |>
    tidyr::pivot_longer(
      -index,
      names_to = "channel",
      values_to = "value"
    ) |>
    ggplot2::ggplot(
      aes(x = index, y = value, color = channel)
    ) +
    geom_line() +
    theme_minimal()
}
