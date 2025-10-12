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

  method <-
    switch(space,
      lab = pal$get_lab,
      rgb = pal$get_rgb
    )

  method() |>
    tibble::as_tibble() |>
    dplyr::mutate(index = dplyr::row_number()) |>
    tidyr::pivot_longer(
      -index,
      names_to = "channel",
      values_to = "value"
    ) |>
    ggplot2::ggplot(
      ggplot2::aes(x = index, y = value, color = channel)
    ) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "Palette index", y = "Value") +
    ggplot2::theme_minimal()
}
