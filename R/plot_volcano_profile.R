#' Profile comparison
#'
#' @description
#' Create a line plot using `ggplot2` that compares the real profile of
#' the volcano dataset against the distortion induced by a colormap.
#'
#' @param pal Vector of hex colour codes, matrix or data.frame or ColorMap object
#' @param signed_deltas Logical, whether to use or not Luminance sign in the
#'   simulation of the perceived error.
#'
#' @returns NULL
#' @export
#'
volcano_profile <- function(pal = jet_colors(256), signed_deltas = TRUE) {
  original <- volcano[, 34]

  induced <- rescale_to_pal(as.matrix(original), pal, L_direction = signed_deltas) |> as.vector()

  x_profile <- (seq_len(nrow(volcano)) - 0.5) * 10

  profile_data <- tibble::tibble(
    x = x_profile,
    original,
    induced
  ) |>
    tidyr::pivot_longer(
      c(original, induced),
      names_to = "Profile",
      values_to = "Elevation"
    )

  profile_data |>
    ggplot2::ggplot(
      ggplot2::aes(x = x, y = Elevation, color = Profile)
    ) +
    ggplot2::coord_fixed() +
    ggplot2::geom_line() +
    ggplot2::theme_minimal()
}
