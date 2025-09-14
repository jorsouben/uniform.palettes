#' Compute palette diagnostics
#'
#' @param hex_vec Character vector of HEX colors (palette).
#'
#' @return A list with:
#'   - tibble of RGB, Lab, ΔE2000, local lightness change
#'   - summary stats: perceptual_error_pct, median_local_lightness
#' @export
palette_diagnostics <- function(hex_vec) {
  rgb_mat <- hex2rgb(hex_vec, maxvalue = 1)
  lab_mat <- rgb2lab(rgb_mat)

  # ΔE2000 between consecutive colors
  delta <- delta2000_seq(lab_mat)

  # Perceptual error: CV of ΔE2000 as %
  d <- delta[!is.na(delta)]
  perceptual_error_pct <- 100 * (stats::sd(d) / mean(d))

  # Local lightness change
  local_lightness <- c(NA, diff(lab_mat[, "L"]))
  median_local_lightness <- stats::median(abs(local_lightness), na.rm = TRUE)

  df <- tibble::tibble(
    idx = seq_along(hex_vec),
    hex = toupper(hex_vec),
    red = rgb_mat[, "red"],
    green = rgb_mat[, "green"],
    blue = rgb_mat[, "blue"],
    L = lab_mat[, "L"],
    a = lab_mat[, "a"],
    b = lab_mat[, "b"],
    delta2000 = delta,
    local_lightness = local_lightness
  )

  list(
    data = df,
    perceptual_error_pct = perceptual_error_pct,
    median_local_lightness = median_local_lightness
  )
}


#' Plot palette diagnostics similar to Scientific Colour Maps
#'
#' @param hex_vec Character vector of HEX colors.
#' @param name Optional palette name for title.
#'
#' @return A ggplot2 object (grid of plots).
#' @export
plot_palette_diagnostics <- function(hex_vec, name = NULL) {
  library(ggplot2)
  library(gridExtra)

  diag <- palette_diagnostics(hex_vec)
  df <- diag$data

  title_text <- if (!is.null(name)) name else "Palette"
  subtitle_text <- sprintf(
    "%.2f%% Perceptual error   |   %.2f Median local lightness contrast",
    diag$perceptual_error_pct,
    diag$median_local_lightness
  )

  # Swatch
  swatch <- pals::pal.sineramp(hex_vec) +
    ggtitle(title_text, subtitle = subtitle_text)

  # Visual error plot
  p1 <- ggplot(df[-1, ], aes(x = idx, y = delta2000)) +
    geom_line() +
    geom_point() +
    labs(y = "ΔE2000", x = "Index", title = "Visual error (%)")

  # RGB channels
  df_rgb <- tidyr::pivot_longer(df, c("red", "green", "blue"), names_to = "channel", values_to = "value")
  p2 <- ggplot(df_rgb, aes(x = idx, y = value, color = channel)) +
    geom_line() +
    labs(y = "Value (0-1)", x = "Index", title = "RGB")

  # Lab lightness
  p3 <- ggplot(df, aes(x = idx, y = L)) +
    geom_line() +
    labs(y = "L*", x = "Index", title = "Lab Lightness")

  # Local lightness change
  p4 <- ggplot(df[-1, ], aes(x = idx, y = local_lightness)) +
    geom_line() +
    labs(y = "ΔL*", x = "Index", title = "Local lightness change")

  gridExtra::grid.arrange(swatch, p1, p2, p3, p4, ncol = 1)
}
library(scico)
batlow_hex <- scico::scico(256, palette = "batlow")

plot_palette_diagnostics(batlow_hex, name = "batlow")


#' Plot palette diagnostics similar to Scientific Colour Maps
#'
#' @param hex_vec Character vector of HEX colors.
#' @param name Optional palette name for title.
#'
#' @return A grid of plots.
#' @export
plot_palette_diagnostics <- function(hex_vec, name = NULL) {
  library(ggplot2)
  library(gridExtra)
  library(pals)
  library(tidyr)
  library(dplyr)

  diag <- palette_diagnostics(hex_vec)
  df <- diag$data

  title_text <- if (!is.null(name)) name else "Palette"
  subtitle_text <- sprintf(
    "%.2f%% Perceptual error   |   %.2f Median local lightness contrast",
    diag$perceptual_error_pct,
    diag$median_local_lightness
  )

  ## Swatch as a grob
  swatch_grob <- gridExtra::arrangeGrob(
    grobs = list(
      grid::textGrob(title_text, gp = grid::gpar(fontsize = 16, fontface = "bold")),
      grid::textGrob(subtitle_text, gp = grid::gpar(fontsize = 10)),
      grid::rasterGrob(matrix(hex_vec, nrow = 1), interpolate = FALSE)
    ),
    ncol = 1,
    heights = c(0.2, 0.1, 0.2)
  )

  ## Visual error (%) = % deviation from mean ΔE2000
  mean_delta <- mean(df$delta2000, na.rm = TRUE)
  df <- df %>%
    mutate(visual_error_pct = 100 * (delta2000 - mean_delta) / mean_delta)

  p1 <- ggplot(df[-1,], aes(x = idx, y = visual_error_pct)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_line() + geom_point() +
    labs(y = "Visual error (%)", x = "Index")

  ## RGB channels
  df_rgb <- pivot_longer(df, c("red","green","blue"),
                         names_to = "channel", values_to = "value")
  p2 <- ggplot(df_rgb, aes(x = idx, y = value, color = channel)) +
    geom_line() + labs(y = "Value (0-1)", x = "Index")

  ## Lab lightness
  p3 <- ggplot(df, aes(x = idx, y = L)) +
    geom_line() + labs(y = "L*", x = "Index")

  ## Local lightness change
  p4 <- ggplot(df[-1,], aes(x = idx, y = local_lightness)) +
    geom_line() + labs(y = "ΔL*", x = "Index")

  gridExtra::grid.arrange(swatch_grob, p1, p2, p3, p4, ncol = 1)
}

