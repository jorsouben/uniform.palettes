#' Diagnose perceptual uniformity from a palette_info tibble
#'
#' @param pal_info Tibble from palette_info() or similar, containing at least:
#'   - delta_2000: consecutive CIEDE2000 distances
#'   - L: Lab lightness
#'
#' @return A list with:
#'   - data: tibble with added visual_error_pct
#'   - perceptual_error_pct: RMS of visual_error_pct
#'   - median_local_lightness: median absolute ΔL*
#' @export
diagnose_palette_info <- function(pal_info) {
  stopifnot(all(c("delta_2000", "L") %in% names(pal_info)))

  mean_delta <- mean(pal_info$delta_2000, na.rm = TRUE)
  visual_error_pct <- 100 * (pal_info$delta_2000 - mean_delta) / mean_delta
  perceptual_error_pct <- sqrt(mean(visual_error_pct[-1]^2, na.rm = TRUE))

  local_lightness <- c(NA, diff(pal_info$L))
  median_local_lightness <- stats::median(abs(local_lightness), na.rm = TRUE)

  pal_info <- dplyr::mutate(pal_info,
                            visual_error_pct = visual_error_pct,
                            local_lightness = local_lightness)

  list(
    data = pal_info,
    perceptual_error_pct = perceptual_error_pct,
    median_local_lightness = median_local_lightness
  )
}

#' Diagnose palette from HEX colors
#'
#' @param hex_vec Character vector of HEX colors.
#' @return Output of diagnose_palette_info().
#' @export
diagnose_palette_hex <- function(hex_vec) {
  pal_info <- palette_info(hex2rgb(hex_vec, maxvalue = 1), hexvalues = hex_vec)
  diagnose_palette_info(pal_info)
}

#' Diagnose palette from RGB values
#'
#' @param rgb_data Data frame or matrix of RGB values by row.
#' @param maxvalue Maximum value in RGB data (default 1).
#' @param channel_map Named character vector mapping "red","green","blue" to column names in rgb_data.
#' @return Output of diagnose_palette_info().
#' @export
diagnose_palette_rgb <- function(rgb_data, maxvalue = 1,
                                 channel_map = c(red = "red", green = "green", blue = "blue")) {
  rgb_norm <- df_rgb_prepare(rgb_data, maxvalue = maxvalue, channel_map = channel_map)
  pal_info <- palette_info(rgb_norm)
  diagnose_palette_info(pal_info)
}

#' Plot palette diagnostics from diagnosis output
#'
#' @param diag_list Output from diagnose_palette_info() or its wrappers.
#' @param name Optional palette name.
#'
#' @return A grid of plots.
#' @export
plot_palette_diagnostics <- function(diag_list, name = NULL) {
  library(ggplot2)
  library(gridExtra)
  library(tidyr)
  library(dplyr)

  df <- diag_list$data
  title_text <- if (!is.null(name)) name else "Palette"
  subtitle_text <- sprintf(
    "%.2f%% Perceptual error   |   %.2f Median local lightness contrast",
    diag_list$perceptual_error_pct,
    diag_list$median_local_lightness
  )

  swatch_grob <- if ("hex" %in% names(df)) {
    gridExtra::arrangeGrob(
      grobs = list(
        grid::textGrob(title_text, gp = grid::gpar(fontsize = 16, fontface = "bold")),
        grid::textGrob(subtitle_text, gp = grid::gpar(fontsize = 10)),
        grid::rasterGrob(matrix(df$hex, nrow = 1), interpolate = FALSE)
      ),
      ncol = 1,
      heights = c(0.2, 0.1, 0.2)
    )
  } else {
    gridExtra::arrangeGrob(
      grobs = list(
        grid::textGrob(title_text, gp = grid::gpar(fontsize = 16, fontface = "bold")),
        grid::textGrob(subtitle_text, gp = grid::gpar(fontsize = 10))
      ),
      ncol = 1
    )
  }

  p1 <- ggplot(df[-1,], aes(x = seq_len(nrow(df))[-1], y = visual_error_pct)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_line() + geom_point() +
    labs(y = "Visual error (%)", x = "Index")

  p2 <- ggplot(df, aes(x = seq_len(nrow(df)), y = L)) +
    geom_line() + labs(y = "L*", x = "Index")

  p3 <- ggplot(df[-1,], aes(x = seq_len(nrow(df))[-1], y = local_lightness)) +
    geom_line() + labs(y = "ΔL*", x = "Index")

  if ("red" %in% names(df) && "green" %in% names(df) && "blue" %in% names(df)) {
    df_rgb <- tidyr::pivot_longer(df, c("red","green","blue"),
                                  names_to = "channel", values_to = "value")
    p_rgb <- ggplot(df_rgb, aes(x = seq_len(nrow(df)), y = value, color = channel)) +
      geom_line() + labs(y = "Value (0-1)", x = "Index")
    gridExtra::grid.arrange(swatch_grob, p1, p_rgb, p2, p3, ncol = 1)
  } else {
    gridExtra::grid.arrange(swatch_grob, p1, p2, p3, ncol = 1)
  }
}

library(scico)
batlow_hex <- scico::scico(256, palette = "batlow")

# If we already have palette_info
pal_info <- palette_info(hex2rgb(batlow_hex, maxvalue = 1), hexvalues = batlow_hex)
diag_from_info <- diagnose_palette_info(pal_info)
plot_palette_diagnostics(diag_from_info, name = "batlow")

# From HEX directly
diag_from_hex <- diagnose_palette_hex(batlow_hex)
plot_palette_diagnostics(diag_from_hex, name = "batlow")

# From RGB (0–255)
rgb_255 <- hex2rgb(batlow_hex, maxvalue = 255)
diag_from_rgb <- diagnose_palette_rgb(rgb_255, maxvalue = 255)
plot_palette_diagnostics(diag_from_rgb, name = "batlow")
