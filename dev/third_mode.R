#' Resample a Lab polyline to n points with CIEDE2000 spacing
#'
#' @param lab_controls Matrix of control points (L,a,b) in order.
#' @param n Integer, number of output points.
#' @param anchor_index Optional integer index in lab_controls to fix exactly.
#' @param mode One of "global", "anchor_exact", or "global_anchor_snap".
#'   - "global": perfect global uniform spacing; anchor may shift slightly.
#'   - "anchor_exact": anchor fixed exactly; uniform spacing on each side independently.
#'   - "global_anchor_snap": perfect global uniform spacing; anchor fixed exactly and snapped to nearest grid point.
#' @param samples_per_segment Integer, dense sampling per control segment (default 400).
#'
#' @return Matrix of Lab with n rows and columns L,a,b.
#' @export
resample_lab_equal_ciede2000 <- function(lab_controls, n,
                                         anchor_index = NULL,
                                         mode = c("global", "anchor_exact", "global_anchor_snap"),
                                         samples_per_segment = 400L) {
  mode <- match.arg(mode)
  samp <- lab_path_sample(lab_controls, samples_per_segment = samples_per_segment)
  total <- utils::tail(samp$cumlen, 1L)

  if (is.null(anchor_index) || mode == "global") {
    targets <- seq(0, total, length.out = n)
    return(lab_path_at_cumlen(samp, targets))
  }

  if (mode == "anchor_exact") {
    anchor_lab <- lab_controls[anchor_index, ]
    anchor_pos <- which.min(rowSums((samp$labs - matrix(anchor_lab, nrow(samp$labs), 3L, byrow = TRUE))^2))
    anchor_cum <- samp$cumlen[anchor_pos]

    left_len <- anchor_cum
    right_len <- total - anchor_cum
    n_left <- max(1L, round((left_len / total) * (n - 1L))) + 1L
    n_left <- min(n_left, n)
    n_right <- n - n_left + 1L

    targets_left <- if (n_left == 1L) 0 else seq(0, left_len, length.out = n_left)
    targets_right <- if (n_right == 1L) total else seq(anchor_cum, total, length.out = n_right)

    labs_left <- lab_path_at_cumlen(samp, targets_left)
    labs_right <- lab_path_at_cumlen(samp, targets_right)[-1, , drop = FALSE]

    labs <- rbind(labs_left, labs_right)
    labs[n_left, ] <- anchor_lab
    return(labs)
  }

  if (mode == "global_anchor_snap") {
    # Perfect global spacing
    targets <- seq(0, total, length.out = n)

    # Find anchor's cumulative length
    anchor_lab <- lab_controls[anchor_index, ]
    anchor_pos <- which.min(rowSums((samp$labs - matrix(anchor_lab, nrow(samp$labs), 3L, byrow = TRUE))^2))
    anchor_cum <- samp$cumlen[anchor_pos]

    # Find nearest target index
    snap_idx <- which.min(abs(targets - anchor_cum))
    targets[snap_idx] <- anchor_cum # force exact anchor position

    labs <- lab_path_at_cumlen(samp, targets)
    labs[snap_idx, ] <- anchor_lab
    return(labs)
  }
}


#' Extend a base HEX palette to n colors with uniform CIEDE2000 spacing
#'
#' @param hex_base Character vector of base HEX colors in order.
#' @param n Integer, desired length of the extended palette.
#' @param fixed_hex Optional HEX of a base color to keep exactly fixed.
#' @param mode "global", "anchor_exact", or "global_anchor_snap".
#' @param samples_per_segment Dense sampling per control segment (default 400).
#'
#' @return Tibble with name, hex, red, green, blue, L, a, b, delta_2000, cum_delta_2000.
#' @export
extend_palette_equal_ciede2000 <- function(hex_base, n,
                                           fixed_hex = NULL,
                                           mode = c("global", "anchor_exact", "global_anchor_snap"),
                                           samples_per_segment = 400L) {
  mode <- match.arg(mode)
  rgb_base <- hex2rgb(hex_base, maxvalue = 1)
  lab_base <- rgb2lab(rgb_base)

  anchor_index <- NULL
  if (!is.null(fixed_hex)) {
    idx <- which(toupper(hex_base) == toupper(fixed_hex))
    if (length(idx) == 1L) anchor_index <- idx
  }

  lab_ext <- resample_lab_equal_ciede2000(lab_base, n,
    anchor_index = anchor_index,
    mode = mode,
    samples_per_segment = samples_per_segment
  )

  rgb_ext <- grDevices::convertColor(lab_ext,
    from = "Lab", to = "sRGB",
    from.ref.white = "D65", to.ref.white = "D65"
  )
  rgb_ext <- pmax(pmin(rgb_ext, 1), 0)
  colnames(rgb_ext) <- c("red", "green", "blue")

  tib <- palette_info(rgb_ext)

  if (!is.null(anchor_index)) {
    base_anchor_hex <- toupper(fixed_hex)
    anchor_lab <- lab_base[anchor_index, ]
    row_match <- which.min(rowSums((as.matrix(tib[, c("L", "a", "b")]) -
      matrix(anchor_lab, nrow(tib), 3L, byrow = TRUE))^2))
    tib$hex[row_match] <- base_anchor_hex
  }

  tib
}

base_cols <- c(yellow = "#FFCC00", green = "#4EC433", blue = "#007bc4", red = "#C43E4E")

# Global uniform spacing, anchor snapped to nearest grid point
pal_snap <- extend_palette_equal_ciede2000(base_cols,
  n = 12,
  fixed_hex = "#007bc4",
  mode = "global" # "global_anchor_snap"
)

pal_snap %>% dplyr::select(hex, delta_2000, cum_delta_2000)
