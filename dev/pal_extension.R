#' CIEDE2000 distance between two Lab rows
#'
#' @param lab_i Numeric length-3 vector (L, a, b) for color i.
#' @param lab_j Numeric length-3 vector (L, a, b) for color j.
#'
#' @return Numeric scalar, Delta E 2000.
#' @export
delta2000_pair <- function(lab_i, lab_j) {
  ColorNameR::colordiff(
    color = matrix(lab_i,
      nrow = 1, byrow = TRUE,
      dimnames = list(NULL, c("L", "a", "b"))
    ),
    reference = matrix(lab_j,
      nrow = 1, byrow = TRUE,
      dimnames = list(NULL, c("L", "a", "b"))
    ),
    metric = "CIEDE2000"
  )[1]
}

#' Sequential CIEDE2000 deltas along a Lab path
#'
#' @param lab_matrix Numeric matrix with columns L, a, b (rows are points along a path).
#'
#' @return Numeric vector of length nrow(lab_matrix) with NA for the first, then consecutive deltas.
#' @export
delta2000_seq <- function(lab_matrix) {
  n <- nrow(lab_matrix)
  if (n < 2) {
    return(rep(NA_real_, n))
  }
  c(NA_real_, vapply(2:n, function(i) {
    delta2000_pair(lab_matrix[i, ], lab_matrix[i - 1, ])
  }, numeric(1)))
}

#' Linear interpolation between two Lab points
#'
#' @param lab1 Numeric length-3 vector (L, a, b).
#' @param lab2 Numeric length-3 vector (L, a, b).
#' @param t Numeric in [0, 1], interpolation parameter.
#'
#' @return Numeric length-3 vector at parameter t.
#' @export
lab_lerp <- function(lab1, lab2, t) {
  lab1 + t * (lab2 - lab1)
}


#' Dense CIEDE2000 arc-length sampling over a Lab polyline
#'
#' @param lab_controls Numeric matrix with columns L, a, b; base control points in order.
#' @param samples_per_segment Integer, number of sub-intervals per segment (default 200).
#'
#' @return A list with:
#'   - labs: sampled Lab points (matrix),
#'   - cumlen: cumulative CIEDE2000 length starting at 0,
#'   - seg_index: segment index for each sampled point (1..nseg),
#'   - seg_t: local t in [0,1] within each segment for each sampled point.
#' @export
lab_path_sample <- function(lab_controls, samples_per_segment = 200L) {
  stopifnot(ncol(lab_controls) == 3L)
  m <- nrow(lab_controls)
  if (m < 2) stop("Need at least two control points.")
  nseg <- m - 1L

  labs_list <- vector("list", nseg)
  t_list <- vector("list", nseg)
  seg_index <- vector("list", nseg)

  for (k in seq_len(nseg)) {
    tvals <- seq(0, 1, length.out = samples_per_segment + 1L)
    seg_labs <- t(vapply(
      tvals, function(tt) lab_lerp(lab_controls[k, ], lab_controls[k + 1, ], tt),
      numeric(3)
    ))
    colnames(seg_labs) <- c("L", "a", "b")
    labs_list[[k]] <- if (k == 1L) seg_labs else seg_labs[-1, , drop = FALSE] # avoid duplicate at junction
    t_list[[k]] <- if (k == 1L) tvals else tvals[-1]
    seg_index[[k]] <- rep.int(k, nrow(labs_list[[k]]))
  }

  labs <- do.call(rbind, labs_list)
  seg_t <- unlist(t_list, use.names = FALSE)
  seg_idx <- unlist(seg_index, use.names = FALSE)

  deltas <- delta2000_seq(labs)
  cumlen <- cumsum(replace(deltas, is.na(deltas), 0))

  list(labs = labs, cumlen = cumlen, seg_index = seg_idx, seg_t = seg_t)
}

#' Invert cumulative arc-length to Lab points at target distances
#'
#' @param sampling Output of lab_path_sample().
#' @param targets Numeric vector of target cumulative lengths (must be within [0, total_length]).
#'
#' @return Matrix of Lab points at target positions.
#' @export
lab_path_at_cumlen <- function(sampling, targets) {
  labs <- sampling$labs
  cumlen <- sampling$cumlen
  n <- length(targets)
  out <- matrix(NA_real_,
    nrow = n, ncol = 3L,
    dimnames = list(NULL, c("L", "a", "b"))
  )
  total <- tail(cumlen, 1L)

  # Clamp targets to [0, total] to avoid edge issues
  targets <- pmin(pmax(targets, 0), total)

  for (i in seq_len(n)) {
    tlen <- targets[i]
    if (tlen <= 0) {
      out[i, ] <- labs[1, ]
      next
    }
    if (tlen >= total) {
      out[i, ] <- labs[nrow(labs), ]
      next
    }
    j <- base::findInterval(tlen, cumlen) # cumlen[j] <= tlen < cumlen[j+1]
    if (cumlen[j + 1] == cumlen[j]) {
      out[i, ] <- labs[j + 1, ]
    } else {
      w <- (tlen - cumlen[j]) / (cumlen[j + 1] - cumlen[j])
      out[i, ] <- labs[j, ] + w * (labs[j + 1, ] - labs[j, ])
    }
  }
  out
}

#' Resample a Lab polyline to n points with CIEDE2000 spacing
#'
#' @param lab_controls Matrix of control points (L,a,b) in order.
#' @param n Integer, number of output points.
#' @param anchor_index Optional integer index in lab_controls to fix exactly.
#' @param mode One of "global" or "anchor_exact".
#'   - "global": perfect global uniform spacing; anchor is set to nearest uniform position (may shift).
#'   - "anchor_exact": anchor fixed exactly; uniform spacing on each side independently.
#' @param samples_per_segment Integer, dense sampling per control segment (default 400).
#'
#' @return Matrix of Lab with n rows and columns L,a,b.
#' @export
resample_lab_equal_ciede2000 <- function(lab_controls, n,
                                         anchor_index = NULL,
                                         mode = c("global", "anchor_exact"),
                                         samples_per_segment = 400L) {
  mode <- match.arg(mode)
  samp <- lab_path_sample(lab_controls, samples_per_segment = samples_per_segment)
  total <- utils::tail(samp$cumlen, 1L)

  if (mode == "global" || is.null(anchor_index)) {
    targets <- seq(0, total, length.out = n)
    return(lab_path_at_cumlen(samp, targets))
  }

  # Anchor-exact: split the path at the anchor cumulative length,
  # resample left and right with their own uniform spacings, and join at the anchor.
  anchor_lab <- lab_controls[anchor_index, ]
  # Find anchor cumulative length by sampling and nearest point
  anchor_pos <- which.min(rowSums((samp$labs - matrix(anchor_lab, nrow(samp$labs), 3L, byrow = TRUE))^2))
  anchor_cum <- samp$cumlen[anchor_pos]

  # Choose counts left/right around anchor so that total is n and anchor included
  # Heuristic: distribute by proportional arc length
  left_len <- anchor_cum
  right_len <- total - anchor_cum
  # Ensure at least 1 on each side besides the anchor when possible
  if (n == 1L) {
    return(matrix(anchor_lab, nrow = 1, dimnames = list(NULL, c("L", "a", "b"))))
  }
  # Number including anchor: n = n_left + n_right - 1
  n_left <- max(1L, round((left_len / total) * (n - 1L))) + 1L
  n_left <- min(n_left, n) # cap
  n_right <- n - n_left + 1L

  # Uniform within each half
  targets_left <- if (n_left == 1L) 0 else seq(0, left_len, length.out = n_left)
  targets_right <- if (n_right == 1L) total else seq(anchor_cum, total, length.out = n_right)

  labs_left <- lab_path_at_cumlen(samp, targets_left)
  labs_right <- lab_path_at_cumlen(samp, targets_right)[-1, , drop = FALSE] # drop duplicate anchor

  labs <- rbind(labs_left, labs_right)
  # Force the exact anchor Lab at the appropriate row
  anchor_row <- n_left
  labs[anchor_row, ] <- anchor_lab
  labs
}

#' Extend a base HEX palette to n colors with uniform CIEDE2000 spacing
#'
#' @param hex_base Character vector of base HEX colors in order.
#' @param n Integer, desired length of the extended palette.
#' @param fixed_hex Optional HEX of a base color to keep exactly fixed.
#' @param mode "global" or "anchor_exact" (see resample_lab_equal_ciede2000()).
#' @param samples_per_segment Dense sampling per control segment (default 400).
#'
#' @return Tibble with name, hex, red, green, blue, L, a, b, delta_2000, cum_delta_2000.
#' @export
extend_palette_equal_ciede2000 <- function(hex_base, n,
                                           fixed_hex = NULL,
                                           mode = c("global", "anchor_exact"),
                                           samples_per_segment = 400L) {
  mode <- match.arg(mode)
  # Base to Lab
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

  # Lab -> sRGB in [0,1], clamp
  rgb_ext <- grDevices::convertColor(lab_ext,
    from = "Lab", to = "sRGB",
    from.ref.white = "D65", to.ref.white = "D65"
  )
  rgb_ext <- pmax(pmin(rgb_ext, 1), 0)
  colnames(rgb_ext) <- c("red", "green", "blue")

  # Build final tibble via palette_info (auto-generates HEX)
  tib <- palette_info(rgb_ext)

  # Mark the fixed color name if it exactly matches one row
  if (!is.null(anchor_index)) {
    # Inject the exact HEX for the anchor in case rounding moved it slightly
    base_anchor_hex <- toupper(fixed_hex)
    # Overwrite the closest row in Lab to the base anchor
    anchor_lab <- lab_base[anchor_index, ]
    row_match <- which.min(rowSums((as.matrix(tib[, c("L", "a", "b")]) -
      matrix(anchor_lab, nrow(tib), 3L, byrow = TRUE))^2))
    tib$hex[row_match] <- base_anchor_hex
  }

  tib
}

#' Prepare RGB data for palette_info
#'
#' @param rgb_data Data frame or matrix with RGB values by row.
#' @param maxvalue Maximum value in the input RGB data (default = 1).
#' @param channel_map Named character vector mapping "red","green","blue"
#'   to the corresponding column names in rgb_data.
#'
#' @return Numeric matrix of RGB values scaled to [0,1] with columns red, green, blue.
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

base_cols <- c(yellow = "#FFCC00", green = "#4EC433", blue = "#007bc4", red = "#C43E4E")

# 1) Global uniform spacing (anchor may shift slightly to nearest grid)
pal_global <- extend_palette_equal_ciede2000(base_cols, n = 12, mode = "global")

# 2) Anchor exact: keep "#007bc4" exactly; uniform spacing on each side
pal_anchor <- extend_palette_equal_ciede2000(base_cols,
  n = 12,
  fixed_hex = "#007bc4",
  mode = "anchor_exact"
)

# Inspect uniformity
pal_anchor %>% dplyr::select(name, hex, delta_2000, cum_delta_2000)
