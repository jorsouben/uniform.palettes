#' Measure perceptual uniformity error of a palette
#'
#' Given a vector of consecutive CIEDE2000 distances, compute summary
#' statistics describing deviation from perfect uniformity.
#'
#' @param delta2000 Numeric vector of consecutive ΔE00 distances (e.g., from palette_info()).
#'   May contain NA for the first element; these are ignored.
#'
#' @return A named list with:
#'   - mean_delta: mean ΔE00 (target if perfectly uniform)
#'   - sd_delta: standard deviation of ΔE00
#'   - cv: coefficient of variation (sd / mean)
#'   - mad: mean absolute deviation from mean
#'   - rmse: root mean square error from mean
#'   - max_abs_dev: maximum absolute deviation from mean
#' @export
palette_uniformity_error <- function(delta2000) {
  d <- delta2000[!is.na(delta2000)]
  if (length(d) < 2) stop("Need at least two distances to measure uniformity.")

  mean_d <- mean(d)
  dev <- d - mean_d

  list(
    mean_delta  = mean_d,
    sd_delta    = stats::sd(d),
    cv          = stats::sd(d) / mean_d,
    mad         = mean(abs(dev)),
    rmse        = sqrt(mean(dev^2)),
    max_abs_dev = max(abs(dev))
  )
}

# Suppose we have a palette tibble from extend_palette_equal_ciede2000()
pal <- extend_palette_equal_ciede2000(
  c("#FFCC00", "#4EC433", "#007bc4", "#C43E4E"),
  n = 12,
  fixed_hex = "#007bc4",
  mode = "global_anchor_snap"
)

palette_uniformity_error(pal$delta_2000)
# $mean_delta
# [1] 9.876
# $sd_delta
# [1] 0.012
# $cv
# [1] 0.0012
# $mad
# [1] 0.009
# $rmse
# [1] 0.011
# $max_abs_dev
# [1] 0.021
#
palette_uniformity_error(batinfo$delta_2000)
