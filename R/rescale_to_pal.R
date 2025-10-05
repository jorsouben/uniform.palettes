#' Rescale dataset values to reflect the perceptual error induced
#' by the given palette
#'
#' Rescale values based on perceptual differences in a color palette
#'
#' @param values Numeric vector of values to rescale
#' @param pal ColorMap object or hex color codes vector
#' @param L_direction Logical, indicates if the direction of the L shifts should
#'  be taken into account for the calculations
#' @return A numeric vector of rescaled values
#' @importFrom stats approx
#' @export
rescale_to_pal <- function(values, pal, L_direction = FALSE) {
  # Try to generate ColorMap object if an hex vector is passed
  if (!inherits(pal, "ColorMap")) {
    pal <- ColorMap$new(pal)
  }

  deltas <- pal$deltas()
  # Signed ciede2000 deltas
  if (L_direction) {
    deltas <- deltas * sign(pal$L_diff())
  }
  cum_diffs <- cumsum(deltas)
  # Normalize input values to [0,1]
  vals_norm <- (values - min(values)) / diff(range(values))

  # Create interpolation function
  height_factors <- approx(
    x = seq(0, 1, length.out = length(deltas)),
    y = cum_diffs / max(cum_diffs),
    xout = vals_norm
  )$y

  hf_matrix <- matrix(
    height_factors,
    nrow = nrow(values)
  )

  # return(hf_matrix)
  # Apply the height factors to original scale
  values_range <- diff(range(values))
  values_min <- min(values)
  values_min + hf_matrix * values_range
}
