perceptual_error_rms <- function(pal) {
  pal <- as_colormap(pal)
  deltas <- pal$deltas()[-1]
  n <- length(deltas)
  total <- sum(deltas)

  # cumulative perceptual distances
  cum_actual <- cumsum(deltas)
  # ideal straight line
  cum_ideal <- seq(from = total / n, by = total / n, length.out = n)

  # RMS deviation of cumulative curve from ideal
  rms <- sqrt(mean((cum_actual - cum_ideal)^2))
  perr <- rms / total * 100
  return(perr)
}
