#' Invert cumulative arc-length to Lab points at target distances
#' ("Equalize" deltas)
#'
#' @param resampled Output of map_resample().
#' @param targets Numeric vector of target cumulative lengths
#'
#' @return Matrix of Lab points at target positions.
#' @export
path_at_cumdeltas <- function(inputs, targets) {
  # labs <- sampling$labs
  # cumlen <- sampling$cumlen
  n <- length(targets)
  # out <- matrix(NA_real_,
  #   nrow = n, ncol = 3L,
  #   dimnames = list(NULL, c("L", "a", "b"))
  # )
  out <- c()
  total <- tail(inputs, 1L)
  # Clamp targets to [0, total] to avoid edge issues
  # targets <- pmin(pmax(targets, 0), total)

  for (i in seq_len(n)) {
    tlen <- targets[i]
    if (tlen <= 0) {
      out[i] <- 1
      next
    }
    if (tlen >= total) {
      out[i] <- length(inputs)
      next
    }
    j <- base::findInterval(tlen, inputs) # inputs[j] <= tlen < inputs[j+1]
    if (inputs[j + 1] == inputs[j]) {
      out[i] <- j + 1
    } else {
      out[i] <- j
    }
  }
  out
}
