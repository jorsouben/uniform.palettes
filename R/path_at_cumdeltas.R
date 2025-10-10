#' Invert cumulative arc-length to Lab points at target distances
#' ("Equalize" deltas)
#'
#' @param resampled Output of map_resample().
#' @param targets Numeric vector of target cumulative lengths
#'
#' @return Matrix of Lab points at target positions.
#' @export
path_at_cumdeltas <- function(inputs, targets) {
  n <- length(targets)
  out <- c()
  total <- tail(inputs, 1L)

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
