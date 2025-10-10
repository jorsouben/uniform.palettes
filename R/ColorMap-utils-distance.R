#' @describeIn ColorMap Compute pairwise CIEDE2000 distances
ColorMap$set(
  "public",
  "ciede_matrix",
  function(method = "cie2000") {
    lab_vals <- self$get_lab()
    farver::compare_colour(lab_vals, from_space = "lab", method = method)
  }
)

#' @describeIn ColorMap Return sequential CIEDE2000 deltas
ColorMap$set(
  "public",
  "deltas",
  function(method = "cie2000") {
    mat <- self$ciede_matrix(method)
    if (nrow(mat) <= 1) stop("Can't compute deltas for a single colour")
    c(0, mat[cbind(1:(nrow(mat) - 1), 2:ncol(mat))])
  }
)

#' @describeIn ColorMap Return cumulative deltas
ColorMap$set(
  "public",
  "cum_deltas",
  function(method = "cie2000") {
    cumsum(self$deltas(method))
  }
)

#' @describeIn ColorMap Return Lightness channel sequential deltas
ColorMap$set(
  "public",
  "L_deltas",
  function() {
    c(0, diff(self$get_lab()[, "l"]))
  }
)
