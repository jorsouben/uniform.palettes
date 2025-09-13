#' Convert HEX colors to RGB matrix
#'
#' @param x Character vector of HEX color codes (with or without `#`),
#'  named or unnamed.
#' @param maxvalue Maximum value for RGB components in the output (default = 1).
#'
#' @return Numeric matrix with columns `red`, `green`, `blue` and row names
#'  as color names (or HEX codes if names are missing).
#' @export
hex2rgb <- function(x, maxvalue = 1) {
  rgb_matrix <- t(col2rgb(x)) / (255 / maxvalue)

  colornames <- names(x)
  missing_idx <- if (is.null(colornames)) {
    rep(TRUE, length(x))
  } else {
    is.na(colornames) | colornames == ""
  }

  rownames(rgb_matrix)[missing_idx] <- toupper(x[missing_idx])
  rgb_matrix
}

#' Convert RGB matrix to CIE Lab
#'
#' @param rgb_matrix Numeric matrix of RGB values in the range `[0,1]`.
#'
#' @return Numeric matrix with columns `L`, `a`, `b` in the CIE Lab color space.
#' @export
rgb2lab <- function(rgb_matrix) {
  rgb_matrix <- as.matrix(rgb_matrix)

  # Gamma correction
  rgb <- ifelse(rgb_matrix > 0.04045,
    ((rgb_matrix + 0.055) / 1.055)^2.4,
    rgb_matrix / 12.92
  )

  # RGB to XYZ (D65)
  rgb <- rgb * 100
  X <- rgb[, 1] * 0.4124 + rgb[, 2] * 0.3576 + rgb[, 3] * 0.1805
  Y <- rgb[, 1] * 0.2126 + rgb[, 2] * 0.7152 + rgb[, 3] * 0.0722
  Z <- rgb[, 1] * 0.0193 + rgb[, 2] * 0.1192 + rgb[, 3] * 0.9505

  # Normalize by reference white
  X <- X / 95.047
  Y <- Y / 100.000
  Z <- Z / 108.883

  # XYZ to Lab
  f <- function(t) ifelse(t > 0.008856, t^(1 / 3), (7.787 * t) + (16 / 116))
  fx <- f(X)
  fy <- f(Y)
  fz <- f(Z)

  L <- (116 * fy) - 16
  a <- 500 * (fx - fy)
  b <- 200 * (fy - fz)

  lab_matrix <- cbind(L = L, a = a, b = b)
  rownames(lab_matrix) <- rownames(rgb_matrix)
  lab_matrix
}
