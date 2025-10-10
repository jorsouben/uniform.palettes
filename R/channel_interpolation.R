# Corrects the number of output points to match the input points if
# spread evenly
#' Corrects the number of output points to match the input points if
#' spread evenly
#'
#' @param n_out Integer, number of target points for interpolation
#' @param n_in Integer, number of initial points
#'
#' @returns Integer, corrected `n_out`
#' @export
#'
snap_to_fit <- function(n_out, n_in) {
  steps <- n_in - 1
  offset <- (n_out - n_in) %% steps
  n_out - offset
}

#' Fit a spline through the given values. By default it ensures the output
#' includes the input points
#' @param channel Numeric vector of values to interpolate.
#' @param n_out Integer, number of output interpolated values.
#' @param ensure_fit Logical, whether to ensure original points are present
#'  in the output
#'
#' @returns Numeric vector of length `n_out`. Spline interpolated values.
#' @export
#'
channel_interpolation <- function(channel, n_out = 1000L, ensure_fit = TRUE) {
  n_in <- length(channel)
  # If ensure_fit, we adjust n so the spline will pass through all the
  # original points
  if (ensure_fit) {
    n_out <- snap_to_fit(n_out, n_in)
  }

  spfun <- splinefun(
    x = 1:n_in,
    y = channel,
    method = "monoH.FC"
  )

  spfun(seq(from = 1, to = n_in, length.out = n_out))
}

#' Apply interpolation by columns to a matrix of values
#'
#' @param mat Numeric matrix
#' @param n_out Integer, number of output rows
#' @param ensure_fit Logical, whether to ensure original points are present
#'  in the output
#'
#' @returns Numeric matrix with `n_out` rows. Spline interpolated values.
#' @export
#'
matrix_interpolation <- function(mat, n_out = 1000L, ensure_fit = TRUE) {
  apply(
    mat,
    2,
    function(col) channel_interpolation(col, n_out, ensure_fit)
  )
}

# # Resamples a ColorMap by interpolation in LAB space
# # Returns a ColorMap object with the resampled data
# map_resample <- function(pal, space = c("lab", "rgb"), n_out = 1000L, ensure_fit = TRUE) {
#   pal <- as_colormap(pal)
#
#   pal$get_lab() |>
#     matrix_interpolation(n_out, ensure_fit) |>
#     ColorMap$new()
# }

# Resamples a ColorMap by interpolation in LAB or RGB space
# Returns a ColorMap object with the resampled data
#' Title
#'
#' @param pal Character vector of hex color codes, `ColorMap` object, `matrix`
#'  or `data.frame`
#' @param space One of "lab" or "rgb". Defaults to "lab"
#' @param n_out Integer, number of colors after interpolation
#' @param ensure_fit Logical, whether to ensure original points are present
#'  in the output
#'
#' @returns `ColorMap` object with the values interpolated in the given space.
#' @export
map_resample <- function(pal, space = c("lab", "rgb"), n_out = 1000L, ensure_fit = TRUE) {
  space <- match.arg(space) # Ensures only "lab" or "rgb" are accepted
  pal <- as_colormap(pal)

  method <-
    switch(space,
      lab = pal$get_lab,
      rgb = pal$get_rgb
    )

  method() |>
    matrix_interpolation(n_out, ensure_fit) |>
    ColorMap$new()
}

#' Equalize n deltas after resampling a colormap
#'
#' @param pal Character vector of hex color codes, `ColorMap` object, `matrix`
#'  or `data.frame`
#' @param n Integer, number of colors in the output `ColorMap`
#' @param anchor Hex code or index of a color to keep anchored in the output
#'  `Not yet implemented`
#' @param n_resample Integer, number of points of the intermediate interpolation.
#'
#' @returns `ColorMap` object with the values interpolated in the given space
#'  and equalized deltas.
#' @export
#'
equalize <- function(pal, n, anchor = NULL, n_resample = 3000L) {
  resample <- map_resample(pal, space = "lab", n_out = n_resample, ensure_fit = TRUE)
  cdeltas <- resample$cum_deltas()
  total <- cdeltas |> tail(1L)
  if (is.null(anchor)) {
    targets <- seq(0, total, length.out = n)
    indexes <- path_at_cumdeltas(cdeltas, targets)
    eq_lab <- resample$get_lab()[indexes, ]
    ColorMap$new(eq_lab, space = "lab")
  } else if (!anchor %in% pal$index()) {
    anchor <- pal$match_hex(anchor)
    if (is.na(pal$match_hex(anchor))) stop("Invalid anchor")
    stop("anchoring not yet implemented")
  }
}
