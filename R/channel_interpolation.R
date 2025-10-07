# Corrects the number of output points to match the input points if
# spread evenly
snap_to_fit <- function(n_out, n_in) {
  steps <- n_in - 1
  offset <- (n_out - n_in) %% steps
  n_out - offset
}

# Fits a spline through the given values. By default it ensures the output
# includes the input points
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

# Applies interpolation by columns to a matrix of values
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

# Equalizes n deltas after resampling a colormap
# if an anchor is given, it ensures it appears in the final palette
# anchor can be an index or an hex color code
# For using the anchor we have to calculate the position of that color
# in the resampled pal
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
    if (is.na(anchor)) stop("Invalid anchor")
    stop("anchoring not yet implemented")
  }
}
