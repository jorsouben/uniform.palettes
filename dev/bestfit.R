fit_line_and_sample <- function(mat, n, through_point = NULL) {
  stopifnot(ncol(mat) == 3)

  # PCA for direction
  pca <- prcomp(mat, center = TRUE, scale. = FALSE)
  direction <- pca$rotation[, 1]

  # Anchor point
  if (is.null(through_point)) {
    anchor <- colMeans(mat)
  } else {
    # Ensure it's a numeric vector of length 3
    through_point <- as.numeric(through_point)
    if (length(through_point) != 3) stop("through_point must be length 3")
    # Project through_point onto PCA line through centroid
    centroid <- colMeans(mat)
    t_proj <- sum((through_point - centroid) * direction)
    anchor <- centroid + t_proj * direction
  }

  # Project all points onto the line
  t_values <- (mat - matrix(anchor, nrow(mat), 3, byrow = TRUE)) %*% direction

  # Find the two most distant original points in projection space
  idx_min <- which.min(t_values)
  idx_max <- which.max(t_values)

  # Closest points on the line to those extremes
  t_min <- t_values[idx_min]
  t_max <- t_values[idx_max]

  p_min <- anchor + t_min * direction
  p_max <- anchor + t_max * direction

  # Sample n points between extremes
  t_seq <- seq(t_min, t_max, length.out = n)
  sampled_points <- t( t_seq %*% t(direction) + matrix(anchor, n, 3, byrow = TRUE) )

  list(
    anchor = anchor,
    direction = direction,
    extremes = rbind(p_min, p_max),
    sampled_points = sampled_points
  )
}

labpal <-
base_cols |> hex2rgb() |> rgb2lab()

freefit <-
fit_line_and_sample(labpal, n = 16)$sampled_points |> t()

rgb_ext <- grDevices::convertColor(freefit,
                                   from = "Lab", to = "sRGB",
                                   from.ref.white = "D65", to.ref.white = "D65"
)

colnames(rgb_ext) <- c("r","g","b")

full <-
rgb_ext |>
  df_rgb_prepare() |>
  palette_info()

full$hex |> pals::pal.bands()

# Prueba fijando

throughfit <-
  fit_line_and_sample(labpal, n = 16, through_point = labpal[4,])$sampled_points |> t()

rgb_ext2 <- grDevices::convertColor(throughfit,
                                   from = "Lab", to = "sRGB",
                                   from.ref.white = "D65", to.ref.white = "D65"
)

colnames(rgb_ext2) <- c("r","g","b")

full2 <-
  rgb_ext2 |>
  df_rgb_prepare() |>
  palette_info()

full2$hex |> pals::pal.bands()

set.seed(1)
mat <- matrix(rnorm(30), ncol = 3)

# Default: best-fit line through centroid
res1 <- fit_line_and_sample(mat, n = 5)
res1$extremes
res1$sampled_points

# Force line through a given point (e.g., first row of mat)
res2 <- fit_line_and_sample(mat, n = 5, through_point = mat[1, ])
res2$extremes
res2$sampled_points
