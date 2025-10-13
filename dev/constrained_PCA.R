fit_line_and_sample <- function(mat, n,
                                mode = c("pca", "two_points"),
                                through_point = NULL,
                                point1 = NULL,
                                point2 = NULL) {
  stopifnot(ncol(mat) == 3)
  mode <- match.arg(mode)

  if (mode == "pca") {
    if (is.null(through_point)) {
      # Standard PCA
      pca <- prcomp(mat, center = TRUE, scale. = FALSE)
      direction <- pca$rotation[, 1]
      anchor <- colMeans(mat)
    } else {
      # Constrained PCA: center at through_point
      through_point <- as.numeric(through_point)
      if (length(through_point) != 3) stop("through_point must be length 3")
      centered <- sweep(mat, 2, through_point)
      pca <- prcomp(centered, center = FALSE, scale. = FALSE)
      direction <- pca$rotation[, 1]
      anchor <- through_point
    }

  } else if (mode == "two_points") {
    # Direction from two points
    if (is.null(point1) || is.null(point2)) {
      stop("For mode='two_points', you must supply point1 and point2")
    }
    p1 <- if (length(point1) == 1) mat[point1, ] else as.numeric(point1)
    p2 <- if (length(point2) == 1) mat[point2, ] else as.numeric(point2)
    direction <- p2 - p1
    direction <- direction / sqrt(sum(direction^2)) # normalize

    if (is.null(through_point)) {
      anchor <- p1
    } else {
      through_point <- as.numeric(through_point)
      if (length(through_point) != 3) stop("through_point must be length 3")
      t_proj <- sum((through_point - p1) * direction)
      anchor <- p1 + t_proj * direction
    }
  }

  # Project all points onto the line
  t_values <- (mat - matrix(anchor, nrow(mat), 3, byrow = TRUE)) %*% direction

  # Find extremes based on farthest projections
  idx_min <- which.min(t_values)
  idx_max <- which.max(t_values)

  t_min <- t_values[idx_min]
  t_max <- t_values[idx_max]

  p_min <- anchor + t_min * direction
  p_max <- anchor + t_max * direction

  # Sample n points
  t_seq <- seq(t_min, t_max, length.out = n)
  sampled_points <- t( t_seq %*% t(direction) + matrix(anchor, n, 3, byrow = TRUE) )

  list(
    mode = mode,
    anchor = anchor,
    direction = direction,
    extremes = rbind(p_min, p_max),
    sampled_points = sampled_points
  )
}

# mat <- colorRampPalette(paleta$get_hex())(16)
mat <- matrix(rnorm(30), ncol = 3)

# Standard PCA
res_std <- fit_line_and_sample(mat, n = 5, mode = "pca")

# Constrained PCA through first point
res_constr <- fit_line_and_sample(mat, n = 5, mode = "pca", through_point = mat[1, ])

res_std$direction
res_constr$direction  # different if through_point is far from centroid


labpal <-
  base_cols |> hex2rgb() |> rgb2lab()

freefit <-
  fit_line_and_sample(labpal, n = 16, mode = "pca")$sampled_points |> t()

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
  fit_line_and_sample(labpal, n = 256, mode = "pca", through_point = labpal[3,])$sampled_points |> t()

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


# Install if needed
# install.packages(c("plotly", "ggplot2"))

library(ggplot2)
library(plotly)

# --- Example data ---
set.seed(1)
mat <- matrix(rnorm(30), ncol = 3)
colnames(mat) <- c("L", "a", "b")

# --- Use the constrained PCA fit ---
res <- fit_line_and_sample(
  mat,
  n = 10,
  mode = "pca",
  through_point = mat[1, ]  # force line through first point
)


# mat <- colorRampPalette(igepal_hex)(15)
# labpal <- as_colormap(mat)$get_lab()
labpal <- as_colormap(igepal_hex)$get_lab()
# res <- fit_line_and_sample(labpal, n = 256, mode = "pca", through_point = labpal[8,])
res <- fit_line_and_sample(labpal, n = 256, mode = "pca", through_point = labpal[2,])

library(plotly)

# # Example data
# set.seed(1)
# mat <- matrix(rnorm(30), ncol = 3)
# colnames(mat) <- c("L", "a", "b")
#
# # Use the constrained PCA fit (function from earlier)
# res <- fit_line_and_sample(
#   mat,
#   n = 10,
#   mode = "pca",
#   through_point = mat[1, ]  # force line through first point
# )

# Prepare data frames
df_points   <- as.data.frame(labpal)
df_line     <- as.data.frame(res$sampled_points |> t())
df_extremes <- as.data.frame(res$extremes)
df_anchor   <- as.data.frame(t(res$anchor))

names(df_points)   <- c("L", "a", "b")
names(df_line)     <- c("L", "a", "b")
names(df_extremes) <- c("L", "a", "b")
names(df_anchor)   <- c("L", "a", "b")

# 3D plot with native pipe
plot_ly() |>
  add_markers(
    data = df_points, x = ~L, y = ~a, z = ~b,
    marker = list(color = 'gray', size = 4),
    name = "Original points"
  ) |>
  add_markers(
    data = df_anchor, x = ~L, y = ~a, z = ~b,
    marker = list(color = 'green', size = 6, symbol = 'diamond'),
    name = "Anchor point"
  ) |>
  add_trace(
    data = df_line, x = ~L, y = ~a, z = ~b,
    type = 'scatter3d', mode = 'lines+markers',
    line = list(color = 'red', width = 4),
    marker = list(color = 'blue', size = 4),
    name = "Fitted line & samples"
  ) |>
  add_markers(
    data = df_extremes, x = ~L, y = ~a, z = ~b,
    marker = list(color = 'orange', size = 6, symbol = 'x'),
    name = "Extremes"
  ) |>
  layout(
    scene = list(
      xaxis = list(title = "L"),
      yaxis = list(title = "a"),
      zaxis = list(title = "b")
    )
  )

# prueba con colores por punto

# install.packages("farver")
library(farver)

# mat is your L,a,b matrix
lab_to_hex <- function(lab_mat) {
  # farver expects a matrix with columns L, a, b
  rgb <- convert_colour(lab_mat, from = "lab", to = "rgb", white_from = "D65")
  # Clamp to [0,255] and convert to hex
  rgb <- pmax(pmin(rgb, 255), 0)
  rgb(rgb[,1]/255, rgb[,2]/255, rgb[,3]/255)
}

df_points$col  <- lab_to_hex(as.matrix(df_points))
df_line$col    <- lab_to_hex(as.matrix(df_line))
df_extremes$col <- lab_to_hex(as.matrix(df_extremes))
df_anchor$col  <- lab_to_hex(as.matrix(df_anchor))

plot_ly(type = "scatter3d", mode = "markers") |>
  add_markers(
    data = df_points, x = ~a, y = ~b, z = ~L,
    marker = list(color = df_points$col, size = 4),
    name = "Original points"
  ) |>
  add_markers(
    data = df_anchor, x = ~a, y = ~b, z = ~L,
    marker = list(color = df_anchor$col, size = 6, symbol = 'diamond'),
    name = "Anchor point"
  ) |>
  add_trace(
    data = df_line, x = ~a, y = ~b, z = ~L,
    type = 'scatter3d', mode = 'lines+markers',
    line = list(color = 'black', width = 2), # line stays visible
    marker = list(color = df_line$col, size = 4),
    name = "Fitted line & samples"
  ) |>
  add_markers(
    data = df_extremes, x = ~a, y = ~b, z = ~L,
    marker = list(color = df_extremes$col, size = 6, symbol = 'x'),
    name = "Extremes"
  ) |>
  layout(
    scene = list(
      xaxis = list(title = "L"),
      yaxis = list(title = "a"),
      zaxis = list(title = "b")
    )
  )

