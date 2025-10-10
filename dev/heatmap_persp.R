# Load the volcano dataset (built-in in R)
data("volcano")

# 1. Basic 3D Perspective Plot
persp(
  x = 1:nrow(volcano), # X-axis (rows of the matrix)
  y = 1:ncol(volcano), # Y-axis (columns of the matrix)
  z = volcano, # Z-axis (elevation data)
  # xlim = c(1, ncol(volcano)) * 10,
  # ylim = c(1, nrow(volcano)) * 10,
  # zlim = range(volcano),
  # ylim = range(y),
  theta = 30, # Angle of rotation around the z-axis
  phi = 30, # Angle of elevation
  expand = 0.5, # Scaling factor
  col = "lightblue", # Color of the surface
  border = "grey", # Border color
  xlab = "X", # X-axis label
  ylab = "Y", # Y-axis label
  zlab = "Elevation" # Z-axis label
)

# 2. Heatmap of the Volcano Dataset
heatmap(
  volcano,
  Rowv = NA, # Disable row clustering
  Colv = NA, # Disable column clustering
  # col = terrain.colors(100),    # Color palette
  col = jet_colors(256), # Color palette
  # col = scico::scico(n = 100, palette = "batlow"),    # Color palette
  scale = "none", # No scaling of data
  # xlab = "X-axis", # X-axis label
  # ylab = "Y-axis", # Y-axis label
  # main = "Heatmap of Volcano Dataset" # Title
  symm = FALSE
)


image(t(volcano), col = jet_colors(256), useRaster = TRUE)

######

# Load required libraries
library(ggplot2)
library(reshape2) # for melting the matrix

# Load the volcano dataset
data(volcano)

# Convert the matrix to a data frame suitable for ggplot2
volcano_df <- melt(volcano)

# Rename columns for clarity
colnames(volcano_df) <- c("x", "y", "z")

volcano_df <- volcano |> tibble::as_tibble()
volcano_df <- tibble::as_tibble(volcano, .name_repair = ~ as.character(seq_along(.)))

volcano_df <- volcano |>
  tibble::as_tibble(.name_repair = ~ as.character(seq_along(.))) |>
  tibble::rowid_to_column(var = "x") |>
  tidyr::pivot_longer(
    cols = -x,
    names_to = "y",
    values_to = "z",
    names_transform = list(y = as.integer)
  ) |>
  mutate(x = as.integer(x))
# Create the heatmap
ggplot(volcano_df, aes(x = y, y = x, fill = z)) +
  geom_tile() +
  scale_fill_gradientn(colors = as_colormap(batlow_df)$get_hex()) +
  coord_fixed() + # Ensures correct aspect ratio
  labs(
    fill = "Elevation (m)"
  ) +
  theme_void()
