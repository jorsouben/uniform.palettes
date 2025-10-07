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
  theta = 0, # Angle of rotation around the z-axis
  phi = 90, # Angle of elevation
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
  xlab = "X-axis", # X-axis label
  ylab = "Y-axis", # Y-axis label
  main = "Heatmap of Volcano Dataset" # Title
)
