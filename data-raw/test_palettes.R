# Test hex-vector palette
igepal_hex <- c(
  yellow = "#FFCC00",
  green = "#4EC433",
  blue = "#007bc4",
  red = "#C43E4E"
)

# Test hex-vector palette
igepal_hex2 <- c(
  yellow = "#FFCC00",
  green = "#4EC433",
  soft_blue = "#0099CC",
  blue = "#007bc4",
  red = "#D81126"
)

# Test RGB palette as dataframe
batlow_df <- scico::scico_palette_data("batlow")

usethis::use_data(igepal_hex, igepal_hex2, batlow_df, overwrite = TRUE)
