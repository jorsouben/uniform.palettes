# Test hex-vector palette
igepal_hex <- c(
  yellow = "#FFCC00",
  green = "#4EC433",
  blue = "#007bc4",
  red = "#C43E4E"
)

# Test RGB palette as dataframe
batlow_df <- scico::scico_palette_data("batlow")

usethis::use_data(igepal_hex, batlow_df, overwrite = TRUE)
