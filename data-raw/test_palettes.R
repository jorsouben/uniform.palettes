# Test hex-vector palette
igepal_hex <- c(
  `PANTONE 186 C` = "#C8102E",
  `PANTONE 7461 C` = "#007DBA",
  `PANTONE Yellow` = "#FEDD00"
)

# # Test hex-vector palette
# igepal_hex2 <- c(
#   yellow = "#FFCC00",
#   green = "#4EC433",
#   soft_blue = "#0099CC",
#   blue = "#007DBA",
#   red = "#C8102E"
# )

# Test RGB palette as dataframe
batlow_df <- scico::scico_palette_data("batlow")

# usethis::use_data(igepal_hex, igepal_hex2, batlow_df, overwrite = TRUE)
usethis::use_data(igepal_hex, batlow_df, overwrite = TRUE)
