install.packages("scico")
install.packages("pals")
library(scico)
library(pals)


igepal_hex <- c(
  `PANTONE 186 C` = "#C8102E",
  `PANTONE 7461 C` = "#007DBA",
  `PANTONE Yellow` = "#FEDD00"
)

cores_xunta <- c(
  `7461 C` = "#007BC4",
  `7463 C` = "#002B4A",
  `50% 7461 C` = "#7FBDE1",
  `25% 7461 C` = "#BFDEF0",
  `10% 7461 C` = "#E5F1F9",
  `Branco` = "#FFFFFF"
)

# Las paletas de scico en hex se llaman así
paleta <- scico(n = 5, palette = "hawaii")

# Bandas de colores
paleta |> pal.bands()

# todos los nombres
scico_palette_names()

# Ver todas
scico_palette_show()

# para usarlas en ggplot
# scale_fill_scico / scale_fill_scico_d
