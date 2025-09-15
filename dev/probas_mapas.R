# install.packages("sf")
library(sf)
# read_sf()
galmap <-
  st_read(
    "./dev/mapas_gal/Comunidade_Autonoma_IGN.shp"
  )

galmun <-
  st_read(
    "./dev/mapas_gal/Concellos_IGN.shp"
  )

library(ggplot2)

mipaleta <- function(n) {
  extend_palette_equal_ciede2000(
    testpal3,
    n,
    fixed_hex = "#007bc4",
    mode = "anchor_exact"
  )$hex
}


scale_color_mipaleta_d <- function(...) {
  ggplot2::scale_color_manual(values = mipaleta(..1), ...)
}

scale_fill_mipaleta_d <- function(...) {
  ggplot2::scale_fill_manual(values = mipaleta(..1), ...)
}

scale_color_mipaleta_c <- function(...) {
  ggplot2::scale_color_gradientn(colors = mipaleta(256), ...)
}

scale_fill_mipaleta_c <- function(...) {
  ggplot2::scale_fill_gradientn(colors = mipaleta(256), ...)
}

scale_fill_mipaleta_d_auto <- function(data, var, ...) {
  n <- length(unique(data[[var]]))
  ggplot2::scale_fill_manual(values = mipaleta(n), ...)
}

# ggplot(mpg, aes(x = class, fill = class)) +
ggplot(mpg, aes(x = class, y = displ, color = displ)) +
  # geom_bar() +
  # geom_boxplot() +
  geom_count() +
  # scale_fill_manual(values = c("red", "blue", "green", "orange", "purple", "brown", "pink"))
  # scale_fill_manual(values = scico::scico(n = 4, palette = "lajolla"))
  # scale_fill_manual(values = scico::scico(n = 12, palette = "lajolla"))
  scale_color_mipaleta_c()
# scale_fill_continuous(values = pal_anchor$hex)
