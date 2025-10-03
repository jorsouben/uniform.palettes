# Mapa concellos con cor por provincias

library(ggplot2)
library(patchwork)
# paleta <- "hawaii"
# paleta <- "bilbao"
# paleta <- "brok"

# print(
#   scico::scico(5, palette = paleta)
# )

map_path <-
  system.file(
    "extdata/map_gal/Concellos_IGN.shp",
    package = "unipals"
  )

map_data <- sf::st_read(map_path, quiet = TRUE)

# map_data <-
#   map_data |>
#   dplyr::mutate(
#     prov = as.factor(CodPROV)
#   )
mapacor <- function(pal_scico) {
  ggplot(map_data) +
    # geom_sf(aes(fill = prov)) +
    geom_sf(aes(fill = Provincia)) +
    # scico::scale_fill_scico_d(palette = pal_scico) +
    scico::scale_fill_scico_d(palette = pal_scico, categorical == TRUE) +
    theme_minimal() +
    # labs(title = glue::glue("Paleta 'scico': {pal_scico}"))
    labs(title = pal_scico)
}

bloque_maps <- function(lista_parcial) {
  # bloq <-
  lista_parcial |>
    purrr::map(mapacor) |>
    wrap_plots(ncol = 3)
}

all_pals <-
  scico::scico_palette_names()

bloques <-
  all_pals |>
  # bloques de 9
  split(f = ceiling(seq_along(all_pals) / 9))

grabar <- function(i) {
  p <- bloque_maps(bloques[[i]])
  ggsave(
    filename = glue::glue("continuas_{i}.png"),
    plot = p,
    # device = svg,
    device = png,
    units = "cm",
    # units = "px",
    width = 29.7,
    height = 21
    # width = 1920,
    # height = 1080
  )
}

# listas <-
names(bloques) |>
  purrr::map(grabar)

# Rehago un poco para las categoricas
#
all_pals <-
  scico::scico_palette_names(categorical = TRUE)

grabar <- function(i) {
  p <- bloque_maps(bloques[[i]])
  ggsave(
    filename = glue::glue("categoricas_{i}.png"),
    plot = p,
    # device = svg,
    device = png,
    units = "cm",
    # units = "px",
    width = 29.7,
    height = 21
    # width = 1920,
    # height = 1080
  )
}
