# Demo all scico pals (not in categorical variant)
library(ggplot2)
library(patchwork)

all_pals <-
  scico::scico_palette_names()

scico_pal_info <- tibble::tibble(name = all_pals)

scipal <- function(pal = "batlow") {
  function(n) scico::scico(n, palette = pal)
}

plot_n_save <- function(paleta) {
  pl <- demoplot(scipal(paleta))
  ggsave(
    filename = glue::glue("{paleta}.png"),
    plot = pl,
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

all_pals |>
  purrr::map(plot_n_save)
