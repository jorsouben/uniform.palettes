borrador <- c(
  gris_carbon = "#2E2E2E",
  azul_oscuro = "#004F7A",
  azul_xunta = "#007BC4",
  azul_claro = "#4DB8E8",
  verde_oceano = "#2AA876",
  coral_profundo = "#C3422F",
  dorado = "#B57F03",
  gris_piedra = "#D0D6DC",
  gris_niebla = "#F5F7FA"
)


borrador |> pals::pal.bands()

borrador |>
  hex2palette_info() |>
  View()

borradorpal <- function(n) {
  extend_palette_equal_ciede2000(
    borrador,
    n,
    fixed_hex = "#007BC4",
    mode = "anchor_exact"
  )$hex
}

demoplot(borradorpal, cvd = "protan")

scipal <- function(pal = "batlow") {
  function(n) scico::scico(n, palette = pal)
}

scipal

demoplot(scipal("hawaii"))
demoplot(scipal("navia"))
demoplot(scipal("managua"))


demoplot(demopal)

scico::scico_palette_show()


mono_gradient(24)$hex |> pals::pal.bands()
