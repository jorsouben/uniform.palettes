testpal <- c(
  sunset_yellow = "#FFD700",
  forest_green = "#228B22",
  ocean_blue = "#1E90FF",
  twilight_purple = "#6A5ACD",
  tropical_sun = "#FF5733"
)

testpal2 <- c(
  tropical_sun = "#FFD700",
  electric_lime = "#32CD32",
  caribbean_blue = "#00BFFF",
  radiant_orchid = "#9932CC",
  lit_coral = "#FF4500"
)

testpal3 <- c(
  tropical_sun = "#FFD700",
  electric_lime = "#32CD32",
  azul_xunta = "#007BC4",
  radiant_orchid = "#9932CC",
  lit_coral = "#FF4500"
)

# devtools::install_github("nx10/httpgd")
# "r.plot.useHttpgd": true

# 1) Global uniform spacing (anchor may shift slightly to nearest grid)
pal_global <- extend_palette_equal_ciede2000(testpal3, n = 12, mode = "global")

# 2) Anchor exact: keep "#007bc4" exactly; uniform spacing on each side
pal_anchor <- extend_palette_equal_ciede2000(testpal3,
  n = 12,
  fixed_hex = "#007bc4",
  mode = "anchor_exact"
)

pal_global$hex |> pals::pal.sineramp()
pal_anchor$hex |> pals::pal.sineramp()
pal_global$hex |> pals::pal.bands()
pal_anchor$hex |> pals::pal.bands()
