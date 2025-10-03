basex <- igepal_hex[2] |>
  hex2rgb() |>
  rgb2lab()
ngrad <- 15
labmat <- matrix(
  c(
    L = seq(0, 100, 5),
    a = rep_len(basex[, "a"], 21),
    b = rep_len(basex[, "b"], 21)
  ),
  ncol = 3
)

labref <-
  colorspace::LAB(labmat)
rgbref <- as(labref, "sRGB")
# rgbref@coords |> palette_info()
#
corrected <-
  rgbref@coords |>
  tibble::as_tibble() |>
  dplyr::mutate(
    dplyr::across(
      dplyr::everything(),
      ~ dplyr::if_else(. < 0, 0, .)
    ),
    dplyr::across(
      dplyr::everything(),
      ~ dplyr::if_else(. > 1, 1, .)
    )
  )

info <-
  corrected |>
  df_rgb_prepare(
    channel_map = c(red = "R", green = "G", blue = "B")
  ) |>
  palette_info()

info$hex |> pals::pal.bands()
info$delta_2000 |> min(na.rm = TRUE)
info$delta_2000 |> max(na.rm = TRUE)
equal <-
  info$hex |> extend_palette_equal_ciede2000(21)
equal$L |> plot()
equal$delta_2000 |> plot()
equal$delta_2000 |> min(na.rm = TRUE)
equal$delta_2000 |> max(na.rm = TRUE)

equal$hex |> pals::pal.bands()

info$hex |>
  colorspace::desaturate() |>
  pals::pal.bands()
equal$hex |>
  colorspace::desaturate() |>
  pals::pal.bands()
library(tidyverse)
toplotL <- tibble(
  direct = info$L,
  equalized = equal$L
) |>
  mutate(index = row_number()) |>
  pivot_longer(
    -index,
    names_to = "pal",
    values_to = "valor"
  )

library(ggplot2)

toplotL |>
  ggplot(
    aes(x = index, y = valor, color = pal)
  ) +
  geom_line()
