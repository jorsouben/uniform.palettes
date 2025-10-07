pal <- scico::scico(n = 256, palette = "batlow")
pal <- rainbow(n = 256)
pal <- jet_colors(256)
pal <- colorRampPalette(igepal_hex2)(256)
pal <- colorRampPalette(igepal_hex, space = "Lab")(256)
pal <- unipals::extend_palette_equal_ciede2000(igepal_hex2, 256)$hex
pals::pal.bands(pal)

plot_volcano_3d(color_palette = pal, bw = TRUE)
plot_volcano_3d(color_palette = pal, pal_rescale = TRUE, bw = TRUE)
plot_volcano_3d(color_palette = pal, pal_rescale = TRUE)
plot_volcano_3d()
rescale_to_pal(volcano, pal) |> head()
volcano |> head()
87 * 61

rescale_to_pal(volcano, pal) |>
  as.matrix(nrow = nrow(volcano)) |>
  str()
c(1, 2, 4, 5) |> as.matrix(ncol = 2, dimnames = c("x", "y"))
dim(c(1, 2, 3, 4)) <- c(2, 2)
# Volcano profile passing through the crater bottom
min_pos <- which(volcano == 148, arr.ind = TRUE) |>
  as.data.frame() |>
  dplyr::filter(row %in% 20:40, col %in% 20:40)

original <- volcano[, 34]

induced <- rescale_to_pal(as.matrix(volcano[, 34]), pal) |> as.vector()

x_profile <- (seq_len(nrow(volcano)) - 0.5) * 10

profile_data <- tibble::tibble(
  x = x_profile,
  original,
  induced
) |>
  tidyr::pivot_longer(c(original, induced), names_to = "profile", values_to = "height")

profile_data |>
  ggplot2::ggplot(
    ggplot2::aes(x = x, y = height, color = profile)
  ) +
  ggplot2::geom_line()

induced <-
  rescale_to_colmap(as.matrix(volcano[, 34]), jetmap, TRUE) |> as.vector()
