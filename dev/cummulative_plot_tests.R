library(ggplot2)
library(dplyr)

df <- data.frame(
  step = 1:8,
  cum_delta = cumsum(c(2.1, 1.5, 0.8, 1.2, 0.9, 1.7, 0.6, 1.1))
)

# pal <- c("#440154", "#3b528b", "#21908d", "#5dc863", "#fde725")
pal <- terrain.colors(8)
# Build per-step rectangles from 0 to the segment's top
rects <- df |>
  dplyr::mutate(
    xmin = step - 0.5,
    xmax = step + 0.5,
    ymin = 0,
    ymax = cum_delta
  )

ggplot() +
  geom_rect(
    data = rects,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = factor(step)),
    color = NA
  ) +
  geom_line(data = df, aes(x = step, y = cum_delta), size = 1.2, color = "black") +
  scale_fill_manual(values = pal) +
  labs(
    x = "Palette step", y = "Cumulative ΔE2000",
    title = "Cumulative ΔE2000 with discrete palette blocks"
  ) +
  theme_minimal(base_size = 14) +
  guides(fill = "none")

jetpal <- as_colormap(jet_colors(256))
jetplot <- ggplot(
  data.frame(i = jetpal$index(), delta = jetpal$cum_deltas()),
  aes(x = i, y = delta)
)
jetplot + geom_line(aes(x = i, y = delta))
jetplot + geom_dotplot(binwidth = 1, aes(x = delta, fill = delta))
jetplot + geom_col(width = 1, aes(fill = delta)) +
  scale_fill_gradientn(colors = jet_colors(256)) +
  geom_line(linewidth = 1) +
  # theme_classic()
  # theme_light()
  theme_minimal() +
  labs(
    x = "Palette step", y = "Cumulative ΔE2000",
    title = "Cumulative ΔE2000 with discrete palette blocks"
  )
