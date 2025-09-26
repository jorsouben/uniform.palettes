#' Rescale the 'volcano' dataset to reflect the perceptual error induced
#' by the given palette
#'
#' @param color_palette Character vector of hex colors for the surface gradient.
#'   Defaults to "terra" palette which is good for terrain visualization.
#' @param show_contours Logical, whether to show contour lines (default = TRUE)
#' @return A plotly 3D surface plot object
#' @import plotly
#' @importFrom magrittr %>%
#' @importFrom datasets volcano
#' @export
#' Rescale values based on perceptual differences in a color palette
#'
#' @param values Numeric vector of values to rescale
#' @param palette Character vector of hex colors
#' @return A numeric vector of rescaled values
#' @importFrom stats approx
rescale_to_pal <- function(values, palette) {
  # Get palette info with CIEDE2000 differences
  pal_info <- hex2palette_info(palette)

  # Calculate cumulative differences
  cum_diffs <- pal_info$cum_delta_2000
  # cum_diffs <- seq_len(length(palette))

  # Normalize input values to [0,1]
  vals_norm <- (values - min(values)) / diff(range(values))

  # Create interpolation function
  height_factors <- approx(
    x = seq(0, 1, length.out = length(palette)),
    y = cum_diffs / max(cum_diffs),
    xout = vals_norm
  )$y

  hf_matrix <- matrix(
    height_factors,
    nrow = nrow(values)
  )

  # return(hf_matrix)
  # Apply the height factors to original scale
  values_range <- diff(range(values))
  values_min <- min(values)
  values_min + hf_matrix * values_range
}

#' Plot the volcano dataset in 3D using plotly
#'
#' @param color_palette Character vector of hex colors for the surface gradient.
#'   Defaults to "terra" palette which is good for terrain visualization.
#' @param show_contours Logical, whether to show contour lines (default = TRUE)
#' @param pal_rescale Logical, whether to rescale heights based on perceptual
#'   differences in the palette (default = FALSE)
#' @param z_multiplier Numerical, multiplier to apply to the height to magnify
#'   or decrease the perceived height.
#' @param bw Logical, indicates if the palette should be ignored and plot in grayscale.
#' @return A plotly 3D surface plot object
#' @import plotly
#' @importFrom datasets volcano
#' @importFrom grDevices hcl.colors
#' @export
plot_volcano_3d <- function(color_palette = hcl.colors(n = 256, palette = "terrain"),
                            show_contours = TRUE,
                            pal_rescale = FALSE,
                            z_multiplier = 1L,
                            bw = FALSE) {
  # Create sequence for x and y coordinates in meters (10m grid)
  x_seq <- (seq_len(ncol(volcano)) - 0.5) * 10
  y_seq <- (seq_len(nrow(volcano)) - 0.5) * 10

  # Prepare Z values
  z_values <- if (pal_rescale) {
    rescale_to_pal(volcano, color_palette)
  } else {
    volcano
  }

  if (bw) {
    color_palette <-
      scico::scico(n = length(color_palette), palette = "grayC")
  }
  # Create colorscale in plotly format
  # colorscale <- lapply(seq_along(color_palette), function(i) {
  #   list(
  #     (i - 1) / (length(color_palette) - 1),
  #     color_palette[i]
  #   )
  # })

  # Create the plot with contours included
  p <- plotly::plot_ly(
    type = "surface",
    x = ~x_seq,
    y = ~y_seq,
    z = ~z_values,
    # colorsale = colorscale,
    colors = color_palette,
    showscale = TRUE,
    contours = if (show_contours) {
      list(
        z = list(
          show = TRUE,
          usecolormap = TRUE,
          highlightcolor = "#ffffff",
          width = 8,
          project = list(z = TRUE)
        )
      )
    }
  )

  # Scale factors for more realistic terrain visualization
  # Volcano grid is 87x61, heights range ~100-200
  x_range <- diff(range(x_seq))
  y_range <- diff(range(y_seq))
  z_range <- diff(range(volcano))

  print(glue::glue("Rangos:\nx:{x_range}\ny:{y_range}\nz:{z_range}"))

  # # Normalize so largest horizontal dimension is 1
  max_xy <- max(x_range, y_range)
  x_scale <- x_range / max_xy
  y_scale <- y_range / max_xy
  # # Make vertical scale
  z_scale <- z_range * z_multiplier / max_xy

  print(glue::glue("Escalas:\nx:{x_scale}\ny:{y_scale}\nz:{z_scale}"))
  # Configure layout
  p |> plotly::layout(
    scene = list(
      width = 960,
      height = 540,
      # autosize = TRUE,
      autosize = FALSE,
      camera = list(
        eye = list(x = 0.85, y = 0.55, z = 0.35),
        # eye = list(x = 1.5, y = 0, z = 0),
        # up = list(x = 0, y = 0, z = 1),
        # eye = list(x = 0.87 * x_scale, y = 0.87 * y_scale, z = 0.87),
        center = list(x = 0, y = 0, z = -0.2)
      ),
      aspectmode = "manual",
      aspectratio = list(
        x = x_scale,
        y = y_scale,
        z = z_scale
      ),
      xaxis = list(
        title = "X (meters)" # ,
        # range = c(min(x_seq), max(x_seq))
      ),
      yaxis = list(
        title = "Y (meters)" # ,
        # range = c(min(y_seq), max(y_seq))
      ),
      zaxis = list(
        title = "Elevation (meters)" # ,
        # range = c(min(z_values), max(z_values))
      )
    ),
    margin = list(l = 10, r = 5, b = 0, t = 30),
    # margin = list(t = 40),
    title = "Volcano Dataset 3D Visualization"
  )
}
