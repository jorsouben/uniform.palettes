#' Plot the volcano dataset in 3D using plotly
#'
#' @param pal Vector of hex colour codes, matrix or data.frame
#'   Defaults to Scientific Colour Maps: "bamako"
#' @param show_contours Logical, whether to show contour lines (default = TRUE)
#' @param pal_rescale Logical, whether to rescale heights based on perceptual
#'   differences in the palette (default = FALSE)
#' @param z_multiplier Numerical, multiplier to apply to the height to magnify
#'   or decrease the perceived height.
#' @param signed_deltas Logical, whether to use or not Luminance sign in the
#'   simulation of the perceived error.
#' @param bw Logical, indicates if the palette should be ignored and plot in
#'  grayscale.
#' @return A plotly 3D surface plot object
#' @import plotly
#' @export
volcano_3dplot <- function(
    pal = scico::scico(256, palette = "batlow"),
    show_contours = TRUE,
    pal_rescale = TRUE,
    z_multiplier = 1L,
    signed_deltas = TRUE,
    bw = pal_rescale) {
  # Extract hex vector if ColorMap object is passed
  if (!inherits(pal, "ColorMap")) {
    pal <- pal |> as_colormap()
  }
  volcano <- datasets::volcano

  # Create sequence for x and y coordinates in meters (10m grid)
  x_seq <- (seq_len(ncol(volcano)) - 0.5) * 10
  y_seq <- (seq_len(nrow(volcano)) - 0.5) * 10

  # Prepare Z values
  z_values <- if (pal_rescale) {
    rescale_to_pal(volcano, pal, L_direction = signed_deltas)
  } else {
    volcano
  }

  if (bw) {
    plot_pal <-
      scico::scico(n = length(pal$index()), palette = "bamako")
  } else {
    plot_pal <- pal$get_hex()
  }
  # Create the plot with contours included
  p <- plotly::plot_ly(
    type = "surface",
    x = ~x_seq,
    y = ~y_seq,
    z = ~z_values,
    # colorsale = colorscale,
    colors = plot_pal,
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

  # # Normalize so largest horizontal dimension is 1
  max_xy <- max(x_range, y_range)
  x_scale <- x_range / max_xy
  y_scale <- y_range / max_xy
  # # Make vertical scale
  z_scale <- z_range * z_multiplier / max_xy

  # Configure layout
  p |> plotly::layout(
    scene = list(
      width = 960,
      height = 540,
      autosize = FALSE,
      camera = list(
        eye = list(x = 0.85, y = 0.55, z = 0.35),
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
      ),
      yaxis = list(
        title = "Y (meters)" # ,
      ),
      zaxis = list(
        title = "Elevation (meters)" # ,
      )
    ),
    margin = list(l = 10, r = 5, b = 0, t = 30),
    title = "Volcano Dataset 3D Visualization"
  )
}
