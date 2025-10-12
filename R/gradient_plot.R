#' Plot a curve with an area gradient fill
#'
#' @description
#' Create a bar plot of the deltas or visual error with the colors of the
#' palette using `ggplot2`, with optional rescaling and fixed y-axis.
#'
#' @param values Numeric vector
#' @param pal Vector of hex color codes
#' @param title Name of the series
#' @param max Optional numeric value to fix y-axis between 0 and max
#' @param rescale Logical. If TRUE, rescale values to `[0, 1]` but show original ticks.
#'
#' @returns NULL
#' @export
#'
gradient_plot <- function(values, pal, title, max = NULL, rescale = TRUE) {
  original_values <- values
  if (rescale) {
    rng <- range(values, na.rm = TRUE)
    values <- (values - rng[1]) / diff(rng)
  }

  data <- tibble::tibble(
    i = seq_along(values),
    values,
    original = original_values
  )

  p <- ggplot2::ggplot(data, aes(x = i, y = values)) +
    ggplot2::geom_col(width = 1, aes(fill = i)) +
    ggplot2::scale_fill_gradientn(colors = pal) +
    ggplot2::geom_line(linewidth = 0.5) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(x = "Palette index", y = title)

  if (!is.null(max)) {
    p <- p + ggplot2::scale_y_continuous(limits = c(0, max))
  } else if (rescale) {
    # Show original values as axis labels
    breaks <- scales::pretty_breaks(n = 5)(range(data$values, na.rm = TRUE))
    labels <- scales::label_number()(rng[1] + breaks * diff(rng))
    p <- p + ggplot2::scale_y_continuous(limits = c(0, 1), breaks = breaks, labels = labels)
  } else {
    p <- p + ggplot2::scale_y_continuous(limits = c(0, 1))
  }

  p
}
