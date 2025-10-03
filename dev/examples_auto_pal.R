scale_fill_mipaleta_d_auto <- function(data, var, ...) {
  n <- length(unique(data[[var]]))
  ggplot2::scale_fill_manual(values = mipaleta(n), ...)
}

scale_color_mipaleta_d_auto <- function(data, var, ...) {
  n <- length(unique(data[[var]]))
  ggplot2::scale_color_manual(values = mipaleta(n), ...)
}
