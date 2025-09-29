#' `ggplot2` compatible continuous color palette
#'
#' @param pal Function, returning a vector of hex codes.
#' @param ...
#'
#' @returns `ggplot2` compatible continuous color gradient.
#' @export
scale_color_unipals_c <- function(pal = demopal, ...) {
  ggplot2::scale_color_gradientn(colors = pal(256), ...)
}

#' `ggplot2` compatible continuous fill palette
#'
#' @param pal Function, returning a vector of hex codes.
#' @param ...
#'
#' @returns `ggplot2` compatible continuous fill gradient.
#' @export
scale_fill_unipals_c <- function(pal = demopal, ...) {
  ggplot2::scale_fill_gradientn(colors = pal(256), ...)
}

# scale_fill_mipaleta_d_auto <- function(data, var, ...) {
#   n <- length(unique(data[[var]]))
#   ggplot2::scale_fill_manual(values = mipaleta(n), ...)
# }
#
# scale_color_mipaleta_d_auto <- function(data, var, ...) {
#   n <- length(unique(data[[var]]))
#   ggplot2::scale_color_manual(values = mipaleta(n), ...)
# }

#' `ggplot2` compatible discrete color palette
#'
#' @param pal Function, returning a vector of hex codes.
#' @param n Integer, number of colors to return.
#' @param ...
#'
#' @returns `ggplot2` compatible discrete color palette.
#' @export
scale_color_unipals_d <- function(n, pal = demopal, ...) {
  ggplot2::scale_color_manual(values = pal(n), ...)
}

#' `ggplot2` compatible discrete fill palette
#'
#' @param pal Function, returning a vector of hex codes.
#' @param n Integer, number of colors to return.
#' @param ...
#'
#' @returns `ggplot2` compatible discrete fill palette.
#' @export
scale_fill_unipals_d <- function(n, pal = demopal, ...) {
  ggplot2::scale_fill_manual(values = pal(n), ...)
}
