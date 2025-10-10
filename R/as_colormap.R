#' Attempts to convert an object to a ColorMap object if it is not already
#'
#' @param pal hex color code vector, matrix or data.frame, or ColorMap object.
#'
#' @returns a `ColorMap` object
#' @export
#'
as_colormap <- function(pal) {
  if (inherits(pal, "ColorMap")) {
    pal
  } else {
    ColorMap$new(pal)
  }
}
