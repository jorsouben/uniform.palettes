# Attempts to convert an object to a ColorMap object if it is not already
as_colormap <- function(pal) {
  if (inherits(pal, "ColorMap")) {
    pal
  } else {
    ColorMap$new(pal)
  }
}
