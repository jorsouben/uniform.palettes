#' @describeIn ColorMap Plot a swatch of the palette
ColorMap$set("public", "swatch", function(cvd = FALSE, ...) {
  colorspace::swatchplot(self$get_hex(), cvd = cvd, ...)
})

#' @describeIn ColorMap Plot palette as bands
ColorMap$set("public", "bands", function(...) {
  pals::pal.bands(self$get_hex(), ...)
})

#' @describeIn ColorMap Plot palette as sine ramp
ColorMap$set("public", "sineramp", function(...) {
  pals::pal.sineramp(self$get_hex(), ...)
})
