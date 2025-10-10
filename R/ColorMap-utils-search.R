#' @describeIn ColorMap Find color index by hex value
ColorMap$set(
  "public",
  "match_hex",
  function(hex_value) {
    match(toupper(hex_value), self$get_hex())
  }
)

#' @describeIn ColorMap Get the full index of colours
ColorMap$set(
  "public",
  "index",
  function() {
    seq_along(self$get_hex())
  }
)
