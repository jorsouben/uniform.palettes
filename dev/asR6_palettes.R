library(R6)
library(colorspace)
library(farver)
library(pals)

ColorPalette <- R6Class("ColorPalette",
  private = list(
    hex = NULL,
    rgb = NULL,
    lab = NULL
  ),
  public = list(
    initialize = function(colors, space = NULL) {
      if (is.null(space)) {
        # Autodetection of origin color space
        # Trying HEX
        if (is.character(colors) && all(grepl("^#", colors))) {
          h_regex <- "^#[0-9|a-f|A-F]{6}$"
          if (all(grepl(h_regex, colors))) {
            message("Detected valid hex codes vector")
            space <- "hex"
          } else {
            stop(
              "Detected erroneous hex color codes: \n",
              colors[!grepl(h_regex, colors)]
            )
          }
          # Trying RGB and LAB
        } else if (
          (is.matrix(colors) || is.data.frame(colors)) &&
            ncol(colors) == 3
        ) {
          if (all(colors >= 0) && all(colors <= 1)) {
            colors <- colors * 255
            space <- "rgb"
            message("Detected RGB values in [0-1] range")
          } else if (all(colors >= 0) && all(colors <= 255)) {
            space <- "rgb"
            message("Detected RGB values in [0-255] range")
          } else {
            message(
              "Defaulting to LAB.\n",
              "Check your data if that's not correct"
            )
            space <- "lab"
          }
        } else {
          stop("Input must be an hex code vector, numeric matrix or df.")
        }
      }

      if (space == "hex") {
        private$hex <- toupper(colors)
      } else if (space == "rgb") {
        private$rgb <- colors
      } else if (space == "lab") {
        private$lab <- colors
      } else {
        stop("Unsupported color space. Use 'hex', 'rgb', or 'lab'.")
      }
    },
    get_hex = function() {
      if (is.null(private$hex)) {
        private$hex <- farver::encode_colour(self$get_rgb())
      }
      private$hex
    },
    get_rgb = function() {
      if (is.null(private$rgb)) {
        if (!is.null(private$hex)) {
          private$rgb <- farver::decode_colour(private$hex)
        } else if (!is.null(private$lab)) {
          private$rgb <- convert_colour(private$lab, from = "lab", to = "rgb")
        }
      }
      private$rgb
    },
    get_lab = function() {
      if (is.null(private$lab)) {
        private$lab <- convert_colour(self$get_rgb(), from = "rgb", to = "lab")
      }
      private$lab
    },
    ciede2000_matrix = function() {
      lab_vals <- self$get_lab()
      farver::compare_colour(lab_vals, from_space = "lab", method = "CIE2000")
    },
    plot_swatch = function(cvd = FALSE) {
      # plot_swatch = function() {
      title(main = "My Custom Title")
      # colorspace::swatchplot(self$get_hex())
      colorspace::swatchplot(self$get_hex(), cvd = cvd)
    },
    plot_sineramp = function() {
      pals::pal.sineramp(self$get_hex(), main = "Sine Ramp")
    },
    plot_colorspace_hcl = function() {
      colorspace::hclplot(self$get_hex(), main = "HCL Plot")
    }
  )
)


# Exemplos
#
palbase <- c("#aF0000", "#00fF00", "#0d7cFF")
# From hex
# palette1 <- ColorPalette$new(c("#FF0000", "#00FF00", "#0000FF"))
palette1 <- ColorPalette$new(c("#aF0000", "#00fF00", "#0d7cFF"))
# palette1$plot_swatch()
# palette1$plot_sineramp()
# palette1$ciede2000_distances()

# From RGB
rgb_matrix <- matrix(c(
  255, 0, 0,
  0, 255, 0,
  0, 0, 255
), ncol = 3, byrow = TRUE)
palette2 <- ColorPalette$new(rgb_matrix)
# palette2 <- ColorPalette$new(rgb_matrix, space = "rgb")
# palette2$plot_colorspace_hcl()

rgb_matrixb <- matrix(c(
  0.1, 0, 0,
  0, 0.5, 0,
  0, 0, 1
), ncol = 3, byrow = TRUE)
palette2b <- ColorPalette$new(rgb_matrixb)
# From Lab
lab_matrix <- matrix(c(
  53.2, 80.1, 67.2,
  87.7, -86.2, 83.2,
  32.3, 79.2, -107.9
), ncol = 3, byrow = TRUE)
# palette3 <- ColorPalette$new(lab_matrix, space = "lab")
palette3 <- ColorPalette$new(lab_matrix)
palette3$plot_swatch()

palette1$get_lab()
palette1$get_hex() |>
  decode_colour() |>
  convert_colour(from = "rgb", to = "lab")
palbase |>
  hex2palette_info() |>
  dplyr::select(L, a, b) |>
  as.matrix()
palbase |> hex2palette_info()
rgb_matrixb |>
  convert_colour(from = "rgb", to = "lab")
palette1$ciede2000_distances()
rgb_matrixb |> encode_colour()

mipal <- ColorPalette$new(igepal_hex)
mipal$get_lab()
mipal$plot_colorspace_hcl()
mipal$plot_swatch()
mipal$plot_sineramp()
mipal$ciede2000_matrix()
