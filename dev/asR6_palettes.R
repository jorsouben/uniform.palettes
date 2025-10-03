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
        # Autodetect
        if (is.character(colors) && all(grepl("^#", colors))) {
          space <- "hex"
        } else if ((is.matrix(colors) || is.data.frame(colors)) && ncol(colors) == 3) {
          space <- "rgb"
        } else {
          stop("Cannot autodetect color space. Please specify 'space'.")
        }
      }

      if (space == "hex") {
        private$hex <- colors
      } else if (space == "rgb") {
        private$rgb <- RGB(colors / 255) # normalize to [0,1]
      } else if (space == "lab") {
        private$lab <- LAB(colors)
      } else {
        stop("Unsupported color space. Use 'hex', 'rgb', or 'lab'.")
      }
    },
    get_hex = function() {
      if (is.null(private$hex)) {
        private$hex <- hex(private$get_rgb())
      }
      private$hex
    },
    get_rgb = function() {
      if (is.null(private$rgb)) {
        if (!is.null(private$hex)) {
          private$rgb <- hex2RGB(private$hex)
        } else if (!is.null(private$lab)) {
          private$rgb <- as(private$lab, "RGB")
        }
      }
      private$rgb
    },
    get_lab = function() {
      if (is.null(private$lab)) {
        private$lab <- as(self$get_rgb(), "LAB")
      }
      private$lab
    },
    ciede2000_distances = function() {
      lab_vals <- self$get_lab()@coords
      farver::compare_colour(lab_vals, lab_vals, from_space = "lab", method = "CIE2000")
    },
    plot_swatch = function() {
      swatch(self$get_hex(), main = "Color Swatch")
    },
    plot_sineramp = function() {
      sineramp(self$get_hex(), main = "Sine Ramp")
    },
    plot_colorspace_hcl = function() {
      hclplot(self$get_hex(), main = "HCL Plot")
    }
  )
)


# Exemplos
#
# From hex
palette1 <- ColorPalette$new(c("#FF0000", "#00FF00", "#0000FF"))
palette1$plot_swatch()
palette1$plot_sineramp()
palette1$ciede2000_distances()

# From RGB
rgb_matrix <- matrix(c(
  255, 0, 0,
  0, 255, 0,
  0, 0, 255
), ncol = 3, byrow = TRUE)
palette2 <- ColorPalette$new(rgb_matrix, space = "rgb")
palette2$plot_colorspace_hcl()

# From Lab
lab_matrix <- matrix(c(
  53.2, 80.1, 67.2,
  87.7, -86.2, 83.2,
  32.3, 79.2, -107.9
), ncol = 3, byrow = TRUE)
palette3 <- ColorPalette$new(lab_matrix, space = "lab")
palette3$plot_swatch()
