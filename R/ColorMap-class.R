#' ColorMap R6 Class
#'
#' An R6 class for storing and converting color palettes across
#' multiple color spaces (HEX, RGB, LAB). Provides utilities for
#' distance computation and visualization.
#'
#' @docType class
#' @name ColorMap
#' @export
#'
#' @section Initialization:
#' \preformatted{
#' ColorMap$new(colors, space = NULL)
#' }
#' Creates a new ColorMap object. If `space` is not provided,
#' the constructor attempts to autodetect the color space:
#' \itemize{
#'   \item Character vector of valid hex codes → `"hex"`
#'   \item Numeric matrix/data.frame with 3 columns in `[0,1]` or `[0,255]` → `"rgb"`
#'   \item Otherwise defaults to `"lab"`
#' }
#'
#' @param colors Character vector of hex codes, or numeric matrix/data.frame
#'   with 3 columns representing RGB or LAB values.
#' @param space Optional. One of `"hex"`, `"rgb"`, or `"lab"`. If `NULL`,
#'   autodetection is performed.
#'
#' @section Fields:
#' \describe{
#'   \item{hex}{Private. Character vector of hex codes.}
#'   \item{rgb}{Private. Numeric matrix of RGB values.}
#'   \item{lab}{Private. Numeric matrix of LAB values.}
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{
#'     \code{get_hex()},
#'     \code{get_rgb()},
#'     \code{get_lab()}
#'   }{Retrieve color values in different spaces}
#'   \item{
#'     \code{match_hex()},
#'     \code{index()}
#'   }{Color indexing utilities}
#'   \item{
#'     \code{ciede_matrix()},
#'     \code{deltas()},
#'     \code{cum_deltas()},
#'     \code{L_deltas()}
#'     \code{perceptual_error()}
#'   }{Distance-related utilities}
#'   \item{
#'     \code{swatch()},
#'     \code{bands()},
#'     \code{sineramp()}
#'   }{Palette visualization}
#' }
#'
#' @examples
#' # From hex codes
#' pal <- ColorMap$new(c("#FF0000", "#00FF00", "#0000FF"))
#' pal$get_rgb()
#'
#' # From RGB matrix
#' rgb_mat <- matrix(c(255, 0, 0, 0, 255, 0, 0, 0, 255), ncol = 3, byrow = TRUE)
#' pal2 <- ColorMap$new(rgb_mat, space = "rgb")
#' pal2$get_lab()
#'
ColorMap <- R6::R6Class("ColorMap",
  private = list(
    hex = NULL,
    rgb = NULL,
    lab = NULL
  ),
  public = list(
    initialize = function(colors, space = NULL) {
      if (is.null(space)) {
        if (is.character(colors) && all(grepl("^#", colors))) {
          h_regex <- "^#[0-9a-fA-F]{6}$"
          if (all(grepl(h_regex, colors))) {
            space <- "hex"
          } else {
            stop(
              "Invalid hex codes: ",
              paste(colors[!grepl(h_regex, colors)], collapse = ", ")
            )
          }
        } else if (
          (is.matrix(colors) || is.data.frame(colors)) &&
            ncol(colors) == 3
        ) {
          if (all(colors >= 0) && all(colors <= 1)) {
            colors <- colors * 255
            space <- "rgb"
          } else if (all(colors >= 0) && all(colors <= 255)) {
            space <- "rgb"
          } else {
            space <- "lab"
          }
        } else {
          stop("Input must be hex vector, or numeric matrix/data.frame with 3 columns.")
        }
      }

      if (space == "hex") {
        private$hex <- toupper(colors)
      } else if (space == "rgb") {
        private$rgb <- as.matrix(colors)
        colnames(private$rgb) <- c("r", "g", "b")
      } else if (space == "lab") {
        private$lab <- as.matrix(colors)
        colnames(private$lab) <- c("l", "a", "b")
      } else {
        stop("Unsupported color space. Use 'hex', 'rgb', or 'lab'.")
      }
    },
    # By-color-space values retrieval
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
          private$rgb <- farver::convert_colour(private$lab, from = "lab", to = "rgb")
        }
      }
      private$rgb
    },
    get_lab = function() {
      if (is.null(private$lab)) {
        private$lab <- farver::convert_colour(self$get_rgb(), from = "rgb", to = "lab")
      }
      private$lab
    },
    # Utilities: search

    #' @description Find color index by hex value
    #' @param hex_value hex color code to search for
    #' @return position of the value in the colour map or NA
    match_hex = function(hex_value) {
      match(toupper(hex_value), self$get_hex())
    },
    #' @description Get the full index of colours
    index = function() {
      seq_along(self$get_hex())
    },

    # Utilities: distances

    #' @description Compute pairwise CIEDE2000 distances
    #' @param method Distance metric, passed to [farver::compare_colour()].
    ciede_matrix = function(method = "cie2000") {
      lab_vals <- self$get_lab()
      farver::compare_colour(lab_vals, from_space = "lab", method = method)
    },
    #' @description Return sequential CIEDE2000 deltas
    #' @param method Distance metric, passed to [unipals::ciede_matrix()].
    deltas = function(method = "cie2000") {
      mat <- self$ciede_matrix(method)
      if (nrow(mat) <= 1) stop("Can't compute deltas for a single colour")
      c(0, mat[cbind(1:(nrow(mat) - 1), 2:ncol(mat))])
    },
    #' @description Return cumulative deltas
    #' @param method Distance metric, passed to [unipals::deltas()].
    cum_deltas = function(method = "cie2000") {
      cumsum(self$deltas(method))
    },
    #' @description Return the deltas of the Lightness channel
    L_deltas = function() {
      c(0, diff(self$get_lab()[, "l"]))
    },
    #' @description Calculate RMS visual error
    #' Calculates the square root of the mean of the square
    #' deviations from the ideal cummulative deltas
    perceptual_error = function() {
      deltas <- self$deltas()[-1]
      n <- length(deltas)
      # cumulative perceptual distances
      cum_actual <- self$cum_deltas()[-1]
      total <- tail(cum_actual, 1L)
      # ideal straight line
      cum_ideal <- seq(from = total / n, by = total / n, length.out = n)
      # RMS deviation of cumulative curve from ideal
      rms <- sqrt(mean((cum_actual - cum_ideal)^2))
      perr <- rms / total * 100
      return(perr)
    },

    # Utilities: Swatch plots

    #' @description Plot a swatch of the palette
    #' @param cvd Logical. If TRUE, apply color-vision-deficiency simulation.
    #' @param ... Additional arguments passed to [colorspace::swatchplot()].
    swatch = function(cvd = FALSE, ...) {
      colorspace::swatchplot(self$get_hex(), cvd = cvd, ...)
    },
    #' @description Plot palette as bands
    #' @param ... Additional arguments passed to [pals::pal.bands()].
    bands = function(...) {
      pals::pal.bands(self$get_hex(), ...)
    },
    #' @description Plot palette as sine ramp
    #' @param ... Additional arguments passed to [pals::pal.sineramp()].
    sineramp = function(...) {
      pals::pal.bands(self$get_hex(), ...)
    }
  )
)
