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
#'   \item{\code{get_hex()}}{Return hex codes, converting from RGB or LAB if needed.}
#'   \item{\code{get_rgb()}}{Return RGB values, converting from HEX or LAB if needed.}
#'   \item{\code{get_lab()}}{Return LAB values, converting from RGB if needed.}
#'   \item{\code{ciede2000_matrix()}}{Compute pairwise CIEDE2000 distances between colors.}
#'   \item{\code{deltas()}}{Return sequential CIEDE2000 deltas along the palette.}
#'   \item{\code{cum_deltas()}}{Return cummulative CIEDE2000 deltas along the palette.}
#'   \item{\code{plot_swatch(cvd = FALSE)}}{Plot a swatch of the palette using \pkg{colorspace}.}
#'   \item{\code{plot_sineramp()}}{Plot a sine-ramp visualization using \pkg{pals}.}
#'   \item{\code{plot_colorspace_hcl()}}{Plot the palette in HCL space using \pkg{colorspace}.}
#' }
#'
#' @examples
#' # From hex codes
#' pal <- ColorMap$new(c("#FF0000", "#00FF00", "#0000FF"))
#' pal$get_rgb()
#' pal$plot_swatch()
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
        # private$rgb <- setNames(as.data.frame(colors), nm = c("r", "g", "b"))
        private$rgb <- as.matrix(colors)
        colnames(private$rgb) <- c("r", "g", "b")
      } else if (space == "lab") {
        # private$lab <- setNames(as.data.frame(colors), nm = c("l", "a", "b"))
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
    # Utilities
    match_hex = function(hex_value) {
      match(toupper(hex_value), self$get_hex())
    },
    index = function() {
      seq_along(self$get_hex())
    },
    ciede_matrix = function(method = "cie2000") {
      lab_vals <- self$get_lab()
      farver::compare_colour(lab_vals, from_space = "lab", method = method)
      # farver::compare_colour(lab_vals, from_space = "lab", method = "cie1976")
    },
    deltas = function(method = "cie2000") {
      mat <- self$ciede_matrix(method)
      if (nrow(mat) <= 1) stop("Can't compute deltas for a single colour")
      c(0, mat[cbind(1:(nrow(mat) - 1), 2:ncol(mat))])
    },
    cum_deltas = function(method = "cie2000") {
      cumsum(self$deltas(method))
    },
    L_diff = function() {
      c(0, diff(self$get_lab()[, "l"]))
    },
    swatch = function(cvd = FALSE, ...) {
      colorspace::swatchplot(self$get_hex(), cvd = cvd, ...)
    },
    bands = function(...) {
      pals::pal.bands(self$get_hex(), ...)
    },
    sineramp = function(...) {
      pals::pal.bands(self$get_hex(), ...)
    } # ,
    # plot_colorspace_hcl = function() {
    #   colorspace::hclplot(self$get_hex(), main = "HCL Plot")
    # }
  )
)
