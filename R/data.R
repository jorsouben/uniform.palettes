#' Batlow Scientific Color Palette
#'
#' A perceptually uniform sequential color palette from the Scientific Color Maps
#' collection, created by Fabio Crameri.
#'
#' @format A data frame with color values:
#' \describe{
#'   \item{r}{Red channel value (0-255)}
#'   \item{g}{Green channel value (0-255)}
#'   \item{b}{Blue channel value (0-255)}
#' }
#' @source \url{https://www.fabiocrameri.ch/colourmaps/}
"batlow_df"

#' IGEPAL Example Color Palette
#'
#' A named vector of HEX color codes representing a simple categorical color palette.
#'
#' @format A character vector with 4 named HEX color codes:
#' \describe{
#'   \item{PANTONE 186 C}{#C8102E}
#'   \item{PANTONE 7461 C}{#007DBA}
#'   \item{PANTONE Yellow}{#FEDD00}
#' }
"igepal_hex"

#' Example Map Data
#'
#' This is a shapefile included in the package under `inst/extdata/map/`.
#' It contains a map of the municipalities of Galicia for demonstration purposes.
#'
#' To load it:
#' ```
#' map_path <- system.file("extdata/map_gal/Concellos_IGN.shp", package = "unipals")
#' sf::st_read(map_path)
#' ```
#'
#' @format A shapefile with associated `.dbf`, `.shx`, and `.prj` files.
#' @source © Instituto Geográfico Nacional
#' @name mapa_galicia
#' @docType data
NULL
