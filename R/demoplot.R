#' A demo plot using a given palette, with the option to simulate CVDs
#'
#' @param palette A palette function that returns hex color codes.
#' @param cvd Character, optional, indicates the color deficiency to emulate.
#'  Can be 'protan', 'tritan', 'deutan', or 'desaturate'.
#'
#' @return A grid of plots demoing the palette.
#'
#' @export
demoplot <- function(pal = demopal, cvd = NULL) {
  # --- CVD correction ---
  allowed_cvd <- c("tritan", "deutan", "protan", "desaturate")

  if (!is.null(cvd)) {
    if (!cvd %in% allowed_cvd) {
      stop("Invalid 'cvd' value. Must be 'NULL' or one of:
  'tritan', 'deutan', 'protan' or 'desaturate'.")
    }
    cvd_fun <- get(cvd, envir = asNamespace("colorspace"))
    pal_og <- pal
    pal <- function(n) {
      cvd_fun(pal_og(n))
    }
  }

  # --- Plots ---
  set.seed(123)
  # --- 1. Bar plot ---
  bar_data <- data.frame(
    category = LETTERS[1:8],
    value = sample(10:50, 8)
  )

  p1 <- ggplot(bar_data, aes(x = category, y = value, fill = category)) +
    geom_col() +
    scale_fill_unipals_d(n = nrow(bar_data), pal) +
    # scale_fill_brewer(palette = "Set2") +
    theme_minimal() +
    labs(title = "Bar Plot")

  # --- 2. Map plot ---
  map_path <-
    system.file(
      "extdata/map_gal/Concellos_IGN.shp",
      package = "unipals"
    )

  map_data <- st_read(map_path, quiet = TRUE)

  # Add fictitious values to polygons
  map_data <- map_data %>%
    mutate(value = runif(n(), 0, 100))

  p2 <- ggplot(map_data) +
    geom_sf(aes(fill = value)) +
    # scale_fill_viridis_c(option = "C") +
    scale_fill_unipals_c(pal) +
    theme_minimal() +
    labs(title = "Map Plot")

  # --- 3. Pie plot ---
  pie_data <- data.frame(
    group = LETTERS[1:6],
    value = c(5, 17, 30, 40, 10, 15)
  )

  p3 <- ggplot(pie_data, aes(x = "", y = value, fill = group)) +
    geom_col(width = 1, color = "white") +
    coord_polar(theta = "y") +
    scale_fill_unipals_d(n = nrow(pie_data), pal) +
    theme_void() +
    labs(title = "Pie Plot")

  # --- 4. Series plot (time series) ---
  n_series <- 4
  ts_data <- data.frame(
    time = rep(1:20, times = n_series),
    value = cumsum(rnorm(20 * n_series)),
    serie = rep(letters[1:n_series], each = 20)
  )

  p4 <- ggplot(ts_data, aes(x = time, y = value, color = serie)) +
    geom_line() +
    scale_color_unipals_d(n = n_series, pal) +
    theme_minimal() +
    labs(title = "Series Plot")

  # --- Arrange in 2x2 grid ---
  (p1 | p2) /
    (p3 | p4)
}
