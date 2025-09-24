# Load required libraries
library(scico)
library(purrr)
library(tibble)
library(openxlsx)

# Step 1: Generate the list of categorical palette names
palette_names <- scico_palette_names(categorical = TRUE)

npals <- length(palette_names)
ncolors <- 10

# Step 2: Generate a list of palettes (each is a vector of 10 hex colors)
palettes <- map(palette_names, ~ scico(n = ncolors, palette = .x))

names(palettes) <- palette_names

# # Step 3: Create a tibble with columns: name, color_01, ..., color_10
# palette_tbl <- tibble(
#   name = palette_names,
#   !!!map(1:10, ~ set_names(map(palettes, ~ .x[.]), paste0("color_", sprintf("%02d", .))))
# )

taboa_cores <-
  palettes |> as_tibble()

# Step 4: Create a new workbook and add the data
wb <- createWorkbook()
addWorksheet(wb, "Paletas")
writeData(wb, "Paletas", taboa_cores)

# Step 5: Apply background color formatting to each color cell
for (i in 1:ncolors) {
  for (j in 1:npals) {
    hex <- taboa_cores[[j]][i]
    if (grepl("^#[0-9A-Fa-f]{6}$", hex)) {
      style <- createStyle(fgFill = hex, halign = "left", valign = "bottom", textDecoration = "bold")
      addStyle(wb, sheet = "Paletas", style = style, rows = i + 1, cols = j, gridExpand = FALSE)
    }
  }
}


# Estilo para encabezados: centrado, negrita, fuente más grande
header_style <- createStyle(
  fontSize = 12,
  textDecoration = "bold",
  halign = "center",
  valign = "center"
)
addStyle(wb, sheet = "Paletas", style = header_style, rows = 1, cols = 1:npals, gridExpand = TRUE)



# Ajustar tamaño de filas y columnas
setColWidths(wb, sheet = "Paletas", cols = 1:npals, widths = 15) # ancho de columnas
setRowHeights(wb, sheet = "Paletas", rows = 2:(ncolors + 1), heights = 40) # alto de filas


# openXL(wb)
# Guardar archivo

# Step 6: Save the workbook
saveWorkbook(wb, "paletas_scico_categoricas.xlsx", overwrite = TRUE)
