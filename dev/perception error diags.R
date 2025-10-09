# TODO:
# 1. Funciones de diagnóstico:
#   1.1 local_visual_error (en % o no): adrel_global
#   1.2 error rms (lista, documentar)
# 2. funcion perceptual_error_rms() coge ColorMaps o hex o dfs.
#    Es interesante que funcione fuera del objeto para tener felexibiliad
#    al aplicarlo. Pero al final siempre convierte primero a ColorMap
#    Lo ideal sería (con resto de funciones tambien) hacerlo método
#    y hacer un wrapper con as_colormap -> método para el uso flexible
#    y cómodo de la funcion
# 3. Plots de canales
# 4. plots de deltas/cum_deltas
#  5. Varianzas deltas, cv, rango
#  6. Revisar funcion equalizacion, que sea correcta. Comparar si es posible con
#  el método antiguo.


percl <- function(pal, medida = "cie2000") {
  pal <- as_colormap(pal)
  deltas <- pal$deltas(medida)[-1]
  n <- length(deltas)
  total <- sum(deltas)
  dideal <- total / n
  le <- abs(deltas - total / n)
  ler <- le / total * 100
  lll <- abs(deltas - dideal) / total * 100
  # cumulative perceptual distances
  cum_actual <- cumsum(deltas)
  # ideal straight line
  cum_ideal <- seq(from = total / n, by = total / n, length.out = n)

  cum_d <- cum_actual - cum_ideal
  drel_local <- cum_d / cum_ideal
  absdrel_local <- abs(cum_d / cum_ideal) / total * 100
  drel_global <- cum_d / total
  adrel_global <- abs(cum_d) / total * 100
  # RMS deviation of cumulative curve from ideal
  rms <- sqrt(mean((cum_d)^2))
  perr <- rms / total * 100
  # return(perr)
  tibble::tibble(
    index = 1:n,
    # lll = lll,
    # deltas,
    # cum_actual = cum_actual,
    # cum_ideal = cum_ideal,
    # cumd = cumd,
    # le = le,
    # ler = ler,
    # drel_local = drel_local,
    # absdrel_local = absdrel_local,
    # drel_global = drel_global,
    adrel_global = adrel_global,
    # rms = rms,
    # perr = perr
  ) |>
    tidyr::pivot_longer(
      -index,
      names_to = "variable",
      values_to = "valor"
    )
}

# perceptual_error_rms(bat)
jet <- as_colormap(jet_colors(256))
bat <- as_colormap(batlow_df)
diag <- percl(bat, "cie2000")
diag <- percl(jet, "cie2000")
diag <- percl(bat, "cie1976")
diag <- percl(jet, "cie1976")


diag |>
  ggplot2::ggplot(aes(x = index, y = valor, color = variable)) +
  ggplot2::ylim(-0.0001, 10) +
  ggplot2::geom_line()

# Tal como está este código está claro que cum_d <- cum_actual - cum_ideal,
# (con los deltasciede2000 acum) es lo de "representing a linear graph"
# Los deltas (no acumulados) son el local lightness change
#
# El visual error% puntual es adrel_global <- abs(cum_d) / total * 100
# diferencia absoluta entre cum_deltas y el acumulado_ideal, dividido por el
# total (max(cum_delta) o sum(deltas)) y * 100
#
# Confirmo que en Version5 SCM error visual coincide con adrel_global cuando lo calculo
# con cie1976

which(diag$valor == max(diag$valor))
max(diag$valor)
# [1] 3.301948
# [1] 0.2022716

testcolors <-
  data.frame(
    r = c(0, 0, 0, 0, 100, 205),
    g = c(0, 0, 100, 205, 0, 0),
    b = c(100, 205, 0, 0, 0, 0)
  )
# devtools::load_all()
testpal <- testcolors |> as_colormap()
testpal$plot_swatch()
testpal$deltas()

# Vése máis diferencia no verde. O azul ten franxa máis estreita e mais cara ao celeste
# O vermello máis cara ao amarelo.
# ;
# https://support.hunterlab.com/hc/en-us/categories/201319586-Color-Theory
#
# batpal$deltas() |> plot()
# > batpal$deltas()[-1] |> plot()
# > batpal$deltas()[-1] |> min()
# [1] 0.5267951
# > batpal$deltas()[-1] |> max()
# [1] 0.5820789
# > ecuige$deltas()[-1] |> max()
# Error: objeto 'ecuige' no encontrado
# > eqige$deltas()[-1] |> min()
# [1] 0.4500861
# > eqige$deltas()[-1] |> max()
# [1] 0.5677183
# > equalize(eqige, n = 256) -> eqige2
# Defaulting to LAB.
# Check your data if that's not correct
# > eqige2$deltas() |> plot()
# > eqige2$deltas()[-1] |> plot()
# > eqige2$deltas()[-1] |> var()
# [1] 0.0002692024
# > eqige2$deltas()[-1] |> var()
# [1] 0.0002692024
# > eqige$deltas()[-1] |> var()
# [1] 0.0004631088
# > eqige2$deltas()[-1] |> var()/mean(eqige2$deltas[-1])
# Error en eqige2$deltas[-1]: objeto de tipo 'closure' no es subconjunto
# > eqige2$deltas()[-1] |> var()/mean(eqige2$deltas()[-1])
# [1] 0.0005261545
# > eqige$deltas()[-1] |> var()/mean(eqige$deltas()[-1])
# [1] 0.0009051433
# > batpal$deltas()[-1] |> var()/mean(batpal$deltas()[-1])
# [1] 0.0001459367
