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
[1] 3.301948
[1] 0.2022716
