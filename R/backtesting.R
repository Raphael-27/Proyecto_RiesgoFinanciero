# Backtesting: ¿el sistema habría funcionado en las crisis financieras históricas?
#
# 1. Backtesting del VaR (Kupiec, Christoffersen y semáforo de Basilea): un VaR al 99 % debe ser superado
#    ~1 % de los días, y esas superaciones no deben agruparse. Se evalúa en toda la muestra y dentro de
#    cada crisis (2008, 2020).
# 2. Backtesting de los modelos en crisis: se entrena SOLO con datos anteriores a la crisis (el modelo
#    nunca la vio), se predice durante la crisis y se mide si las alertas llegaron a tiempo.
# 3. Incertidumbre: intervalos de confianza por bootstrap por bloques (las etiquetas a 5 días se solapan,
#    así que las observaciones no son independientes) para saber si el modelo supera de verdad a la
#    línea base o si la diferencia podría ser ruido.

# ------------------------------------------------------------------------ pruebas estadísticas
xlogy <- function(x, y) ifelse(x == 0, 0, x * log(y))      # x * log(y) con la convención 0 * log(0) = 0

#' Prueba de cobertura incondicional de Kupiec (POF): ¿la tasa de violaciones x/n coincide con p?
kupiec_pof <- function(x, n, p) {
  if (n == 0) return(c(lr = NA_real_, p_valor = NA_real_))
  ph <- x / n
  lr <- -2 * (((n - x) * log(1 - p) + xlogy(x, p)) - (xlogy(n - x, 1 - ph) + xlogy(x, ph)))
  c(lr = max(lr, 0), p_valor = stats::pchisq(max(lr, 0), 1, lower.tail = FALSE))
}

#' Prueba de independencia de Christoffersen: ¿las violaciones se agrupan en el tiempo?
christoffersen_ind <- function(v) {
  v <- as.integer(v)
  if (length(v) < 2) return(c(lr = NA_real_, p_valor = NA_real_))
  a <- v[-length(v)]; b <- v[-1]
  n00 <- sum(a == 0 & b == 0); n01 <- sum(a == 0 & b == 1); n10 <- sum(a == 1 & b == 0); n11 <- sum(a == 1 & b == 1)
  seguro <- function(num, den) if (den == 0) 0 else num / den
  pi01 <- seguro(n01, n00 + n01); pi11 <- seguro(n11, n10 + n11); pi <- seguro(n01 + n11, n00 + n01 + n10 + n11)
  ll0 <- xlogy(n00 + n10, 1 - pi) + xlogy(n01 + n11, pi)
  ll1 <- xlogy(n00, 1 - pi01) + xlogy(n01, pi01) + xlogy(n10, 1 - pi11) + xlogy(n11, pi11)
  lr <- max(-2 * (ll0 - ll1), 0)
  c(lr = lr, p_valor = stats::pchisq(lr, 1, lower.tail = FALSE))
}

#' Semáforo de Basilea para un VaR al 99 % en 250 días: verde 0-4 violaciones, amarillo 5-9, rojo >= 10.
zona_basilea <- function(violaciones_250) {
  if (is.na(violaciones_250)) return(NA_character_)
  if (violaciones_250 <= 4) "verde" else if (violaciones_250 <= 9) "amarilla" else "roja"
}

maximo_ventana <- function(v, ancho = 250) {
  if (length(v) < ancho) return(NA_real_)
  max(zoo::rollsum(as.integer(v), ancho, align = "right"), na.rm = TRUE)
}

# ------------------------------------------------------------------------------ ventanas
parametros_backtesting <- function(cfg = NULL) {
  b <- cfg$backtesting %||% list()
  crisis <- b$crisis %||% list(
    list(nombre = "Crisis 2008", inicio = "2008-09-01", fin = "2009-06-30"),
    list(nombre = "COVID-19", inicio = "2020-02-19", fin = "2020-06-30"))
  list(niveles_var = unlist(b$niveles_var %||% c(0.95, 0.99)),
       crisis = lapply(crisis, function(c) list(nombre = c$nombre, inicio = as.Date(c$inicio), fin = as.Date(c$fin))),
       remuestreos = as.integer(b$remuestreos %||% 500), bloque_bootstrap = as.integer(b$bloque_bootstrap %||% 21),
       semilla = as.integer(b$semilla %||% 123))
}

ventanas <- function(crisis) c(list(list(nombre = "Toda la muestra", inicio = as.Date("1900-01-01"), fin = as.Date("2999-12-31"))), crisis)

# --------------------------------------------------------------------------- backtest del VaR
#' Backtesting del VaR histórico y paramétrico. El VaR calculado en t es el pronóstico para t+1: se
#' compara con el retorno realizado de t+1 (sin mirar el futuro al calcularlo).
backtest_var <- function(curada, ticker, params = parametros_backtesting()) {
  d <- curada[order(curada$fecha), , drop = FALSE]
  n <- nrow(d); r <- d$retorno_log
  r_sig <- r[-1]; fechas <- d$fecha[-1]
  filas <- list()
  for (nivel in params$niveles_var) {
    et <- as.integer(round(nivel * 100))
    for (metodo in c("hist", "param")) {
      col <- paste0("var_", metodo, "_", et)
      if (!col %in% names(d)) next
      pron <- d[[col]][-n]
      es <- if (metodo == "hist" && paste0("es_hist_", et) %in% names(d)) d[[paste0("es_hist_", et)]][-n] else rep(NA_real_, n - 1)
      valido <- !is.na(pron) & !is.na(r_sig)
      viol <- r_sig < -pron
      for (w in ventanas(params$crisis)) {
        i <- which(valido & fechas >= w$inicio & fechas <= w$fin)
        if (length(i) < 20) next
        x <- sum(viol[i]); m <- length(i)
        k <- kupiec_pof(x, m, 1 - nivel); ind <- christoffersen_ind(viol[i])
        cc_lr <- k["lr"] + ind["lr"]; cc_p <- stats::pchisq(cc_lr, 2, lower.tail = FALSE)
        ratio_es <- if (x > 0 && !all(is.na(es[i][viol[i]]))) mean(-r_sig[i][viol[i]]) / mean(es[i][viol[i]], na.rm = TRUE) else NA_real_
        filas[[length(filas) + 1]] <- data.frame(
          ticker = ticker, metodo = if (metodo == "hist") "historico" else "parametrico", nivel = nivel, ventana = w$nombre,
          n = m, violaciones = x, tasa_observada = x / m, tasa_esperada = 1 - nivel,
          kupiec_p = unname(k["p_valor"]), independencia_p = unname(ind["p_valor"]), cobertura_cond_p = unname(cc_p),
          rechaza = !is.na(cc_p) && cc_p < 0.05,
          max_violaciones_250d = if (nivel == 0.99) maximo_ventana(viol[i]) else NA_real_,
          zona_basilea = if (nivel == 0.99) zona_basilea(maximo_ventana(viol[i])) else NA_character_,
          ratio_es = ratio_es, stringsAsFactors = FALSE)
      }
    }
  }
  if (length(filas) == 0) return(NULL)
  do.call(rbind, filas)
}

# ------------------------------------------------------------------ modelos en las crisis
#' Entrena con datos ANTERIORES a la crisis (menos la purga), predice durante la crisis y mide el desempeño y
#' la anticipación de las alertas. Compara con la regla base y con la frecuencia aprendida antes de la crisis.
backtest_crisis_modelos <- function(curada, ticker, params_modelado, crisis, minimo_entrenamiento = 300L) {
  ds <- construir_dataset_modelo(curada, params_modelado)
  cols <- columnas_features(ds); n <- nrow(ds)
  filas_out <- list(); series_out <- list()

  for (w in crisis) {
    idx_ini <- which(ds$fecha >= w$inicio)[1]
    if (is.na(idx_ini)) next
    for (obj in ETIQUETAS) {
      y <- ds[[obj]]; base <- score_base(ds, obj)
      train <- which(!is.na(y) & seq_len(n) <= idx_ini - params_modelado$purga_dias - 1)
      val <- which(!is.na(y) & ds$fecha >= w$inicio & ds$fecha <= w$fin)
      if (length(train) < minimo_entrenamiento || length(val) < 20) next
      ref <- mean(y[train])                                  # frecuencia aprendida ANTES de la crisis
      peor <- val[which.min(ds$ret_fut[val])]                # día con el peor retorno a 5 días de la ventana

      candidatos <- list()
      for (tipo in params_modelado$modelos) {
        mod <- ajustar_modelo(tipo, ds, train, cols, y[train], params_modelado)
        candidatos[[tipo]] <- list(prob = predecir_modelo(mod, ds, val), umbral = mod$umbral, ajuste = mod$tipo)
      }
      ub <- umbral_f1(base[train], y[train])
      candidatos[["base"]] <- list(prob = base[val], umbral = ub, ajuste = "regla", es_base = TRUE)

      for (nombre in names(candidatos)) {
        cd <- candidatos[[nombre]]; es_base <- isTRUE(cd$es_base)
        alerta <- cd$prob >= cd$umbral
        m <- metricas_clasificacion(y[val], alerta)
        primera <- if (any(alerta, na.rm = TRUE)) ds$fecha[val][which(alerta)[1]] else as.Date(NA)
        filas_out[[length(filas_out) + 1]] <- data.frame(
          ticker = ticker, ventana = w$nombre, objetivo = obj, modelo = nombre, ajuste = cd$ajuste,
          n_entrenamiento = length(train), n_prueba = length(val), prevalencia_prueba = mean(y[val]),
          prevalencia_entrenamiento = ref, auc = auc_roc(y[val], cd$prob),
          habilidad_brier = if (es_base) NA_real_ else 1 - brier(y[val], cd$prob) / brier(y[val], rep(ref, length(val))),
          alertas = unname(m["alertas"]), precision = unname(m["precision"]), exhaustividad = unname(m["recall"]),
          primera_alerta = primera, dias_desde_inicio = as.numeric(primera - w$inicio),
          peor_dia = ds$fecha[peor], anticipacion_dias = if (!is.na(primera)) as.numeric(ds$fecha[peor] - primera) else NA_real_,
          stringsAsFactors = FALSE)
        series_out[[length(series_out) + 1]] <- data.frame(
          ticker = ticker, ventana = w$nombre, objetivo = obj, modelo = nombre, fecha = ds$fecha[val],
          valor = cd$prob, umbral = cd$umbral, y_real = y[val], stringsAsFactors = FALSE)
      }
    }
  }
  if (length(filas_out) == 0) return(NULL)
  list(resumen = do.call(rbind, filas_out), series = do.call(rbind, series_out))
}

# ----------------------------------------------------------------- incertidumbre (bootstrap)
#' Intervalos de confianza por bootstrap por BLOQUES de la métrica de cada modelo y de su diferencia con la
#' línea base. Las observaciones consecutivas comparten información (etiquetas a 5 días que se solapan), así
#' que se remuestrean bloques de días seguidos, no días sueltos.
backtest_incertidumbre <- function(pred, bloque = 21L, B = 500L, semilla = 123L) {
  pred <- pred[order(pred$fecha), , drop = FALSE]
  y <- pred$y_real; p <- pred$probabilidad; b <- pred$score_base; n <- length(y)
  if (n < 5 * bloque || length(unique(y)) < 2) return(NULL)
  set.seed(semilla)
  inicios <- seq_len(n - bloque + 1); nb <- ceiling(n / bloque)
  res <- matrix(NA_real_, B, 4, dimnames = list(NULL, c("auc", "auc_base", "dif", "hb")))
  for (i in seq_len(B)) {
    idx <- as.vector(outer(0:(bloque - 1), sample(inicios, nb, TRUE), "+"))[seq_len(n)]
    yi <- y[idx]
    a <- auc_roc(yi, p[idx]); ab <- auc_roc(yi, b[idx])
    res[i, ] <- c(a, ab, a - ab, 1 - brier(yi, p[idx]) / brier(yi, rep(mean(yi), n)))
  }
  q <- function(v) stats::quantile(v, c(0.025, 0.975), na.rm = TRUE, names = FALSE)
  m <- resumir_metricas(pred)
  data.frame(ticker = pred$ticker[1], objetivo = pred$objetivo[1], modelo = pred$modelo[1], n = n,
             auc = m$auc, auc_ic_inf = q(res[, "auc"])[1], auc_ic_sup = q(res[, "auc"])[2],
             auc_base = m$auc_base, dif_auc = m$auc - m$auc_base,
             dif_ic_inf = q(res[, "dif"])[1], dif_ic_sup = q(res[, "dif"])[2],
             habilidad_brier = m$habilidad_brier, hb_ic_inf = q(res[, "hb"])[1], hb_ic_sup = q(res[, "hb"])[2],
             supera_base = q(res[, "dif"])[1] > 0, habilidad_significativa = q(res[, "hb"])[1] > 0,
             stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------------- gráficas
#' Retornos diarios frente al VaR histórico: los puntos son los días en que la pérdida superó el VaR 99 %.
grafico_violaciones_var <- function(curada, ticker, ruta, nivel = 0.99) {
  requerir_paquetes()
  d <- curada[order(curada$fecha), , drop = FALSE]; n <- nrow(d)
  et95 <- "var_hist_95"; et99 <- paste0("var_hist_", as.integer(round(nivel * 100)))
  if (!all(c(et95, et99) %in% names(d))) return(invisible(NULL))
  dd <- data.frame(fecha = d$fecha[-1], retorno = d$retorno_log[-1], v95 = -d[[et95]][-n], v99 = -d[[et99]][-n])
  dd <- dd[!is.na(dd$v95) & !is.na(dd$retorno), , drop = FALSE]
  inicio <- min(dd$fecha); fin <- max(dd$fecha)
  viol <- dd[dd$retorno < dd$v99, , drop = FALSE]
  larga <- rbind(data.frame(fecha = dd$fecha, valor = dd$v95, serie = "var95", etiqueta = "VaR 95%"),
                 data.frame(fecha = dd$fecha, valor = dd$v99, serie = "var99", etiqueta = "VaR 99%"))
  larga$serie <- factor(larga$serie, levels = c("var95", "var99"))
  g <- ggplot2::ggplot() + franjas_crisis(inicio, fin, rotular = TRUE) +
    ggplot2::geom_point(data = dd, ggplot2::aes(fecha, retorno), color = "#c9c8c3", size = 0.35) +
    ggplot2::geom_line(data = larga, ggplot2::aes(fecha, valor, color = serie), linewidth = 0.55) +
    ggplot2::geom_point(data = viol, ggplot2::aes(fecha, retorno), shape = 21, fill = TINTA, color = SUPERFICIE, size = 1.8, stroke = 0.4) +
    ggplot2::scale_color_manual(values = c(var95 = SERIES[1], var99 = SERIES[2]), labels = c(var95 = "VaR 95%", var99 = "VaR 99%")) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1)) + escala_x(inicio, fin) +
    etiquetas_finales(larga, fin) + ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(title = sprintf("%s: retornos diarios frente al VaR histórico", ticker),
                  subtitle = sprintf("Puntos negros = días en que la pérdida superó el VaR %d%% (%d de %d días; se esperaban ~%.0f)",
                                     as.integer(round(nivel * 100)), nrow(viol), nrow(dd), nrow(dd) * (1 - nivel))) + tema_etl()
  dir.create(dirname(ruta), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(ruta, g, width = 11, height = 4.8, dpi = 140, bg = SUPERFICIE)
  invisible(ruta)
}

#' Probabilidad del modelo durante cada crisis, con su umbral de alerta y los días en que el evento ocurrió.
grafico_crisis <- function(series, resumen, ticker, ruta, objetivo = "y_vol_alta", modelo = "logistica") {
  requerir_paquetes()
  s <- series[series$ticker == ticker & series$objetivo == objetivo & series$modelo == modelo, , drop = FALSE]
  if (nrow(s) == 0) return(invisible(NULL))
  paneles <- lapply(unique(s$ventana), function(v) {
    d <- s[s$ventana == v, , drop = FALSE]; u <- d$umbral[1]
    r <- resumen[resumen$ticker == ticker & resumen$ventana == v & resumen$objetivo == objetivo & resumen$modelo == modelo, , drop = FALSE]
    d$evento <- ifelse(d$y_real == 1, 0, NA_real_)
    sub <- if (nrow(r) && !is.na(r$primera_alerta)) sprintf("Primera alerta: %s (día %d de la crisis); %d alertas", format(r$primera_alerta), as.integer(r$dias_desde_inicio), as.integer(r$alertas))
           else "El modelo no emitió alertas en esta ventana"
    g <- ggplot2::ggplot(d, ggplot2::aes(fecha, valor)) +
      ggplot2::geom_point(ggplot2::aes(y = evento), shape = "|", size = 3.2, color = SERIES[2], na.rm = TRUE) +
      ggplot2::geom_hline(yintercept = u, linetype = "dashed", color = TINTA_3, linewidth = 0.4) +
      ggplot2::geom_line(color = SERIES[1], linewidth = 0.6) +
      ggplot2::annotate("text", x = min(d$fecha), y = u, label = " umbral de alerta", hjust = 0, vjust = -0.5, size = 3, color = TINTA_2) +
      ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1), limits = c(0, 1), expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
      ggplot2::scale_x_date(date_labels = "%b %Y") + ggplot2::labs(title = v, subtitle = sub) + tema_etl()
    g
  })
  g <- patchwork::wrap_plots(paneles, nrow = 1) + patchwork::plot_annotation(
    title = sprintf("%s: probabilidad de alta volatilidad durante las crisis (modelo entrenado ANTES de cada crisis)", ticker),
    caption = "Marcas naranjas al pie = días en que el evento realmente ocurrió",
    theme = ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 12.5, color = TINTA),
                           plot.caption = ggplot2::element_text(color = TINTA_2, size = 8.5),
                           plot.background = ggplot2::element_rect(fill = SUPERFICIE, color = NA)))
  dir.create(dirname(ruta), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(ruta, g, width = 6.5 * length(paneles), height = 4.2, dpi = 140, bg = SUPERFICIE)
  invisible(ruta)
}
