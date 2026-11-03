# Transformación: variables derivadas de riesgo (capa curated).
#
# Retornos logarítmicos, medias móviles, volatilidad histórica, Valor en Riesgo (VaR) y Déficit
# Esperado (Expected Shortfall) históricos y paramétricos, y variables normalizadas. Todas las
# variables en la fecha t usan únicamente información disponible hasta t (sin look-ahead).
# VaR y ES se expresan como pérdida positiva en fracción del valor (0.03 = 3 %).

retornos_log <- function(precio) {
  precio[!is.na(precio) & precio <= 0] <- NA
  c(NA_real_, diff(log(precio)))
}

# Aplica `f` a cada ventana móvil de `ventana` datos que termina en t; NA si faltan datos
# (se exige la ventana completa, igual que min_periods=ventana en pandas).
ventana_movil <- function(x, ventana, f, min_validos = ventana) {
  n <- length(x)
  out <- rep(NA_real_, n)
  if (n < ventana) return(out)
  for (i in ventana:n) {
    w <- x[(i - ventana + 1):i]
    if (sum(!is.na(w)) >= min_validos) out[i] <- f(w[!is.na(w)])
  }
  out
}

#' VaR y ES históricos sobre ventana móvil.
var_es_historico <- function(retornos, ventana = 250, niveles = c(0.95, 0.99)) {
  out <- list()
  for (nivel in niveles) {
    et <- as.integer(round(nivel * 100))
    out[[paste0("var_hist_", et)]] <- ventana_movil(retornos, ventana, function(w) -stats::quantile(w, 1 - nivel, names = FALSE))
    out[[paste0("es_hist_", et)]] <- ventana_movil(retornos, ventana, function(w) {
      q <- stats::quantile(w, 1 - nivel, names = FALSE)
      -mean(w[w <= q])
    })
  }
  as.data.frame(out)
}

#' VaR normal: -(media + z * desviación) con z = qnorm(1 - nivel).
var_parametrico <- function(retornos, ventana = 250, niveles = c(0.95, 0.99)) {
  mu <- ventana_movil(retornos, ventana, mean)
  sd <- ventana_movil(retornos, ventana, stats::sd)
  out <- list()
  for (nivel in niveles) out[[paste0("var_param_", as.integer(round(nivel * 100)))]] <- -(mu + stats::qnorm(1 - nivel) * sd)
  as.data.frame(out)
}

#' Estandarización con media/desviación de la ventana ANTERIOR (excluye el dato actual).
zscore_movil <- function(x, ventana = 252) {
  n <- length(x)
  minimo <- max(ventana %/% 4, 2)
  out <- rep(NA_real_, n)
  for (i in seq_len(n)[-1]) {
    w <- x[max(1L, i - ventana):(i - 1L)]
    w <- w[!is.na(w)]
    if (length(w) >= minimo && !is.na(x[i])) {
      s <- stats::sd(w)
      if (!is.na(s) && s > 0) out[i] <- (x[i] - mean(w)) / s
    }
  }
  out
}

#' Construye las variables derivadas de un activo a partir de su serie limpia.
calcular_caracteristicas <- function(df_limpio, cfg = NULL) {
  c <- cfg$caracteristicas %||% list()
  medias <- unlist(c$medias_moviles %||% c(20, 50, 200))
  vent_vol <- c$ventana_volatilidad %||% 21
  anio <- c$dias_anio %||% 252
  vent_var <- c$ventana_var %||% 250
  niveles <- unlist(c$niveles_var %||% c(0.95, 0.99))
  vent_z <- c$ventana_zscore %||% 252

  df <- df_limpio[order(df_limpio$fecha), , drop = FALSE]
  precio <- df$adj_close
  r <- retornos_log(precio)

  cols <- intersect(c("fecha", "ticker", "adj_close", "volume", "es_atipico", "error_dato", "relleno"), names(df))
  out <- df[, cols, drop = FALSE]
  out$retorno_log <- r
  for (m in medias) out[[paste0("ma_", m)]] <- ventana_movil(precio, m, mean)
  out[[paste0("vol_hist_", vent_vol)]] <- ventana_movil(r, vent_vol, stats::sd) * sqrt(anio)
  out <- cbind(out, var_es_historico(r, vent_var, niveles), var_parametrico(r, vent_var, niveles))
  out$retorno_z <- zscore_movil(r, vent_z)
  if ("volume" %in% names(df)) out$volumen_z <- zscore_movil(log1p(df$volume), vent_z)
  rownames(out) <- NULL
  out
}

#' Características + variables macro alineadas por fecha de disponibilidad.
#' `rezagos` = lista nombre -> días; si no se indica, se toma de macro.series de la configuración.
construir_curada <- function(df_limpio, macro_largo = NULL, cfg = NULL, rezagos = NULL) {
  curada <- calcular_caracteristicas(df_limpio, cfg)
  if (!is.null(macro_largo) && nrow(macro_largo) > 0) {
    if (is.null(rezagos)) {
      s <- cfg$macro$series %||% list()
      rezagos <- stats::setNames(lapply(s, function(x) x$rezago_dias %||% 1), vapply(s, function(x) x$nombre, character(1)))
    }
    curada <- alinear_macro(curada, macro_largo, rezago_dias = 1, rezagos = rezagos)
  }
  curada
}
