# Utilidades compartidas por los módulos del ETL.

`%||%` <- function(a, b) if (is.null(a)) b else a

# Convierte un ticker ('^GSPC', 'EUR/USD') en un nombre de carpeta válido.
seguro_para_ruta <- function(x) gsub("[^[:alnum:]_.^=-]", "_", x)

# Mediana y MAD exactos de las `ventana` observaciones ANTERIORES a cada posición (excluye la
# actual), tolerando NA. Con menos de `min_periodos` datos válidos devuelve NA.
mediana_mad_previa <- function(x, ventana, min_periodos) {
  n <- length(x)
  med <- rep(NA_real_, n)
  mad <- rep(NA_real_, n)
  for (i in seq_len(n)[-1]) {
    w <- x[max(1L, i - ventana):(i - 1L)]
    w <- w[!is.na(w)]
    if (length(w) >= min_periodos) {
      m <- stats::median(w)
      med[i] <- m
      mad[i] <- stats::median(abs(w - m))
    }
  }
  list(mediana = med, mad = mad)
}

# Propaga el último valor válido hasta `limite` posiciones consecutivas (nunca hacia atrás).
ffill_limite <- function(x, limite) {
  ultimo <- NA
  cuenta <- 0L
  for (i in seq_along(x)) {
    if (!is.na(x[i])) {
      ultimo <- x[i]
      cuenta <- 0L
    } else if (!is.na(ultimo)) {
      cuenta <- cuenta + 1L
      if (cuenta <= limite) x[i] <- ultimo
    }
  }
  x
}

# Cantidad de días de lunes a viernes en [desde, hasta) para cada par de fechas.
dias_semana_entre <- function(desde, hasta) {
  vapply(seq_along(desde), function(i) {
    if (hasta[i] <= desde[i]) return(0L)
    s <- seq(desde[i], hasta[i] - 1, by = "day")
    sum(!(format(s, "%u") %in% c("6", "7")))
  }, integer(1))
}

es_vacio <- function(x) is.null(x) || length(x) == 0

# Registro de eventos con marca de tiempo (el documento exige logging de filas procesadas/descartadas)
log_info <- function(...) message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " INFO ", sprintf(...))
log_warn <- function(...) message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " ALERTA ", sprintf(...))
