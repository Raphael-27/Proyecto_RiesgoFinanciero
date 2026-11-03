# Módulo de limpieza del ETL - Sistema de Predicción de Riesgo Financiero (versión R).
#
# Cada regla (Tabla 2 del documento) vive en su propia función y devuelve cuántos registros
# tocó, para construir el reporte de calidad de cada ejecución. Los datos son data.frames con
# una columna `fecha` (Date) y las columnas open, high, low, close, adj_close, volume.
#
# Decisiones metodológicas (las mismas que la versión Python):
#  * "Conservar el último registro recibido": el orden de llegada se respeta (order() es estable).
#  * Las fechas con zona horaria se convierten a la fecha LOCAL del mercado, no a UTC.
#  * El z-score robusto usa solo observaciones pasadas (excluye el dato evaluado) y adj_close.
#  * Un "error de dato" exige: salto extremo + reversión al día siguiente + salto desproporcionado
#    + sin volumen disparado. Así el rebote en V del 12-13/mar/2020 NO se corrige como error.
#  * Los feriados reales no se rellenan: solo se completan días de negociación esperados.

COLUMNAS_PRECIO <- c("open", "high", "low", "close", "adj_close")
COLUMNAS_NUMERICAS <- c(COLUMNAS_PRECIO, "volume")

parametros_limpieza <- function(cfg = NULL) {
  p <- list(ventana = 60L, min_periodos = 20L, umbral_atipico = 8, tolerancia_reversion = 0.25,
            salto_minimo_error = 0.405, factor_volumen_real = 3, limite_ffill = 3L,
            tolerancia_ohlc = 1e-6)
  c <- cfg$limpieza %||% list()
  if (!is.null(c$umbral)) p$umbral_atipico <- c$umbral
  for (k in names(p)) if (!is.null(c[[k]])) p[[k]] <- c[[k]]
  p
}

# ------------------------------------------------------------------------------ esquema
# Fecha calendario de cualquier entrada (Date, POSIXct, texto, texto con zonas horarias mezcladas).
# Con zona horaria -> fecha local del mercado; sin zona -> la fecha tal como viene.
a_fecha_local <- function(x, zona_horaria = NULL) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) {
    tz <- zona_horaria %||% attr(x, "tzone")[1] %||% "UTC"
    if (is.na(tz) || !nzchar(tz)) tz <- "UTC"
    return(as.Date(format(x, "%Y-%m-%d", tz = tz)))
  }
  s <- trimws(as.character(x))
  out <- rep(as.Date(NA), length(s))
  ok <- !is.na(s) & nzchar(s)

  patron_zona <- "^\\d{4}-\\d{2}-\\d{2}[T ]\\d{2}:\\d{2}(:\\d{2}(\\.\\d+)?)?(Z|[+-]\\d{2}:?\\d{2})$"
  con_zona <- ok & grepl(patron_zona, s)
  if (any(con_zona)) {
    z <- s[con_zona]
    if (is.null(zona_horaria)) {
      out[con_zona] <- as.Date(substr(z, 1, 10), format = "%Y-%m-%d")
    } else {
      z <- sub("Z$", "+0000", z)
      z <- sub("([+-]\\d{2}):(\\d{2})$", "\\1\\2", z)
      z <- sub("T", " ", z, fixed = TRUE)
      z <- sub("\\.\\d+", "", z)
      z <- ifelse(grepl(":\\d{2}:\\d{2}[+-]\\d{4}$", z), z, sub("([+-]\\d{4})$", ":00\\1", z))
      instante <- as.POSIXct(strptime(z, "%Y-%m-%d %H:%M:%S%z", tz = "UTC"), tz = "UTC")
      out[con_zona] <- as.Date(format(instante, "%Y-%m-%d", tz = zona_horaria))
    }
  }
  resto <- ok & !con_zona
  if (any(resto)) {
    d <- as.Date(substr(s[resto], 1, 10), format = "%Y-%m-%d")
    alt <- as.Date(substr(s[resto], 1, 10), format = "%Y/%m/%d")
    d[is.na(d)] <- alt[is.na(d)]
    out[resto] <- d
  }
  out
}

#' Normaliza nombres de columnas, tipos de dato y fechas. Devuelve list(df, stats).
estandarizar_esquema <- function(df, zona_horaria = NULL) {
  names(df) <- gsub(" ", "_", tolower(trimws(names(df))))
  if (!"fecha" %in% names(df)) stop("La serie debe tener una columna 'fecha'.", call. = FALSE)
  df$fecha <- a_fecha_local(df$fecha, zona_horaria)
  validas <- !is.na(df$fecha)
  stats <- list(fechas_invalidas = sum(!validas))
  df <- df[validas, , drop = FALSE]

  no_numericos <- 0L
  for (col in intersect(COLUMNAS_NUMERICAS, names(df))) {
    antes <- !is.na(df[[col]])
    df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
    no_numericos <- no_numericos + sum(antes & is.na(df[[col]]))
  }
  stats$valores_no_numericos <- no_numericos

  if (!"close" %in% names(df) && !"adj_close" %in% names(df)) {
    stop("La serie no tiene columna 'close' ni 'adj_close'.", call. = FALSE)
  }
  if (!"close" %in% names(df)) df$close <- df$adj_close
  if (!"adj_close" %in% names(df)) {
    # Fuente sin ajuste (p. ej. Alpha Vantage gratuito): se usa close y se deja constancia.
    df$adj_close <- df$close
    stats$adj_close_es_close <- TRUE
  }
  df <- df[order(df$fecha), , drop = FALSE]   # order() es estable: conserva el orden de llegada
  rownames(df) <- NULL
  list(df = df, stats = stats)
}

# ------------------------------------------------------------------------------- reglas
#' Conserva el último registro recibido para cada fecha.
eliminar_duplicados <- function(df) {
  dup <- duplicated(df$fecha, fromLast = TRUE)
  list(df = df[!dup, , drop = FALSE], n = sum(dup))
}

#' Elimina fines de semana y, si hay calendario, feriados bursátiles.
excluir_no_habiles <- function(df, calendario = NULL) {
  if (nrow(df) == 0) return(list(df = df, n = 0L))
  if (is.null(calendario)) {
    habil <- !(format(df$fecha, "%u") %in% c("6", "7"))
  } else {
    habil <- df$fecha %in% dias_esperados(min(df$fecha), max(df$fecha), calendario)
  }
  list(df = df[habil, , drop = FALSE], n = sum(!habil))
}

mascara_incoherente <- function(df, tol = 1e-6) {
  precios <- intersect(COLUMNAS_PRECIO, names(df))
  inval <- rowSums(sapply(precios, function(c) !is.na(df[[c]]) & df[[c]] <= 0)) > 0
  if (all(c("high", "low") %in% names(df))) {
    inval <- inval |
      (!is.na(df$high) & !is.na(df$low) & df$high < df$low * (1 - tol)) |
      (!is.na(df$close) & !is.na(df$high) & df$close > df$high * (1 + tol)) |
      (!is.na(df$close) & !is.na(df$low) & df$close < df$low * (1 - tol))
  }
  if ("volume" %in% names(df)) inval <- inval | (!is.na(df$volume) & df$volume < 0)
  inval
}

#' Invalida (pone en NA) los registros que violan reglas básicas de un mercado real.
validar_coherencia_ohlc <- function(df, tol = 1e-6) {
  inval <- mascara_incoherente(df, tol)
  for (col in intersect(COLUMNAS_NUMERICAS, names(df))) df[[col]][inval] <- NA_real_
  list(df = df, n = sum(inval))
}

#' Agrega filas vacías para los días de negociación esperados que faltan dentro del rango.
#' Solo con calendario conocido: los feriados reales NO se rellenan (evita retornos artificiales
#' de valor cero). Las filas nuevas se cubren luego si el hueco es corto, o se descartan.
completar_calendario <- function(df, calendario = NULL) {
  if (is.null(calendario) || nrow(df) == 0) return(list(df = df, n = 0L))
  esperados <- dias_esperados(min(df$fecha), max(df$fecha), calendario)
  faltantes <- esperados[!(esperados %in% df$fecha)]
  if (length(faltantes) == 0) return(list(df = df, n = 0L))
  extra <- df[rep(NA_integer_, length(faltantes)), , drop = FALSE]
  extra$fecha <- faltantes
  df <- rbind(df, extra)
  df <- df[order(df$fecha), , drop = FALSE]
  rownames(df) <- NULL
  list(df = df, n = length(faltantes))
}

#' Detecta saltos extremos con z-score robusto (mediana y MAD) sobre retornos logarítmicos.
#'  - es_atipico: retorno extremo (se conserva siempre; es información de riesgo).
#'  - error_dato: extremo que se revierte al día siguiente, es desproporcionado
#'    (|retorno log| >= salto_minimo_error, ~50 %) y no viene con volumen disparado.
marcar_atipicos <- function(df, ventana = 60L, umbral = 8, tol = 0.25, min_periodos = 20L,
                            salto_minimo_error = 0.405, factor_volumen = 3) {
  precio <- if ("adj_close" %in% names(df)) df$adj_close else df$close
  precio[!is.na(precio) & precio <= 0] <- NA
  r <- c(NA_real_, diff(log(precio)))

  mm <- mediana_mad_previa(r, ventana, min_periodos)
  mad <- ifelse(!is.na(mm$mad) & mm$mad > 0, mm$mad, NA_real_)
  z <- 0.6745 * (r - mm$mediana) / mad
  extremo <- !is.na(z) & abs(z) > umbral

  r_sig <- c(r[-1], NA_real_)
  reversion <- !is.na(r + r_sig) & abs(r + r_sig) < tol * abs(r)
  desproporcionado <- !is.na(r) & abs(r) >= salto_minimo_error

  volumen_real <- rep(FALSE, nrow(df))
  if ("volume" %in% names(df) && !is.null(factor_volumen) && factor_volumen > 0) {
    previo <- mediana_mad_previa(df$volume, ventana, min_periodos)$mediana
    volumen_real <- !is.na(df$volume) & !is.na(previo) & df$volume >= factor_volumen * previo
  }
  df$es_atipico <- extremo
  df$error_dato <- extremo & reversion & desproporcionado & !volumen_real
  df
}

#' Propaga el último precio válido (máx. `limite_ffill` días; nunca hacia atrás).
#' Devuelve list(df, n = filas descartadas); agrega `relleno` = TRUE en las filas propagadas.
tratar_nulos <- function(df, limite_ffill = 3L) {
  precios <- intersect(COLUMNAS_PRECIO, names(df))
  vacio_antes <- is.na(df$close)
  for (col in precios) df[[col]] <- ffill_limite(df[[col]], limite_ffill)
  df$relleno <- vacio_antes & !is.na(df$close)
  n_inicial <- nrow(df)
  df <- df[!is.na(df$close) & !is.na(df$adj_close), , drop = FALSE]
  list(df = df, n = n_inicial - nrow(df))
}

# --------------------------------------------------------------------------- orquestación
#' Orquesta la limpieza de una serie y devuelve list(df, reporte).
limpiar_serie <- function(df, ticker, params = parametros_limpieza(), zona_horaria = NULL,
                          calendario = NULL, fuente = NA_character_) {
  rep <- list(ticker = ticker, fuente = fuente, filas_entrada = nrow(df))

  e <- estandarizar_esquema(df, zona_horaria); df <- e$df; rep <- c(rep, e$stats)
  r <- eliminar_duplicados(df);   df <- r$df; rep$duplicados <- r$n
  r <- excluir_no_habiles(df, calendario); df <- r$df; rep$no_habiles <- r$n
  r <- validar_coherencia_ohlc(df, params$tolerancia_ohlc); df <- r$df; rep$incoherentes <- r$n
  r <- completar_calendario(df, calendario); df <- r$df; rep$dias_completados <- r$n

  df <- marcar_atipicos(df, params$ventana, params$umbral_atipico, params$tolerancia_reversion,
                        params$min_periodos, params$salto_minimo_error, params$factor_volumen_real)
  rep$atipicos <- sum(df$es_atipico)
  rep$errores_dato <- sum(df$error_dato)
  for (col in intersect(COLUMNAS_PRECIO, names(df))) df[[col]][df$error_dato] <- NA_real_

  r <- tratar_nulos(df, params$limite_ffill); df <- r$df; rep$filas_sin_precio <- r$n
  rep$rellenadas <- sum(df$relleno)
  rep$filas_salida <- nrow(df)

  # filas de la entrada que terminaron eliminadas o corregidas (métrica "impacto", Tabla 3)
  modificadas <- rep$duplicados + rep$no_habiles + rep$filas_sin_precio + rep$rellenadas
  rep$filas_modificadas <- modificadas
  rep$pct_modificadas <- round(modificadas / max(rep$filas_entrada, 1), 6)

  df$ticker <- ticker
  df$fuente <- fuente
  rownames(df) <- NULL
  list(df = df, reporte = rep)
}

# --------------------------------------------------------------------------------- macro
#' Limpia una serie macro en formato largo: columnas fecha, valor [, fecha_publicacion].
#' FRED marca los faltantes con "."; se descartan. Si hay varias versiones de una fecha se
#' conserva la de primera publicación.
limpiar_macro <- function(df, serie) {
  rep <- list(serie = serie, filas_entrada = nrow(df))
  df$fecha <- a_fecha_local(df$fecha)
  df$valor <- suppressWarnings(as.numeric(df$valor))
  if ("fecha_publicacion" %in% names(df)) {
    df$fecha_publicacion <- a_fecha_local(df$fecha_publicacion)
    df <- df[order(df$fecha, df$fecha_publicacion), , drop = FALSE]
  } else {
    df <- df[order(df$fecha), , drop = FALSE]
  }
  antes <- nrow(df)
  df <- df[!is.na(df$fecha) & !is.na(df$valor), , drop = FALSE]
  rep$nulos_descartados <- antes - nrow(df)
  antes <- nrow(df)
  df <- df[!duplicated(df$fecha), , drop = FALSE]    # primera publicación de cada fecha
  rep$duplicados <- antes - nrow(df)
  df$serie <- serie
  rep$filas_salida <- nrow(df)
  rownames(df) <- NULL
  list(df = df, reporte = rep)
}

#' Une variables macro respetando su fecha de publicación (evita look-ahead bias).
#' `macro` puede venir en formato largo (serie, fecha, valor [, fecha_publicacion]) o ancho
#' (fecha + una columna por serie). Un valor es utilizable desde fecha (o fecha_publicacion) +
#' rezago; cada fecha de precios recibe el último valor ya disponible. `tolerancia` (días)
#' limita la antigüedad máxima admitida del dato.
alinear_macro <- function(df_precios, macro, rezago_dias = 1, rezagos = NULL, tolerancia = NULL,
                          prefijo = "macro_") {
  if (!"serie" %in% names(macro)) {
    series <- setdiff(names(macro), "fecha")
    macro <- do.call(rbind, lapply(series, function(s)
      data.frame(serie = s, fecha = macro$fecha, valor = macro[[s]], stringsAsFactors = FALSE)))
  }
  macro$fecha <- a_fecha_local(macro$fecha)
  macro$valor <- suppressWarnings(as.numeric(macro$valor))
  if ("fecha_publicacion" %in% names(macro)) {
    base <- a_fecha_local(macro$fecha_publicacion)
    defecto <- 0
  } else {
    base <- macro$fecha
    defecto <- rezago_dias
  }
  dias <- vapply(macro$serie, function(s) as.numeric(rezagos[[s]] %||% defecto), numeric(1))
  macro$disponible <- base + dias
  macro <- macro[!is.na(macro$valor) & !is.na(macro$disponible), , drop = FALSE]

  salida <- df_precios[order(df_precios$fecha), , drop = FALSE]
  for (serie in unique(macro$serie)) {
    g <- macro[macro$serie == serie, , drop = FALSE]
    g <- g[order(g$fecha, g$disponible), , drop = FALSE]
    g <- g[!duplicated(g$fecha), , drop = FALSE]            # primera publicación
    g <- g[order(g$disponible), , drop = FALSE]
    idx <- findInterval(as.numeric(salida$fecha), as.numeric(g$disponible))   # último disponible <= fecha
    valor <- ifelse(idx > 0, g$valor[pmax(idx, 1)], NA_real_)
    if (!is.null(tolerancia)) {
      edad <- as.numeric(salida$fecha) - as.numeric(g$disponible[pmax(idx, 1)])
      valor[idx > 0 & edad > tolerancia] <- NA_real_
    }
    salida[[paste0(prefijo, serie)]] <- valor
  }
  rownames(salida) <- NULL
  salida
}
