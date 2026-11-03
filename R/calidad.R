# Perfilado inicial y validaciones automáticas de calidad (Tabla 3 del documento).

UMBRALES_DEFECTO <- list(completitud_min = 0.99, duplicados_max = 0, incoherentes_max = 0,
                         huecos_max_dias_habiles = 3, oportunidad_max_horas = 2, impacto_max_pct = 0.02)

error_calidad <- function(mensaje) {
  structure(class = c("error_calidad", "error", "condition"), list(message = mensaje, call = NULL))
}

# ------------------------------------------------------------------------------ perfilado
frecuencia_serie <- function(fechas) {
  if (length(fechas) < 3) return("indeterminada")
  m <- stats::median(as.numeric(diff(sort(fechas))))
  if (m <= 4) "diaria" else if (m <= 10) "semanal" else if (m <= 40) "mensual" else "trimestral_o_menor"
}

resumen_rango <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NULL)
  q <- stats::quantile(x, c(0.01, 0.5, 0.99), names = FALSE)
  list(min = min(x), p01 = q[1], p50 = q[2], p99 = q[3], max = max(x), std = stats::sd(x))
}

#' Línea base de calidad de una serie ANTES de limpiar (completitud, unicidad, rango,
#' cobertura temporal y frecuencia).
perfilar <- function(df_raw, ticker, calendario = NULL, zona_horaria = NULL) {
  e <- estandarizar_esquema(df_raw, zona_horaria)
  df <- e$df
  nombres_raw <- gsub(" ", "_", tolower(trimws(names(df_raw))))
  perfil <- c(list(ticker = ticker, filas = nrow(df)), e$stats)

  cols <- intersect(COLUMNAS_NUMERICAS, nombres_raw)
  perfil$completitud_columnas <- stats::setNames(lapply(cols, function(c) {
    v <- df_raw[[which(nombres_raw == c)[1]]]
    round(mean(!is.na(v) & !(is.character(v) & is.na(suppressWarnings(as.numeric(v))))), 6)
  }), cols)
  perfil$fechas_duplicadas <- sum(duplicated(df$fecha))

  precio <- df$adj_close
  precio[!is.na(precio) & precio <= 0] <- NA
  retornos <- diff(log(precio)); retornos <- retornos[is.finite(retornos)]
  perfil$rangos <- Filter(Negate(is.null), list(
    adj_close = resumen_rango(precio),
    volume = if ("volume" %in% names(df)) resumen_rango(df$volume) else NULL,
    retorno_log = resumen_rango(retornos)))

  unicas <- sort(unique(df$fecha))
  perfil$cobertura <- list(inicio = if (length(unicas)) format(min(unicas)) else NA,
                           fin = if (length(unicas)) format(max(unicas)) else NA,
                           dias_recibidos = length(unicas))
  if (length(unicas)) {
    esp <- dias_esperados(min(unicas), max(unicas), calendario)
    perfil$cobertura$dias_habiles_esperados <-
      if (!is.null(esp)) length(esp) else sum(!(format(seq(min(unicas), max(unicas), by = "day"), "%u") %in% c("6", "7")))
  }
  perfil$frecuencia <- frecuencia_serie(unicas)
  perfil
}

# --------------------------------------------------------------------------- validaciones
resultado <- function(dimension, metrica, valor, umbral, cumple, bloqueante, detalle = "") {
  list(dimension = dimension, metrica = metrica, valor = valor, umbral = umbral,
       cumple = isTRUE(cumple), bloqueante = isTRUE(bloqueante), detalle = detalle)
}

# Huecos con más de `max_dias` días hábiles faltantes entre dos fechas consecutivas
huecos_largos <- function(fechas, max_dias) {
  if (length(fechas) < 2) return(0L)
  faltantes <- dias_semana_entre(fechas[-length(fechas)], fechas[-1]) - 1L
  sum(faltantes > max_dias)
}

#' Calcula las seis dimensiones de la Tabla 3 para una serie ya limpia.
evaluar_calidad <- function(df, reporte, activo, cfg_calidad = NULL, ahora = Sys.time(), tolerancia_ohlc = 1e-6) {
  cfg_calidad <- cfg_calidad %||% list()
  u <- utils::modifyList(UMBRALES_DEFECTO, cfg_calidad$umbrales %||% list())
  bloqueantes <- unlist(cfg_calidad$bloqueantes %||% c("completitud", "unicidad", "validez", "consistencia", "oportunidad"))
  calendario <- activo$calendario

  if (nrow(df) == 0) {
    return(list(resultado("completitud", "serie_vacia", 0, u$completitud_min, FALSE, TRUE,
                          "La serie quedó vacía tras la limpieza")))
  }
  res <- list()

  # Completitud: % de días hábiles esperados con cierre válido (sin contar los rellenados)
  esperados <- dias_esperados(min(df$fecha), max(df$fecha), calendario)
  if (is.null(esperados)) {
    res[[length(res) + 1]] <- resultado("completitud", "pct_dias_con_cierre_valido", NA, u$completitud_min,
                                        TRUE, FALSE, "No evaluada: el activo no tiene calendario de feriados")
  } else {
    validos <- if ("relleno" %in% names(df)) df$fecha[!df$relleno] else df$fecha
    valor <- length(intersect(as.numeric(validos), as.numeric(esperados))) / max(length(esperados), 1)
    res[[length(res) + 1]] <- resultado("completitud", "pct_dias_con_cierre_valido", round(valor, 6),
                                        u$completitud_min, valor >= u$completitud_min, "completitud" %in% bloqueantes)
  }

  dup <- sum(duplicated(df$fecha))
  res[[length(res) + 1]] <- resultado("unicidad", "fechas_duplicadas", dup, u$duplicados_max,
                                      dup <= u$duplicados_max, "unicidad" %in% bloqueantes)

  inc <- sum(mascara_incoherente(df, tolerancia_ohlc))
  res[[length(res) + 1]] <- resultado("validez", "registros_incoherentes_ohlc", inc, u$incoherentes_max,
                                      inc <= u$incoherentes_max, "validez" %in% bloqueantes)

  fuera_de_orden <- sum(diff(as.numeric(df$fecha)) < 0)
  huecos <- huecos_largos(df$fecha, u$huecos_max_dias_habiles)
  res[[length(res) + 1]] <- resultado(
    "consistencia", "fechas_fuera_de_orden_o_huecos_largos", fuera_de_orden + huecos, 0,
    fuera_de_orden + huecos == 0, "consistencia" %in% bloqueantes,
    sprintf("fuera_de_orden=%d, huecos>%dd=%d", fuera_de_orden, as.integer(u$huecos_max_dias_habiles), huecos))

  res[[length(res) + 1]] <- evaluar_oportunidad(df, activo, cfg_calidad, u, ahora, "oportunidad" %in% bloqueantes)

  pct <- reporte$pct_modificadas %||% 0
  res[[length(res) + 1]] <- resultado("impacto_limpieza", "pct_filas_modificadas_o_descartadas", round(pct, 6),
                                      u$impacto_max_pct, pct <= u$impacto_max_pct, "impacto" %in% bloqueantes,
                                      "Alerta si supera el umbral")
  res
}

evaluar_oportunidad <- function(df, activo, cfg_calidad, u, ahora, bloqueante) {
  m <- "horas_desde_cierre_hasta_dato_limpio"
  if (!isTRUE(cfg_calidad$evaluar_oportunidad)) {
    return(resultado("oportunidad", m, NA, u$oportunidad_max_horas, TRUE, FALSE,
                     "No evaluada (calidad.evaluar_oportunidad = false)"))
  }
  zona <- activo$zona_horaria %||% "UTC"
  hm <- as.integer(strsplit(as.character(activo$cierre %||% "16:00"), ":")[[1]])
  local <- as.POSIXct(format(as.POSIXct(ahora), tz = zona, usetz = FALSE), tz = zona)
  hoy <- as.Date(format(as.POSIXct(ahora), "%Y-%m-%d", tz = zona))
  cierre <- as.POSIXct(sprintf("%s %02d:%02d:00", format(hoy), hm[1], hm[2]), tz = zona)

  if (!es_dia_habil(hoy, activo$calendario) || local < cierre) {
    return(resultado("oportunidad", m, NA, u$oportunidad_max_horas, TRUE, FALSE,
                     "No evaluada: hoy no hay sesión cerrada"))
  }
  if (max(df$fecha) < hoy) {
    return(resultado("oportunidad", m, NA, u$oportunidad_max_horas, FALSE, bloqueante, "Falta el dato de la sesión de hoy"))
  }
  horas <- as.numeric(difftime(local, cierre, units = "hours"))
  resultado("oportunidad", m, round(horas, 3), u$oportunidad_max_horas, horas <= u$oportunidad_max_horas, bloqueante)
}

#' Lanza un error de clase error_calidad si alguna métrica bloqueante de algún activo no cumple.
exigir_calidad <- function(resultados) {
  fallos <- character()
  for (t in names(resultados)) for (r in resultados[[t]]) {
    if (!r$cumple && r$bloqueante) {
      fallos <- c(fallos, trimws(sprintf("%s: %s=%s (umbral %s) %s", t, r$dimension,
                                         format(r$valor), format(r$umbral), r$detalle)))
    }
  }
  if (length(fallos)) {
    stop(error_calidad(paste0("Calidad insuficiente, se detiene antes de la carga:\n  - ",
                              paste(fallos, collapse = "\n  - "))))
  }
  invisible(TRUE)
}

#' Métricas no bloqueantes incumplidas (se notifican pero no detienen el flujo).
alertas <- function(resultados) {
  out <- character()
  for (t in names(resultados)) for (r in resultados[[t]]) {
    if (!r$cumple && !r$bloqueante) {
      out <- c(out, sprintf("%s: %s=%s supera/incumple el umbral %s", t, r$dimension, format(r$valor), format(r$umbral)))
    }
  }
  out
}
