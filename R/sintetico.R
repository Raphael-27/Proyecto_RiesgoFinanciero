# Datos sintéticos para pruebas y demostraciones sin acceso a las APIs.
# `serie_sintetica` reproduce la prueba de la Tabla 4 del documento: 1000 días hábiles con dos
# fechas duplicadas, dos registros incoherentes, dos nulos consecutivos, un error de captura
# aislado (precio x10 un solo día) y una caída persistente del 30 %.

PRECIOS <- c("open", "high", "low", "close", "adj_close")

serie_ohlc <- function(dias, semilla = 42, precio_inicial = 100) {
  set.seed(semilla)
  n <- length(dias)
  cierre <- precio_inicial * exp(cumsum(rnorm(n, 0.0003, 0.01)))
  apertura <- c(precio_inicial, cierre[-n]) * (1 + rnorm(n, 0, 0.002))
  maximo <- pmax(apertura, cierre) * (1 + abs(rnorm(n, 0, 0.004)))
  minimo <- pmin(apertura, cierre) * (1 - abs(rnorm(n, 0, 0.004)))
  data.frame(fecha = dias, open = apertura, high = maximo, low = minimo, close = cierre,
             adj_close = cierre, volume = as.numeric(sample(1e6:5e6, n, replace = TRUE)))
}

# posiciones (base 1) de las anomalías inyectadas en la serie base
POS <- list(duplicados = c(201L, 301L), negativo = 401L, high_menor_low = 451L,
            nulos = c(501L, 502L), error_captura = 601L, caida = 701L)

#' Devuelve list(raw, base): `base` es la serie sin anomalías, `raw` la que se limpia.
serie_sintetica <- function(dias = NULL, semilla = 42, con_anomalias = TRUE) {
  if (is.null(dias)) dias <- seq_dias_habiles(as.Date("2018-01-01"), 1000)
  base <- serie_ohlc(dias, semilla)
  if (!con_anomalias) return(list(raw = base, base = base))
  df <- base
  n <- nrow(df)

  df[POS$caida:n, PRECIOS] <- df[POS$caida:n, PRECIOS] * 0.7              # caída persistente del 30 %
  df[POS$error_captura, PRECIOS] <- df[POS$error_captura, PRECIOS] * 10   # error de captura
  df[POS$negativo, PRECIOS] <- -5                                         # registro incoherente
  df$high[POS$high_menor_low] <- df$low[POS$high_menor_low] * 0.98        # high < low
  df[POS$nulos[1]:POS$nulos[2], PRECIOS] <- NA_real_                      # nulos consecutivos

  # dos fechas duplicadas llegan DESPUÉS con valores corregidos (+0,1 %): debe ganar el último
  dup <- df[POS$duplicados, ]
  dup[PRECIOS] <- dup[PRECIOS] * 1.001
  list(raw = rbind(df, dup), base = base)
}

# `n` días de lunes a viernes desde `inicio`
seq_dias_habiles <- function(inicio, n) {
  d <- seq(inicio, by = "day", length.out = ceiling(n * 7 / 5) + 10)
  d <- d[!(format(d, "%u") %in% c("6", "7"))]
  d[seq_len(n)]
}

#' Prepara un entorno de demostración aislado (datos y base de datos en `directorio`) y escribe en
#' la capa raw series sintéticas para cada activo y variable macro. Devuelve la configuración
#' apuntando al entorno de demostración.
preparar_demo <- function(cfg, directorio = file.path(directorio_datos(cfg), "demo")) {
  cfg$proyecto$directorio_datos <- directorio
  cfg$carga$base_datos_url <- paste0("sqlite:///", file.path(directorio, "riesgo.db"))
  dir.create(directorio, recursive = TRUE, showWarnings = FALSE)

  for (i in seq_along(activos(cfg))) {
    a <- activos(cfg)[[i]]
    dias <- dias_esperados("2022-01-03", "2027-12-31", a$calendario)
    if (is.null(dias)) dias <- seq_dias_habiles(as.Date("2022-01-03"), 1000)
    raw <- serie_sintetica(dias[1:1000], semilla = 41 + i)$raw
    guardar_raw(raw, cfg, a$fuente %||% "yahoo", a$ticker)
  }
  set.seed(7)
  for (s in cfg$macro$series %||% list()) {
    mensual <- (s$rezago_dias %||% 1) > 10
    fechas <- if (mensual) seq(as.Date("2021-06-01"), as.Date("2026-10-31"), by = "month") else
      seq_dias_habiles(as.Date("2021-06-01"), 1400)
    valores <- as.character(round(cumsum(rnorm(length(fechas))) + 20, 3))
    valores[4] <- "."            # FRED marca los faltantes con "."
    guardar_raw(data.frame(fecha = fechas, valor = valores, stringsAsFactors = FALSE), cfg, "fred", s$id)
  }
  cfg
}
