# Capas de datos tipo medallón (Tabla 1): raw -> clean -> curated, en archivos CSV.
#
# raw     data/raw/<fuente>/<ticker>/<AAAAMMDDTHHMMSS.mmm>Z.csv   (respuesta original, intacta)
# clean   data/clean/ticker=<T>/year=<AAAA>.csv                    (particionado por activo y año)
# curated data/curated/<ticker>.csv                                (características + macro)
#         data/modelos/                                            (predicciones y métricas de los modelos)
#         data/backtesting/                                        (VaR, crisis y modelos: resultados del backtesting)
#         data/reportes/                                           (perfilado y calidad por ejecución)
# Los datos raw nunca se modifican: cada extracción crea un archivo nuevo.

# Mensaje claro cuando faltan los datos procesados (lo más común: se borró o reemplazó la carpeta del proyecto)
mensaje_sin_datos <- function(capa, ticker, cfg) {
  sprintf(paste0("No hay %s de %s en:\n  %s\n",
                 "Si borraste o reemplazaste la carpeta del proyecto, los datos se perdieron con ella. ",
                 "Regénéralos ejecutando:  Rscript pipeline.R --sin-macro\n",
                 "Para que no vuelva a pasar, define ETL_DATOS en el archivo .env (ver README)."),
          capa, ticker, directorio_datos(cfg))
}

raiz_capa <- function(cfg, capa) {
  ruta <- file.path(directorio_datos(cfg), capa)
  dir.create(ruta, recursive = TRUE, showWarnings = FALSE)
  ruta
}

escribir_csv <- function(df, ruta) {
  utils::write.csv(df, ruta, row.names = FALSE, fileEncoding = "UTF-8")
}

leer_csv <- function(ruta) {
  utils::read.csv(ruta, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
}

# Fecha como texto ISO (con zona horaria si la tiene) para guardar el dato raw tal como llegó
fecha_a_texto <- function(x) {
  if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
  if (inherits(x, "POSIXt")) return(format(x, "%Y-%m-%d %H:%M:%S%z"))
  as.character(x)
}

# ------------------------------------------------------------------------------------ raw
guardar_raw <- function(df, cfg, fuente, clave, ahora = Sys.time()) {
  carpeta <- file.path(raiz_capa(cfg, "raw"), fuente, seguro_para_ruta(clave))
  dir.create(carpeta, recursive = TRUE, showWarnings = FALSE)
  out <- df
  out$fecha <- fecha_a_texto(out$fecha)
  out$fecha_descarga <- format(as.POSIXct(ahora), "%Y-%m-%d %H:%M:%S", tz = "UTC")
  out$fuente <- fuente
  ruta <- file.path(carpeta, paste0(format(as.POSIXct(ahora), "%Y%m%dT%H%M%OS3", tz = "UTC"), "Z.csv"))
  escribir_csv(out, ruta)
  invisible(ruta)
}

leer_raw_ultimo <- function(cfg, fuente, clave) {
  carpeta <- file.path(raiz_capa(cfg, "raw"), fuente, seguro_para_ruta(clave))
  archivos <- sort(list.files(carpeta, pattern = "\\.csv$", full.names = TRUE))
  if (length(archivos) == 0) {
    stop(sprintf("No hay datos raw de %s/%s. Ejecuta primero la extracción.", fuente, clave), call. = FALSE)
  }
  df <- leer_csv(archivos[length(archivos)])
  df[, setdiff(names(df), c("fecha_descarga", "fuente")), drop = FALSE]
}

# ---------------------------------------------------------------------------------- clean
guardar_clean <- function(df, cfg, ticker) {
  carpeta <- file.path(raiz_capa(cfg, "clean"), paste0("ticker=", seguro_para_ruta(ticker)))
  if (dir.exists(carpeta)) unlink(carpeta, recursive = TRUE)   # se reescribe completo (idempotente)
  dir.create(carpeta, recursive = TRUE, showWarnings = FALSE)
  out <- df
  out$ticker <- NULL
  out$fecha <- format(as.Date(out$fecha), "%Y-%m-%d")
  for (anio in unique(substr(out$fecha, 1, 4))) {
    escribir_csv(out[substr(out$fecha, 1, 4) == anio, , drop = FALSE], file.path(carpeta, paste0("year=", anio, ".csv")))
  }
  invisible(carpeta)
}

leer_clean <- function(cfg, ticker) {
  carpeta <- file.path(raiz_capa(cfg, "clean"), paste0("ticker=", seguro_para_ruta(ticker)))
  archivos <- sort(list.files(carpeta, pattern = "\\.csv$", full.names = TRUE))
  if (length(archivos) == 0) stop(mensaje_sin_datos("capa clean", ticker, cfg), call. = FALSE)
  df <- do.call(rbind, lapply(archivos, leer_csv))
  df$fecha <- as.Date(df$fecha)
  df$ticker <- ticker
  df <- df[order(df$fecha), , drop = FALSE]
  rownames(df) <- NULL
  df
}

guardar_macro_clean <- function(df, cfg) {
  out <- df
  out$fecha <- format(out$fecha, "%Y-%m-%d")
  if ("fecha_publicacion" %in% names(out)) out$fecha_publicacion <- format(out$fecha_publicacion, "%Y-%m-%d")
  escribir_csv(out, file.path(raiz_capa(cfg, "clean"), "macro.csv"))
}

leer_macro_clean <- function(cfg) {
  ruta <- file.path(raiz_capa(cfg, "clean"), "macro.csv")
  if (!file.exists(ruta)) return(NULL)
  df <- leer_csv(ruta)
  df$fecha <- as.Date(df$fecha)
  if ("fecha_publicacion" %in% names(df)) df$fecha_publicacion <- as.Date(df$fecha_publicacion)
  df
}

# -------------------------------------------------------------------------------- curated
guardar_curada <- function(df, cfg, ticker) {
  out <- df
  out$fecha <- format(as.Date(out$fecha), "%Y-%m-%d")
  escribir_csv(out, file.path(raiz_capa(cfg, "curated"), paste0(seguro_para_ruta(ticker), ".csv")))
}

leer_curada <- function(cfg, ticker) {
  ruta <- file.path(raiz_capa(cfg, "curated"), paste0(seguro_para_ruta(ticker), ".csv"))
  if (!file.exists(ruta)) stop(mensaje_sin_datos("capa curated", ticker, cfg), call. = FALSE)
  df <- leer_csv(ruta)
  df$fecha <- as.Date(df$fecha)
  df
}

# ----------------------------------------------------------------------------- reportes
guardar_reporte <- function(obj, cfg, nombre) {
  ruta <- file.path(raiz_capa(cfg, "reportes"), paste0(nombre, ".json"))
  jsonlite::write_json(obj, ruta, auto_unbox = TRUE, pretty = TRUE, na = "null", digits = NA, null = "null")
  invisible(ruta)
}

leer_reporte <- function(cfg, nombre) {
  ruta <- file.path(raiz_capa(cfg, "reportes"), paste0(nombre, ".json"))
  if (!file.exists(ruta)) return(NULL)
  jsonlite::read_json(ruta, simplifyVector = FALSE)
}

# ------------------------------------------------------------------------------- modelos
guardar_modelos <- function(df, cfg, nombre) {
  ruta <- file.path(raiz_capa(cfg, "modelos"), paste0(nombre, ".csv"))
  out <- df
  if ("fecha" %in% names(out)) out$fecha <- format(as.Date(out$fecha), "%Y-%m-%d")
  escribir_csv(out, ruta)
  invisible(ruta)
}

leer_modelos <- function(cfg, nombre) {
  ruta <- file.path(raiz_capa(cfg, "modelos"), paste0(nombre, ".csv"))
  if (!file.exists(ruta)) return(NULL)
  df <- leer_csv(ruta)
  if ("fecha" %in% names(df)) df$fecha <- as.Date(df$fecha)
  df
}

# ---------------------------------------------------------------------------- backtesting
guardar_tabla <- function(df, cfg, capa, nombre) {
  out <- df
  for (c in names(out)) if (inherits(out[[c]], "Date")) out[[c]] <- format(out[[c]], "%Y-%m-%d")
  escribir_csv(out, file.path(raiz_capa(cfg, capa), paste0(nombre, ".csv")))
  invisible(TRUE)
}

leer_tabla <- function(cfg, capa, nombre) {
  ruta <- file.path(raiz_capa(cfg, capa), paste0(nombre, ".csv"))
  if (!file.exists(ruta)) return(NULL)
  df <- leer_csv(ruta)
  for (c in intersect(c("fecha", "primera_alerta", "peor_dia"), names(df))) df[[c]] <- as.Date(df[[c]])
  df
}
