# Carga a base de datos relacional (SQLite por defecto; PostgreSQL en producción).
#
# La carga es IDEMPOTENTE: para cada (tabla, activo) se borran las filas previas y se insertan
# las nuevas dentro de una misma transacción; ejecutar dos veces el proceso para la misma fecha
# deja exactamente el mismo contenido, sin duplicados. La fecha se guarda como texto ISO
# (AAAA-MM-DD) para que cualquier herramienta (DB Browser, Excel, Power BI) la lea igual.
#
# PostgreSQL requiere el paquete RPostgres y NO se pudo probar en este desarrollo (solo SQLite).
# No se implementó InfluxDB (el documento lo menciona como alternativa).

crear_conexion <- function(cfg) {
  ruta <- ruta_sqlite(cfg)
  if (!is.null(ruta)) return(DBI::dbConnect(RSQLite::SQLite(), ruta))

  url <- cfg$carga$base_datos_url
  m <- regmatches(url, regexec("^postgres(?:ql)?(?:\\+\\w+)?://([^:@/]+)(?::([^@]*))?@([^:/]+)(?::(\\d+))?/(.+)$", url, perl = TRUE))[[1]]
  if (length(m) == 0) stop("base_datos_url no soportada. Usa sqlite:///ruta.db o postgresql://usuario:clave@host:5432/base", call. = FALSE)
  if (!requireNamespace("RPostgres", quietly = TRUE)) stop("Para PostgreSQL instala el paquete RPostgres: install.packages('RPostgres')", call. = FALSE)
  DBI::dbConnect(RPostgres::Postgres(), user = m[2], password = m[3], host = m[4],
                 port = as.integer(if (nzchar(m[5])) m[5] else 5432), dbname = m[6])
}

con_conexion <- function(cfg, f) {
  con <- crear_conexion(cfg)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  f(con)
}

asegurar_esquema <- function(con, tabla, df, claves = c("ticker", "fecha")) {
  q <- function(x) as.character(DBI::dbQuoteIdentifier(con, x))
  if (!DBI::dbExistsTable(con, tabla)) {
    DBI::dbCreateTable(con, tabla, df)
    if (length(claves)) {
      DBI::dbExecute(con, sprintf("CREATE UNIQUE INDEX IF NOT EXISTS %s ON %s (%s)", q(paste0("ux_", tabla)), q(tabla),
                                  paste(q(claves), collapse = ", ")))
    }
    return(invisible())
  }
  existentes <- DBI::dbListFields(con, tabla)
  for (col in setdiff(names(df), existentes)) {   # la tabla evolucionó: se agregan columnas nuevas
    DBI::dbExecute(con, sprintf("ALTER TABLE %s ADD COLUMN %s %s", q(tabla), q(col), DBI::dbDataType(con, df[[col]])))
  }
}

#' Reemplaza atómicamente las filas de `ticker` en `tabla`. Devuelve las filas insertadas.
cargar_dataframe <- function(con, tabla, df, ticker, claves = c("ticker", "fecha")) {
  df <- as.data.frame(df)
  if ("fecha" %in% names(df)) df$fecha <- format(as.Date(df$fecha), "%Y-%m-%d")
  df$ticker <- ticker
  for (c in names(df)) {                       # inf -> NULL
    if (is.numeric(df[[c]])) df[[c]][!is.finite(df[[c]])] <- NA
  }
  DBI::dbWithTransaction(con, {
    asegurar_esquema(con, tabla, df, claves)
    DBI::dbExecute(con, DBI::sqlInterpolate(
      con, sprintf("DELETE FROM %s WHERE ticker = ?tk", DBI::dbQuoteIdentifier(con, tabla)), tk = ticker))
    if (nrow(df) > 0) DBI::dbAppendTable(con, tabla, df)
  })
  nrow(df)
}

#' Guarda el reporte de la ejecución (se reemplaza si se repite la misma ejecución).
cargar_reporte <- function(con, ejecucion, ticker, reporte, calidad) {
  DBI::dbWithTransaction(con, {
    if (!DBI::dbExistsTable(con, "reportes_calidad")) {
      DBI::dbExecute(con, paste("CREATE TABLE reportes_calidad (ejecucion TEXT NOT NULL, ticker TEXT NOT NULL,",
                                "reporte TEXT, calidad TEXT, PRIMARY KEY (ejecucion, ticker))"))
    }
    DBI::dbExecute(con, DBI::sqlInterpolate(
      con, "DELETE FROM reportes_calidad WHERE ejecucion = ?ej AND ticker = ?tk", ej = ejecucion, tk = ticker))
    # OJO: los argumentos de sqlInterpolate se nombran ej/tk/rp/cl; una letra como `c` o `s` se
    # interpretaría como abreviatura de los argumentos formales `conn`/`sql` (coincidencia parcial).
    a_json <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE, na = "null", digits = NA, null = "null"))
    DBI::dbExecute(con, DBI::sqlInterpolate(
      con, "INSERT INTO reportes_calidad (ejecucion, ticker, reporte, calidad) VALUES (?ej, ?tk, ?rp, ?cl)",
      ej = ejecucion, tk = ticker, rp = a_json(reporte), cl = a_json(calidad)))
  })
  invisible(TRUE)
}

#' Lee las características de un activo desde la base (con sus banderas como lógicas).
leer_caracteristicas <- function(cfg, ticker) {
  df <- con_conexion(cfg, function(con) {
    if (!DBI::dbExistsTable(con, "caracteristicas")) {
      stop(paste0("La base de datos aún no tiene datos. Ejecuta primero el pipeline:  ",
                  "Rscript pipeline.R --sin-macro   (o --demo para datos de prueba)"), call. = FALSE)
    }
    DBI::dbGetQuery(con, DBI::sqlInterpolate(
      con, "SELECT * FROM caracteristicas WHERE ticker = ?tk ORDER BY fecha", tk = ticker))
  })
  if (nrow(df) == 0) stop(sprintf("No hay datos de %s en la base de datos. Ejecuta antes el pipeline.", ticker), call. = FALSE)
  df$fecha <- as.Date(df$fecha)
  for (c in intersect(c("es_atipico", "error_dato", "relleno"), names(df))) df[[c]] <- as.logical(df[[c]])
  df
}
