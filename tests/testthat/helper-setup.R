# Carga el proyecto y define utilidades de prueba.
suppressMessages(source(file.path(testthat::test_path(), "..", "..", "R", "00_cargar.R")))

# Configuración real del proyecto, pero con datos y base de datos en una carpeta temporal.
cfg_temporal <- function() {
  cfg <- cargar_config()
  d <- tempfile("etl_"); dir.create(d)
  cfg$proyecto$directorio_datos <- d
  cfg$carga$base_datos_url <- paste0("sqlite:///", file.path(d, "test.db"))
  cfg$extraccion$espera_base_segundos <- 0
  cfg$extraccion$espera_maxima_segundos <- 0
  for (f in names(cfg$fuentes)) {
    for (k in c("pausa_entre_llamadas_segundos", "pausa_entre_activos_segundos")) {
      if (!is.null(cfg$fuentes[[f]][[k]])) cfg$fuentes[[f]][[k]] <- 0
    }
  }
  cfg
}

# Respuesta HTTP simulada: list(status, texto)
resp_http <- function(datos = NULL, status = 200, texto = NULL) {
  list(status = status, texto = texto %||% jsonlite::toJSON(datos, auto_unbox = TRUE, null = "null"))
}

# http_get falso: devuelve una cola de respuestas (la última se repite) y registra las llamadas
http_falso <- function(...) {
  cola <- list(...); llamadas <- list()
  fn <- function(url, params, timeout) {
    llamadas[[length(llamadas) + 1]] <<- list(url = url, params = params)
    r <- cola[[1]]
    if (length(cola) > 1) cola <<- cola[-1]
    if (inherits(r, "error")) stop(r)
    r
  }
  list(fn = fn, llamadas = function() llamadas)
}

dias_bd <- function(inicio, fin) { d <- seq(as.Date(inicio), as.Date(fin), by = "day"); d[!(format(d, "%u") %in% c("6", "7"))] }
