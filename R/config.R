# Carga de configuración y credenciales (nunca se guardan claves en el repositorio).

# Lee un archivo .env sencillo (CLAVE=valor) sin pisar variables que ya existan en el entorno.
cargar_env <- function(ruta = file.path(ETL_RAIZ, ".env")) {
  if (!file.exists(ruta)) return(invisible(FALSE))
  for (linea in readLines(ruta, warn = FALSE, encoding = "UTF-8")) {
    linea <- trimws(linea)
    if (!nzchar(linea) || startsWith(linea, "#") || !grepl("=", linea, fixed = TRUE)) next
    k <- trimws(sub("=.*$", "", linea))
    v <- trimws(sub("^[^=]*=", "", linea))
    v <- gsub("^[\"']|[\"']$", "", v)
    if (nzchar(v) && !nzchar(Sys.getenv(k))) do.call(Sys.setenv, stats::setNames(list(v), k))
  }
  invisible(TRUE)
}

# Prioridad de la ruta: argumento > variable ETL_CONFIG > config/config.yaml
cargar_config <- function(ruta = NULL) {
  cargar_env()
  ruta <- ruta %||% (if (nzchar(Sys.getenv("ETL_CONFIG"))) Sys.getenv("ETL_CONFIG") else NULL) %||%
    file.path(ETL_RAIZ, "config", "config.yaml")
  if (!file.exists(ruta)) stop(sprintf("No existe el archivo de configuración: %s", ruta), call. = FALSE)
  # Se lee como UTF-8 explícito: read_yaml() depende de la configuración regional del equipo y, en
  # un locale distinto de UTF-8, corta el archivo en el primer acento SIN avisar.
  texto <- paste(readLines(ruta, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  cfg <- yaml::yaml.load(texto)
  validar_config(cfg, ruta)
  cfg$.raiz <- normalizePath(dirname(dirname(ruta)))
  # ETL_DATOS: carpeta de datos FUERA del proyecto (definida en .env, que no se sobrescribe al actualizar el
  # código). Así borrar o reemplazar la carpeta del proyecto nunca pierde los datos descargados y calculados.
  datos <- Sys.getenv("ETL_DATOS")
  if (nzchar(datos)) {
    cfg$proyecto$directorio_datos <- datos
    if (!nzchar(Sys.getenv("DATABASE_URL"))) cfg$carga$base_datos_url <- paste0("sqlite:///", file.path(datos, "riesgo.db"))
  }
  url <- Sys.getenv("DATABASE_URL")
  if (nzchar(url)) {
    cfg$carga <- cfg$carga %||% list()
    cfg$carga$base_datos_url <- url
  }
  # los calendarios necesitan conocer los cierres extraordinarios definidos por el usuario
  options(etl.feriados_extra = cfg$calendarios$feriados_extra)
  cfg
}

# Falla con un mensaje claro si la configuración está incompleta (en vez de ejecutar "en vacío").
validar_config <- function(cfg, ruta = "config.yaml") {
  if (!is.list(cfg) || length(cfg$activos) == 0) {
    stop(sprintf("La configuración %s no contiene 'activos' (¿archivo vacío o con formato YAML inválido?).", ruta), call. = FALSE)
  }
  for (a in cfg$activos) {
    if (is.null(a$ticker) || !is.character(a$ticker)) {
      stop("Cada activo necesita 'ticker' (texto). Revisa el YAML: evita claves y/n/yes/no sin comillas.", call. = FALSE)
    }
  }
  for (k in c("proyecto", "extraccion", "limpieza", "carga")) {
    if (is.null(cfg[[k]])) stop(sprintf("Falta la sección '%s' en %s.", k, ruta), call. = FALSE)
  }
  invisible(TRUE)
}

directorio_datos <- function(cfg) {
  base <- cfg$proyecto$directorio_datos %||% "data"
  es_absoluta <- grepl("^([A-Za-z]:|/|\\\\|~)", base)
  if (!es_absoluta) base <- file.path(cfg$.raiz %||% ETL_RAIZ, base)
  dir.create(base, recursive = TRUE, showWarnings = FALSE)
  base
}

# Devuelve la ruta del archivo SQLite (o NULL si la URL apunta a otra base de datos)
ruta_sqlite <- function(cfg) {
  url <- cfg$carga$base_datos_url
  if (is.null(url) || !startsWith(url, "sqlite:///")) return(NULL)
  ruta <- sub("^sqlite:///", "", url)
  if (ruta == ":memory:") return(ruta)
  es_absoluta <- grepl("^([A-Za-z]:|/|\\\\)", ruta)
  if (!es_absoluta) ruta <- file.path(cfg$.raiz %||% ETL_RAIZ, ruta)
  dir.create(dirname(ruta), recursive = TRUE, showWarnings = FALSE)
  ruta
}

obtener_secreto <- function(nombre) {
  valor <- Sys.getenv(nombre)
  if (!nzchar(valor)) {
    stop(sprintf(paste0("Falta la variable de entorno %s (defínela en tu entorno o en el archivo .env; ",
                        "no la escribas en el repositorio)."), nombre), call. = FALSE)
  }
  valor
}

activos <- function(cfg) cfg$activos %||% list()
