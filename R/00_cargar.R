# Carga todos los módulos del ETL. Uso:  source("R/00_cargar.R")  (desde la raíz del proyecto)

.etl_raiz <- function() {
  # La raíz es la carpeta que contiene R/ y config/ (funciona desde RStudio, Rscript o los tests).
  args <- commandArgs(FALSE)
  script <- sub("^--file=", "", grep("^--file=", args, value = TRUE))
  carpeta_script <- if (length(script)) dirname(normalizePath(script[1], mustWork = FALSE)) else character()
  candidatos <- c(carpeta_script, getwd(), file.path(getwd(), ".."), file.path(getwd(), "..", ".."))
  for (c in candidatos) {
    if (file.exists(file.path(c, "R", "00_cargar.R"))) return(normalizePath(c))
  }
  stop("Ejecuta desde la carpeta del proyecto (la que contiene R/ y config/).")
}

ETL_RAIZ <- .etl_raiz()

# Paquetes imprescindibles: si falta alguno, se indica cómo instalarlo (en vez de un error críptico).
.faltan <- Filter(function(p) !requireNamespace(p, quietly = TRUE),
                  c("yaml", "DBI", "RSQLite", "jsonlite", "timeDate", "zoo", "httr2"))
if (length(.faltan)) {
  stop(sprintf("Faltan paquetes de R: %s.\nInstálalos ejecutando:  Rscript instalar_paquetes.R   (o source('instalar_paquetes.R') en RStudio)",
               paste(.faltan, collapse = ", ")), call. = FALSE)
}
rm(.faltan)
for (f in c("utils.R", "config.R", "calendario.R", "limpieza.R", "calidad.R", "transformacion.R",
            "modelado.R", "backtesting.R", "almacen.R", "carga.R", "extraccion.R", "tareas.R", "sintetico.R", "visualizar.R")) {
  ruta <- file.path(ETL_RAIZ, "R", f)
  if (file.exists(ruta)) {
    # parse() marca los textos como UTF-8 sin depender de la configuración regional del equipo
    # (source(encoding=) falla en servidores/contenedores con locale "C").
    for (e in parse(file = ruta, encoding = "UTF-8", keep.source = FALSE)) eval(e, envir = globalenv())
  }
}
