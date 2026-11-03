# Instala los paquetes de R que necesita el proyecto (solo los que falten).
#   Rscript instalar_paquetes.R     o, en RStudio:  source("instalar_paquetes.R")
paquetes <- c("yaml", "DBI", "RSQLite", "jsonlite", "timeDate", "zoo", "httr2", "quantmod",
              "ggplot2", "patchwork", "scales", "glmnet", "ranger", "testthat", "withr")

# En Windows la carpeta de paquetes del sistema (C:/Program Files/R/...) suele estar protegida.
# Si no se puede escribir en ella, se usa la carpeta personal del usuario (R_LIBS_USER), que R
# incluye automáticamente en las ejecuciones siguientes.
es_escribible <- function(d) {
  if (!dir.exists(d)) return(FALSE)
  f <- file.path(d, paste0(".prueba_", Sys.getpid()))
  ok <- suppressWarnings(file.create(f))
  if (ok) unlink(f)
  ok
}
if (!es_escribible(.libPaths()[1])) {
  lib <- Sys.getenv("R_LIBS_USER")
  if (!nzchar(lib)) lib <- file.path(path.expand("~"), "R", "library")
  dir.create(lib, recursive = TRUE, showWarnings = FALSE)
  .libPaths(c(lib, .libPaths()))
  cat("La carpeta de paquetes del sistema no es escribible. Se usará tu carpeta personal:\n  ", lib, "\n")
}

faltan <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltan) == 0) {
  cat("Todos los paquetes ya están instalados.\n")
} else {
  cat("Instalando:", paste(faltan, collapse = ", "), "\n")
  install.packages(faltan, lib = .libPaths()[1], repos = "https://cloud.r-project.org")
  quedan <- faltan[!vapply(faltan, requireNamespace, logical(1), quietly = TRUE)]
  if (length(quedan)) stop("No se pudieron instalar: ", paste(quedan, collapse = ", "), call. = FALSE)
  cat("Listo: todos los paquetes quedaron instalados.\n")
}
