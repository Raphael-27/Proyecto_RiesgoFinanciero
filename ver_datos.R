# Muestra las primeras filas de lo que quedó en la base de datos.   Rscript ver_datos.R [--demo]
source("R/00_cargar.R")
cfg <- cargar_config()
if ("--demo" %in% commandArgs(trailingOnly = TRUE)) {
  cfg$proyecto$directorio_datos <- file.path(directorio_datos(cfg), "demo")
  cfg$carga$base_datos_url <- paste0("sqlite:///", file.path(cfg$proyecto$directorio_datos, "riesgo.db"))
}
con_conexion(cfg, function(con) {
  for (t in DBI::dbListTables(con)) {
    cat("\n== ", t, " (", DBI::dbGetQuery(con, paste("select count(*) n from", t))$n, " filas) ==\n", sep = "")
    print(utils::head(DBI::dbGetQuery(con, paste("select * from", t, "limit 5")), 5))
  }
})
