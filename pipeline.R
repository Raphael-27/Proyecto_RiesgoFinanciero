# Ejecuta el ETL.   Uso (desde la carpeta del proyecto):
#   Rscript pipeline.R                      # todas las etapas con config/config.yaml
#   Rscript pipeline.R --sin-macro          # solo precios de Yahoo (no necesita FRED_API_KEY)
#   Rscript pipeline.R --demo               # datos sintéticos, sin APIs ni claves
#   Rscript pipeline.R --etapas limpiar validar transformar cargar    # re-procesar sin descargar
source("R/00_cargar.R")
estado <- main_pipeline()
if (!interactive()) quit(status = estado)
