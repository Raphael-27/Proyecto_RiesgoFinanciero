# Gráficas y exportación de los datos cargados.   Uso (desde la carpeta del proyecto):
#   Rscript visualizar.R                       # panel de riesgo por activo + comparación
#   Rscript visualizar.R --ticker SPY --abrir
#   Rscript visualizar.R --exportar            # además, CSV por activo (Excel / Power BI)
#   Rscript visualizar.R --demo                # usa la base de datos de la demostración
source("R/00_cargar.R")
tryCatch(main_visualizar(), error = function(e) { message(conditionMessage(e)); if (!interactive()) quit(status = 1) })
