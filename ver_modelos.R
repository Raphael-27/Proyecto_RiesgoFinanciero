# Muestra cómo se comportaron los modelos y cuál es la señal de hoy.   Rscript ver_modelos.R [--demo]
source("R/00_cargar.R")
cfg <- cargar_config()
if ("--demo" %in% commandArgs(trailingOnly = TRUE)) cfg$proyecto$directorio_datos <- file.path(directorio_datos(cfg), "demo")
met <- leer_modelos(cfg, "metricas"); pred <- leer_modelos(cfg, "predicciones")
if (is.null(met)) stop("Aún no hay modelos. Ejecuta:  Rscript pipeline.R --etapas modelar cargar", call. = FALSE)

tabla <- data.frame(
  activo = met$ticker, evento = unname(NOMBRE_OBJETIVO[met$objetivo]), modelo = met$modelo,
  casos = met$n, frecuencia = sprintf("%.1f%%", 100 * met$prevalencia),
  AUC = round(met$auc, 3), `AUC base` = round(met$auc_base, 3),
  `habilidad Brier` = sprintf("%+.1f%%", 100 * met$habilidad_brier),
  precision = round(met$precision, 3), exhaustividad = round(met$recall, 3),
  recomendado = ifelse(met$recomendado, "SI", "-"), check.names = FALSE)
cat("\n== Desempeño fuera de muestra (validación walk-forward) ==\n")
cat("AUC base = línea base sencilla (persistencia de la volatilidad / VaR pasado).\n")
cat("Habilidad Brier > 0 = el modelo pronostica mejor que repetir la frecuencia histórica.\n\n")
print(tabla, row.names = FALSE)

act <- pred[pred$origen == "actual", ]
senal <- data.frame(
  activo = act$ticker, fecha = format(act$fecha), evento = unname(NOMBRE_OBJETIVO[act$objetivo]), modelo = act$modelo,
  probabilidad = sprintf("%.1f%%", 100 * act$probabilidad), `umbral de alerta` = ifelse(act$umbral > 1, "sin alerta", sprintf("%.1f%%", 100 * act$umbral)),
  alerta = ifelse(act$probabilidad >= act$umbral, "ALERTA", "-"), check.names = FALSE)
cat("\n== Señal más reciente ==\n\n"); print(senal, row.names = FALSE)
cat("\nUsa como referencia solo los modelos marcados como recomendado; si ninguno lo está, ninguno superó a la frecuencia histórica.\n")
