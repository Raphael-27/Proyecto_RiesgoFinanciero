# Muestra los resultados del backtesting.   Rscript ver_backtest.R [--demo]
source("R/00_cargar.R")
cfg <- cargar_config()
if ("--demo" %in% commandArgs(trailingOnly = TRUE)) cfg$proyecto$directorio_datos <- file.path(directorio_datos(cfg), "demo")
v <- leer_tabla(cfg, "backtesting", "backtest_var")
if (is.null(v)) stop("Aún no hay backtesting. Ejecuta:  Rscript pipeline.R --etapas backtest cargar", call. = FALSE)
pv <- function(x) ifelse(is.na(x), "-", ifelse(x < 0.001, "<0.001", sprintf("%.3f", x)))

cat("\n== 1. Backtesting del VaR ==\n")
cat("Un VaR al 99% debe ser superado ~1% de los días y sin agruparse. 'RECHAZADO' = p < 0.05 en la prueba conjunta.\n\n")
t1 <- data.frame(activo = v$ticker, metodo = v$metodo, nivel = sprintf("%d%%", round(100 * v$nivel)), periodo = v$ventana,
                 dias = v$n, violaciones = v$violaciones, observada = sprintf("%.1f%%", 100 * v$tasa_observada),
                 esperada = sprintf("%.1f%%", 100 * v$tasa_esperada), `p Kupiec` = pv(v$kupiec_p), `p indep.` = pv(v$independencia_p),
                 resultado = ifelse(v$rechaza, "RECHAZADO", "ok"), `semaforo Basilea` = ifelse(is.na(v$zona_basilea), "-", v$zona_basilea),
                 check.names = FALSE)
print(t1[v$metodo == "historico", ], row.names = FALSE)

cr <- leer_tabla(cfg, "backtesting", "backtest_crisis")
cat("\n== 2. Modelos entrenados ANTES de cada crisis ==\n")
if (is.null(cr)) cat("Los datos no cubren las ventanas de crisis configuradas.\n") else {
  cat("'base' = regla sencilla. Habilidad = mejora del Brier sobre la frecuencia aprendida antes de la crisis.\n")
  cat("Anticipacion = dias entre la primera alerta y el peor dia (retorno a 5 dias) de la ventana.\n\n")
  t2 <- data.frame(activo = cr$ticker, crisis = cr$ventana, evento = unname(NOMBRE_OBJETIVO[cr$objetivo]), modelo = cr$modelo,
                   entrenado_con = cr$n_entrenamiento, dias = cr$n_prueba, AUC = round(cr$auc, 3),
                   habilidad = ifelse(is.na(cr$habilidad_brier), "-", sprintf("%+.0f%%", 100 * cr$habilidad_brier)),
                   alertas = cr$alertas, precision = round(cr$precision, 2), exhaustividad = round(cr$exhaustividad, 2),
                   `primera alerta` = ifelse(is.na(cr$primera_alerta), "ninguna", format(cr$primera_alerta)),
                   anticipacion = cr$anticipacion_dias, check.names = FALSE)
  print(t2, row.names = FALSE)
}

ic <- leer_tabla(cfg, "backtesting", "backtest_modelos_ic")
cat("\n== 3. Incertidumbre: ¿el modelo supera de verdad a la regla base? (bootstrap por bloques, IC 95%) ==\n")
if (is.null(ic)) cat("Sin datos suficientes.\n") else {
  f <- function(x, lo, hi) sprintf("%.3f [%.3f, %.3f]", x, lo, hi)
  t3 <- data.frame(activo = ic$ticker, evento = unname(NOMBRE_OBJETIVO[ic$objetivo]), modelo = ic$modelo,
                   AUC = f(ic$auc, ic$auc_ic_inf, ic$auc_ic_sup), `AUC base` = round(ic$auc_base, 3),
                   `AUC - base` = f(ic$dif_auc, ic$dif_ic_inf, ic$dif_ic_sup),
                   `habilidad Brier` = sprintf("%+.1f%% [%+.1f, %+.1f]", 100 * ic$habilidad_brier, 100 * ic$hb_ic_inf, 100 * ic$hb_ic_sup),
                   `supera a la base` = ifelse(ic$supera_base, "SI", "no"), `habilidad real` = ifelse(ic$habilidad_significativa, "SI", "no"),
                   check.names = FALSE)
  print(t3, row.names = FALSE)
  cat("\n'supera a la base' = el IC de la diferencia de AUC queda por encima de 0. 'habilidad real' = el IC de la habilidad de Brier queda por encima de 0.\n")
}
cat("\nGraficas: data/graficas/backtest_var_*.png y backtest_crisis_*.png\n")
