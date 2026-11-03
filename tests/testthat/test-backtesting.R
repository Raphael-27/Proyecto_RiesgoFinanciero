skip_if_not_installed("glmnet"); skip_if_not_installed("ranger"); skip_if_not_installed("zoo")

# Serie con choques de volatilidad en las ventanas indicadas (el choque se aplica al retorno observado)
curada_crisis <- function(n, ventanas_choque, semilla = 5) {
  set.seed(semilla); dias <- seq_dias_habiles(as.Date("2005-01-03"), n)
  sh <- rep(1, n); for (w in ventanas_choque) sh[dias >= as.Date(w[1]) & dias <= as.Date(w[2])] <- 3.2
  r <- eps <- h <- numeric(n); h[1] <- 1e-4
  for (t in 2:n) { h[t] <- 2e-6 + 0.07 * eps[t - 1]^2 + 0.9 * h[t - 1]; eps[t] <- sqrt(h[t]) * rt(1, 6) / sqrt(1.5); r[t] <- eps[t] * sh[t] }
  p <- 100 * exp(cumsum(r))
  raw <- data.frame(fecha = dias, open = p, high = p * 1.003, low = p * 0.997, close = p, adj_close = p, volume = 1e6)
  calcular_caracteristicas(limpiar_serie(raw, "X")$df)
}

# --------------------------------------------------------------------- pruebas estadísticas
test_that("Kupiec coincide con el cálculo manual", {
  k <- kupiec_pof(5, 250, 0.01)
  expect_equal(unname(k["lr"]), 1.9568, tolerance = 1e-3); expect_equal(unname(k["p_valor"]), 0.1619, tolerance = 1e-3)
  expect_equal(unname(kupiec_pof(2.5, 250, 0.01)["lr"]), 0)                    # justo en la tasa esperada
  expect_equal(unname(kupiec_pof(0, 250, 0.01)["lr"]), -2 * 250 * log(0.99), tolerance = 1e-9)
  expect_lt(kupiec_pof(30, 250, 0.01)["p_valor"], 1e-10)                        # muy por encima: se rechaza
})

test_that("Christoffersen detecta violaciones agrupadas y no rechaza las independientes", {
  set.seed(1); expect_gt(christoffersen_ind(rbinom(3000, 1, 0.05))["p_valor"], 0.01)
  agrupadas <- c(rep(0, 200), rep(1, 10), rep(0, 200), rep(1, 10), rep(0, 180))
  expect_lt(christoffersen_ind(agrupadas)["p_valor"], 1e-6)
  expect_equal(unname(christoffersen_ind(rep(0, 100))["lr"]), 0)               # sin violaciones: nada que probar
})

test_that("semáforo de Basilea", {
  expect_equal(c(zona_basilea(4), zona_basilea(5), zona_basilea(9), zona_basilea(10)), c("verde", "amarilla", "amarilla", "roja"))
  expect_equal(maximo_ventana(c(rep(0, 100), rep(1, 6), rep(0, 200))), 6)
})

# ------------------------------------------------------------------------------ VaR
test_that("el VaR calculado en t se compara con el retorno de t+1 (sin mirar el futuro)", {
  n <- 400; d <- data.frame(fecha = seq_dias_habiles(as.Date("2020-01-01"), n), retorno_log = 0.001)
  d$var_hist_95 <- 0.02; d$var_hist_99 <- 0.03; d$var_param_95 <- 0.02; d$var_param_99 <- 0.03
  d$retorno_log[c(100, 200, 300)] <- -0.025                                    # superan el VaR 95 %, no el 99 %
  bp <- parametros_backtesting(); bp$crisis <- list()
  v <- backtest_var(d, "X", bp); fila <- v[v$metodo == "historico" & v$nivel == 0.95 & v$ventana == "Toda la muestra", ]
  expect_equal(fila$violaciones, 3)
  expect_equal(v$violaciones[v$metodo == "historico" & v$nivel == 0.99], 0)
})

test_that("el VaR histórico falla en las crisis y se cumple en calma", {
  cur <- curada_crisis(3500, list(c("2008-09-15", "2009-03-31")))
  bp <- parametros_backtesting(); v <- backtest_var(cur, "X", bp); h <- v[v$metodo == "historico" & v$nivel == 0.95, ]
  crisis <- h[h$ventana == "Crisis 2008", ]
  expect_gt(crisis$tasa_observada, 2 * crisis$tasa_esperada)                   # el VaR basado en el pasado calmado se queda corto
  expect_true(crisis$rechaza)
  expect_false("COVID-19" %in% h$ventana)                                       # los datos no llegan a 2020: se omite sin error
  expect_true(all(c("kupiec_p", "independencia_p", "cobertura_cond_p", "ratio_es") %in% names(h)))
})

# -------------------------------------------------------------------- modelos en crisis
test_that("los modelos de crisis se entrenan SOLO con datos anteriores a la ventana", {
  cur <- curada_crisis(3500, list(c("2008-09-15", "2009-03-31")))
  mp <- parametros_modelado(); mp$bosque$arboles <- 60L; mp$bosque$arboles_ajuste <- 40L
  bp <- parametros_backtesting(); cr <- backtest_crisis_modelos(cur, "X", mp, bp$crisis)
  expect_false(is.null(cr)); expect_equal(unique(cr$resumen$ventana), "Crisis 2008")
  idx_ini <- which(cur$fecha >= as.Date("2008-09-01"))[1]
  expect_lte(max(cr$resumen$n_entrenamiento), idx_ini - mp$purga_dias - 1)      # nunca ve la crisis (menos purga)
  expect_true(all(cr$series$fecha >= as.Date("2008-09-01") & cr$series$fecha <= as.Date("2009-06-30")))
  expect_true("base" %in% cr$resumen$modelo)                                    # siempre se compara con la regla sencilla
  vol <- cr$resumen[cr$resumen$objetivo == "y_vol_alta" & cr$resumen$modelo == "logistica", ]
  expect_gt(vol$auc, 0.6); expect_true(!is.na(vol$primera_alerta))              # detecta el choque de volatilidad
  expect_gte(vol$dias_desde_inicio, 0)
})

test_that("si los datos no cubren ninguna crisis, se omite sin error", {
  cur <- curada_crisis(600, list()); mp <- parametros_modelado(); bp <- parametros_backtesting()   # termina en 2007: antes de ambas crisis
  expect_null(backtest_crisis_modelos(cur, "X", mp, bp$crisis))
})

# ------------------------------------------------------------------------- incertidumbre
test_that("el bootstrap por bloques distingue habilidad real de ruido", {
  set.seed(4); n <- 1500; z <- rnorm(n); y <- rbinom(n, 1, plogis(-2 + 1.5 * z))
  fuerte <- data.frame(ticker = "X", objetivo = "o", modelo = "m", fecha = as.Date("2010-01-01") + seq_len(n), y_real = y,
                       probabilidad = plogis(-2 + 1.5 * z), score_base = rnorm(n), umbral = .2, ajuste = "m")
  r <- backtest_incertidumbre(fuerte, bloque = 21, B = 200)
  expect_true(r$supera_base); expect_true(r$habilidad_significativa)
  expect_true(r$auc >= r$auc_ic_inf && r$auc <= r$auc_ic_sup)                   # el estimado cae dentro de su IC
  ruido <- fuerte; ruido$probabilidad <- runif(n); ruido$score_base <- runif(n)
  r2 <- backtest_incertidumbre(ruido, bloque = 21, B = 200)
  expect_false(r2$habilidad_significativa); expect_lte(r2$dif_ic_inf, 0); expect_gte(r2$dif_ic_sup, 0)   # el IC incluye 0
  expect_null(backtest_incertidumbre(fuerte[1:50, ], bloque = 21, B = 50))       # muy pocos datos
})

# --------------------------------------------------------------------- etapa del pipeline
test_that("la etapa backtest guarda resultados, dibuja y carga a la base", {
  skip_if_not_installed("ggplot2")
  demo <- suppressMessages(preparar_demo(cfg_temporal(), file.path(tempfile("demo_"))))
  demo$modelado <- list(ventana_inicial = 300, paso_refit = 300, bosque = list(arboles = 40, arboles_ajuste = 30))
  demo$backtesting <- list(remuestreos = 50)
  suppressMessages(for (f in list(limpiar_datos, validar_calidad, transformar_datos, modelar_datos, backtest_datos, cargar_datos)) f(fecha = "2026-11-02", config = demo))
  v <- leer_tabla(demo, "backtesting", "backtest_var")
  expect_setequal(unique(v$ticker), c("SPY", "AAPL", "MSFT")); expect_true(all(v$ventana == "Toda la muestra"))   # el demo (2022-2025) no incluye crisis
  expect_true(file.exists(file.path(directorio_datos(demo), "graficas", "backtest_var_SPY.png")))
  con_conexion(demo, function(con) {
    expect_equal(DBI::dbGetQuery(con, "select count(*) n from backtest_var")$n, nrow(v))
    expect_true(DBI::dbExistsTable(con, "backtest_modelos_ic"))
  })
})
