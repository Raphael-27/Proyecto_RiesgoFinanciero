skip_if_not_installed("glmnet"); skip_if_not_installed("ranger")

# Serie con (garch = TRUE) o sin (FALSE) agrupamiento de volatilidad -> curated
curada_sim <- function(n, garch, semilla = 11) {
  set.seed(semilla); r <- numeric(n); h <- numeric(n); h[1] <- 1e-4
  for (t in 2:n) { h[t] <- if (garch) 2e-6 + 0.09 * r[t - 1]^2 + 0.88 * h[t - 1] else 1e-4; r[t] <- sqrt(h[t]) * rnorm(1) }
  p <- 100 * exp(cumsum(r))
  raw <- data.frame(fecha = seq_dias_habiles(as.Date("2005-01-03"), n), open = p, high = p * 1.005, low = p * 0.995,
                    close = p, adj_close = p, volume = 1e6)
  calcular_caracteristicas(limpiar_serie(raw, "X")$df)
}
params_rapidos <- function() {
  p <- parametros_modelado(); p$ventana_inicial <- 800L; p$paso_refit <- 500L
  p$bosque$arboles <- 100L; p$bosque$arboles_ajuste <- 60L; p
}

# ------------------------------------------------------------------ etiquetas y variables
test_that("las etiquetas se calculan como define el documento", {
  cur <- curada_sim(600, TRUE); p <- parametros_modelado(); ds <- construir_dataset_modelo(cur, p)
  precio <- cur$adj_close
  for (t in c(50, 300, 595)) {
    esperada <- if (t + 5 <= nrow(cur)) as.numeric(precio[t + 5] / precio[t] - 1 <= -0.05) else NA
    expect_equal(ds$y_caida[t], esperada)
  }
  expect_true(all(is.na(ds$y_caida[596:600])))                       # sin futuro no hay etiqueta
  r <- cur$retorno_log
  expect_equal(ds$vol_fut[100], stats::sd(r[101:105]) * sqrt(252))
  expect_true(all(is.na(ds$y_vol_alta[1:254])))                      # el umbral exige historial mínimo
})

test_that("las variables de entrada y el umbral NO miran el futuro", {
  cur <- curada_sim(900, TRUE); p <- parametros_modelado()
  completo <- construir_dataset_modelo(cur, p)
  corte <- 700; parcial <- construir_dataset_modelo(cur[1:corte, ], p)
  cols <- columnas_features(completo)
  expect_equal(unlist(completo[corte, cols]), unlist(parcial[corte, cols]))
  expect_equal(completo$umbral_vol[corte], parcial$umbral_vol[corte])
  # y si el futuro cambia por completo, lo anterior no se altera
  cur2 <- cur; cur2$adj_close[701:900] <- cur2$adj_close[701:900] * seq(1, 5, length.out = 200)
  cur2$retorno_log <- retornos_log(cur2$adj_close)
  alterado <- construir_dataset_modelo(cur2, p)
  expect_equal(unlist(alterado[600, cols]), unlist(completo[600, cols]))
})

test_that("la purga debe cubrir el horizonte (si no, hay fuga de información)", {
  cfg <- list(modelado = list(horizonte_dias = 5, purga_dias = 3))
  expect_error(parametros_modelado(cfg), "purga_dias")
})

# ------------------------------------------------------------------------ particiones
test_that("walk-forward: el entrenamiento termina antes de la prueba, con purga", {
  p <- params_rapidos(); y <- c(rep(0, 2000), rep(NA, 5)); y[sample(2000, 200)] <- 1
  folds <- particiones_walk_forward(y, p); expect_gte(length(folds), 2)
  for (f in folds) {
    expect_lte(max(f$train), min(f$test) - p$purga_dias - 1)
    expect_equal(length(intersect(f$train, f$test)), 0)
    expect_false(any(is.na(y[c(f$train, f$test)])))
  }
  pruebas <- unlist(lapply(folds, `[[`, "test")); expect_false(anyDuplicated(pruebas) > 0)
  expect_false(is.unsorted(pruebas))
})

# ----------------------------------------------------------------------------- métricas
test_that("AUC, Brier y métricas de clasificación", {
  y <- c(0, 0, 1, 1)
  expect_equal(auc_roc(y, c(.1, .2, .8, .9)), 1); expect_equal(auc_roc(y, c(.9, .8, .2, .1)), 0)
  expect_equal(auc_roc(y, rep(.5, 4)), 0.5); expect_true(is.na(auc_roc(c(1, 1), c(.2, .3))))
  expect_equal(brier(y, c(0, 0, 1, 1)), 0); expect_equal(brier(y, rep(.5, 4)), .25)
  m <- metricas_clasificacion(c(1, 1, 0, 0, 0), c(TRUE, FALSE, TRUE, FALSE, FALSE))
  expect_equal(unname(m["precision"]), 0.5); expect_equal(unname(m["recall"]), 0.5)
})

test_that("el umbral de alerta respeta la tasa máxima y no alerta con probabilidades constantes", {
  set.seed(1); prob <- runif(1000); y <- rbinom(1000, 1, prob)
  u <- umbral_f1(prob, y, tasa_max = 0.25); expect_lte(mean(prob >= u), 0.26)
  expect_gt(umbral_f1(rep(0.3, 500), rbinom(500, 1, .3)), 1)        # inalcanzable: nunca alerta
})

test_that("la calibración de Platt reduce a la frecuencia histórica un modelo sin habilidad", {
  set.seed(2); y <- rbinom(500, 1, 0.1); prob_ruido <- runif(500)
  cal <- ajustar_platt(prob_ruido, y); p <- aplicar_platt(prob_ruido, cal)
  expect_lt(diff(range(p)), diff(range(prob_ruido)))                  # probabilidades más moderadas
  cal_neg <- ajustar_platt(1 - y * 0.9 + runif(500) * .1, y)           # anti-señal -> constante
  expect_equal(unname(cal_neg["b"]), 0)
})

test_that("con muy pocos casos positivos se usa la frecuencia histórica", {
  cur <- curada_sim(1500, FALSE); p <- params_rapidos(); ds <- construir_dataset_modelo(cur, p)
  filas <- which(!is.na(ds$y_caida)); y <- rep(0, length(filas)); y[1:3] <- 1
  mod <- ajustar_modelo("logistica", ds, filas, columnas_features(ds), y, p)
  expect_equal(mod$tipo, "constante"); expect_equal(predecir_modelo(mod, ds, filas[1:5]), rep(mean(y), 5))
})

test_that("marcar_recomendado solo recomienda modelos con habilidad positiva", {
  m <- data.frame(ticker = "X", objetivo = c("a", "a", "b", "b"), modelo = c("logistica", "bosque", "logistica", "bosque"),
                  habilidad_brier = c(0.05, 0.02, -0.01, -0.3))
  r <- marcar_recomendado(m)
  expect_equal(r$recomendado, c(TRUE, FALSE, FALSE, FALSE))
})

# ------------------------------------------ comportamiento del modelo (señal real vs. ruido)
test_that("el modelo detecta señal cuando existe (agrupamiento de volatilidad)", {
  p <- params_rapidos(); ds <- construir_dataset_modelo(curada_sim(3300, TRUE), p)
  wf <- walk_forward(ds, "y_vol_alta", "logistica", p); m <- resumir_metricas(wf)
  expect_gt(m$auc, 0.65); expect_gt(m$habilidad_brier, 0)             # mejor que repetir la frecuencia histórica
})

test_that("el modelo NO inventa habilidad cuando no hay señal (paseo aleatorio)", {
  p <- params_rapidos(); ds <- construir_dataset_modelo(curada_sim(3300, FALSE), p)
  for (tipo in c("logistica", "bosque")) {
    m <- resumir_metricas(walk_forward(ds, "y_vol_alta", tipo, p))
    expect_gt(m$auc, 0.40); expect_lt(m$auc, 0.62); expect_lt(m$habilidad_brier, 0.03)
  }
})

test_that("las predicciones fuera de muestra son probabilidades válidas y reproducibles", {
  p <- params_rapidos(); ds <- construir_dataset_modelo(curada_sim(2500, TRUE), p)
  a <- walk_forward(ds, "y_vol_alta", "logistica", p); b <- walk_forward(ds, "y_vol_alta", "logistica", p)
  expect_true(all(a$probabilidad >= 0 & a$probabilidad <= 1)); expect_equal(a$probabilidad, b$probabilidad)
  expect_false(is.unsorted(a$fecha)); expect_false(anyDuplicated(a$fecha) > 0)
})

# --------------------------------------------------------------------- etapa del pipeline
test_that("la etapa modelar guarda resultados, los carga y es repetible", {
  demo <- suppressMessages(preparar_demo(cfg_temporal(), file.path(tempfile("demo_"))))
  demo$modelado <- list(ventana_inicial = 300, paso_refit = 300, bosque = list(arboles = 60, arboles_ajuste = 40))
  suppressMessages(for (f in list(limpiar_datos, validar_calidad, transformar_datos, modelar_datos, cargar_datos)) f(fecha = "2026-11-02", config = demo))
  suppressMessages(cargar_datos(fecha = "2026-11-02", config = demo))               # segunda carga
  met <- leer_modelos(demo, "metricas"); expect_setequal(unique(met$ticker), c("SPY", "AAPL", "MSFT"))
  expect_true(all(c("auc", "auc_base", "habilidad_brier", "recomendado") %in% names(met)))
  con_conexion(demo, function(con) {
    q <- function(s) DBI::dbGetQuery(con, s)$n
    expect_equal(q("select count(*) n from metricas_modelos"), nrow(met))
    expect_equal(q("select count(*) n from predicciones where origen = 'actual'"), 3 * 2 * 2)  # activos x objetivos x modelos
    expect_equal(q("select count(*) n from (select ticker, fecha, objetivo, modelo, origen from predicciones group by 1,2,3,4,5 having count(*) > 1)"), 0)
  })
  actual <- leer_modelos(demo, "predicciones"); actual <- actual[actual$origen == "actual", ]
  expect_true(all(actual$probabilidad >= 0 & actual$probabilidad <= 1))
})
