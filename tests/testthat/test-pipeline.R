ACTIVO <- list(ticker = "T", calendario = NULL, zona_horaria = "America/New_York", cierre = "16:00")
limpia <- function() { s <- serie_sintetica(); limpiar_serie(s$raw, "T", fuente = "sintetico") }
res_por_dim <- function(res) stats::setNames(res, vapply(res, function(r) r$dimension, character(1)))

# ------------------------------------------------------------- características de riesgo
test_that("VaR y ES históricos coinciden con el cálculo manual", {
  set.seed(3); r <- rnorm(600, 0, 0.01); out <- var_es_historico(r, 250, c(0.95, 0.99))
  w <- tail(r, 250); q <- quantile(w, 0.05, names = FALSE)
  expect_equal(tail(out$var_hist_95, 1), -q); expect_equal(tail(out$es_hist_95, 1), -mean(w[w <= q]))
  expect_gt(tail(out$var_hist_99, 1), tail(out$var_hist_95, 1))
  expect_true(all(out$es_hist_95 >= out$var_hist_95, na.rm = TRUE))
  expect_true(all(is.na(out$var_hist_95[1:249])))             # sin ventana completa no se calcula
})

test_that("VaR paramétrico cercano al teórico", {
  set.seed(5); r <- rnorm(5000, 0, 0.02)
  expect_equal(tail(var_parametrico(r, 1000)$var_param_95, 1), 1.645 * 0.02, tolerance = 0.1)
})

test_that("las características no miran el futuro", {
  df <- limpia()$df; completa <- calcular_caracteristicas(df)
  corte <- df$fecha[600]; parcial <- calcular_caracteristicas(df[df$fecha <= corte, ])
  cols <- setdiff(names(completa), c("ticker", "fecha"))
  expect_equal(unlist(completa[completa$fecha == corte, cols]), unlist(parcial[parcial$fecha == corte, cols]))
})

test_that("el z-score excluye el dato actual", {
  expect_true(is.na(zscore_movil(c(rep(0, 300), 5), 252)[10]))
  set.seed(0); x <- rnorm(301); x[301] <- 20
  expect_gt(zscore_movil(x, 252)[301], 10)
})

# -------------------------------------------------------------------------------- calidad
test_that("la serie sintética cumple la Tabla 3", {
  l <- limpia(); res <- evaluar_calidad(l$df, l$reporte, ACTIVO)
  expect_silent(exigir_calidad(list(T = res))); expect_length(alertas(list(T = res)), 0)
  expect_setequal(names(res_por_dim(res)), c("completitud", "unicidad", "validez", "consistencia", "oportunidad", "impacto_limpieza"))
})

test_that("un hueco largo se detecta y bloquea el flujo", {
  l <- limpia(); res <- evaluar_calidad(l$df[-(300:305), ], l$reporte, ACTIVO)
  expect_error(exigir_calidad(list(T = res)), "consistencia", class = "error_calidad")
})

test_that("duplicados e incoherentes residuales se detectan", {
  l <- limpia(); malo <- rbind(l$df, l$df[10, ]); malo$close[5] <- -1
  res <- res_por_dim(evaluar_calidad(malo, l$reporte, ACTIVO))
  expect_false(res$unicidad$cumple); expect_false(res$validez$cumple)
})

test_that("la completitud con calendario penaliza los días rellenados", {
  dias <- dias_esperados("2024-01-02", "2024-06-28", "NYSE")
  raw <- serie_sintetica(dias, con_anomalias = FALSE)$raw; raw[41:44, PRECIOS] <- NA
  r <- limpiar_serie(raw, "T", calendario = "NYSE")
  res <- res_por_dim(evaluar_calidad(r$df, r$reporte, c(ACTIVO[-2], list(calendario = "NYSE"))))
  expect_lt(res$completitud$valor, 0.99); expect_false(res$completitud$cumple)
})

test_that("la oportunidad mide horas desde el cierre (el cron de las 22:00 del documento incumple)", {
  l <- limpia(); hoy <- format(max(l$df$fecha)); cc <- list(evaluar_oportunidad = TRUE)
  ny <- function(h, m) as.POSIXct(sprintf("%s %02d:%02d:00", hoy, h, m), tz = "America/New_York")
  op <- function(a) res_por_dim(evaluar_calidad(l$df, l$reporte, ACTIVO, cc, ahora = a))$oportunidad
  expect_equal(op(ny(17, 30))$valor, 1.5); expect_true(op(ny(17, 30))$cumple)
  expect_equal(op(ny(22, 0))$valor, 6); expect_false(op(ny(22, 0))$cumple)
})

test_that("perfilado de la serie cruda", {
  p <- perfilar(serie_sintetica()$raw, "T")
  expect_equal(p$filas, 1002); expect_equal(p$fechas_duplicadas, 2); expect_equal(p$frecuencia, "diaria")
  expect_equal(p$cobertura$dias_recibidos, 1000); expect_lt(p$completitud_columnas$close, 1)
})

# ----------------------------------------------------------------------------------- carga
test_that("la carga es idempotente y la tabla evoluciona sin perder datos", {
  cfg <- cfg_temporal(); df <- limpia()$df
  con_conexion(cfg, function(con) {
    n1 <- cargar_dataframe(con, "precios_limpios", df, "T"); n2 <- cargar_dataframe(con, "precios_limpios", df, "T")
    cargar_dataframe(con, "precios_limpios", df, "OTRO")
    cuenta <- function(w = "") DBI::dbGetQuery(con, paste("select count(*) n from precios_limpios", w))$n
    expect_equal(c(n1, n2), c(1000, 1000)); expect_equal(cuenta("where ticker='T'"), 1000); expect_equal(cuenta(), 2000)
    cargar_dataframe(con, "precios_limpios", cbind(df, nueva_variable = 1.5), "T")      # columna nueva
    expect_equal(DBI::dbGetQuery(con, "select count(nueva_variable) n from precios_limpios where ticker='T'")$n, 1000)
    expect_equal(cuenta(), 2000)
  })
})

test_that("el índice único impide duplicados", {
  cfg <- cfg_temporal()
  con_conexion(cfg, function(con) {
    cargar_dataframe(con, "precios_limpios", limpia()$df, "T")
    expect_error(DBI::dbExecute(con, "insert into precios_limpios (ticker, fecha) select ticker, fecha from precios_limpios limit 1"))
  })
})

# ------------------------------------------------------------------------ pipeline completo
etapas_proceso <- function(cfg, fecha = "2026-11-02") {
  suppressMessages(for (f in list(limpiar_datos, validar_calidad, transformar_datos, cargar_datos)) f(fecha = fecha, config = cfg))
}

test_that("el pipeline completo funciona y se puede repetir", {
  demo <- suppressMessages(preparar_demo(cfg_temporal(), file.path(tempfile("demo_"))))
  etapas_proceso(demo); etapas_proceso(demo)                  # segunda ejecución: mismo resultado
  con_conexion(demo, function(con) {
    q <- function(s) DBI::dbGetQuery(con, s)$n
    expect_equal(q("select count(*) n from precios_limpios"), 3000); expect_equal(q("select count(*) n from caracteristicas"), 3000)
    expect_equal(q("select count(*) n from reportes_calidad"), 3); expect_equal(q("select count(macro_vix) n from caracteristicas"), 3000)
  })
  clean <- leer_clean(demo, "SPY"); expect_equal(nrow(clean), 1000); expect_false(is.unsorted(clean$fecha))
})

test_that("la calidad detiene el flujo antes de la carga", {
  demo <- suppressMessages(preparar_demo(cfg_temporal(), file.path(tempfile("demo_"))))
  suppressMessages(limpiar_datos(fecha = "2026-11-02", config = demo))
  limpio <- leer_clean(demo, "SPY"); guardar_clean(limpio[-(201:210), ], demo, "SPY")      # hueco de 10 sesiones
  expect_error(suppressMessages(validar_calidad(fecha = "2026-11-02", config = demo)), "SPY", class = "error_calidad")
  con_conexion(demo, function(con) expect_false(DBI::dbExistsTable(con, "precios_limpios")))   # nada llegó a la base
})

test_that("extraer: con clientes simulados y fallos parciales", {
  cfg <- cfg_temporal(); cfg$macro$habilitado <- FALSE; raw <- serie_sintetica()$raw
  cliente <- list(descargar = function(ticker, inicio) {
    if (ticker == "MSFT") stop(error_api("símbolo inexistente")); raw })
  expect_error(suppressMessages(extraer_datos(config = cfg, clientes = list(yahoo = cliente))), "MSFT", class = "error_api")
  expect_equal(nrow(leer_raw_ultimo(cfg, "yahoo", "SPY")), 1002)                  # los demás sí quedaron guardados
  cfg$extraccion$tolerar_fallos_parciales <- TRUE
  res <- suppressMessages(extraer_datos(config = cfg, clientes = list(yahoo = cliente)))
  expect_equal(names(res$fallidos), "MSFT")
})

test_that("la capa raw conserva los datos originales (duplicados y anomalías incluidos)", {
  cfg <- cfg_temporal(); raw <- serie_sintetica()$raw
  guardar_raw(raw, cfg, "yahoo", "SPY"); leido <- leer_raw_ultimo(cfg, "yahoo", "SPY")
  expect_equal(nrow(leido), 1002); expect_true(anyDuplicated(leido$fecha) > 0)
  expect_equal(leido$close, raw$close, tolerance = 1e-9)
})

# --------------------------------------------------------------------------- configuración
test_that("una configuración sin activos falla con un mensaje claro (no se ejecuta en vacío)", {
  ruta <- tempfile(fileext = ".yaml"); writeLines("proyecto: {directorio_datos: data}\nactivos: []", ruta)
  expect_error(cargar_config(ruta), "no contiene 'activos'")
})

test_that("el YAML con acentos se lee completo aunque el locale no sea UTF-8", {
  cfg <- cargar_config()
  expect_gte(length(cfg$activos), 3); expect_equal(cfg$activos[[1]]$cierre, "16:00"); expect_equal(cfg$limpieza$tolerancia_ohlc, 1e-6)
})

test_that("los argumentos de la línea de comandos se interpretan", {
  a <- parsear_argumentos(c("--demo", "--etapas", "limpiar", "validar", "--fecha", "2026-11-02"))
  expect_true(a$demo); expect_equal(a$etapas, c("limpiar", "validar")); expect_equal(a$fecha, "2026-11-02")
  expect_error(parsear_argumentos("--inventado"), "desconocido"); expect_error(parsear_argumentos(c("--etapas", "volar")), "extraer, limpiar, validar")
})

test_that("ETL_DATOS mueve datos y base de datos fuera del proyecto", {
  fuera <- tempfile("datos_")
  withr::local_envvar(c(ETL_DATOS = fuera, DATABASE_URL = ""))
  cfg <- cargar_config()
  expect_equal(cfg$proyecto$directorio_datos, fuera)
  expect_equal(ruta_sqlite(cfg), file.path(fuera, "riesgo.db"))
  expect_true(startsWith(directorio_datos(cfg), fuera)); expect_true(dir.exists(fuera))
  withr::local_envvar(c(ETL_DATOS = fuera, DATABASE_URL = "sqlite:///otra.db"))          # DATABASE_URL tiene prioridad
  expect_match(ruta_sqlite(cargar_config()), "otra\\.db$")
})

test_that("si faltan los datos, el error explica qué pasó y cómo arreglarlo", {
  cfg <- cfg_temporal()
  e <- tryCatch(leer_curada(cfg, "SPY"), error = function(e) conditionMessage(e))
  expect_match(e, "pipeline.R --sin-macro"); expect_match(e, "ETL_DATOS"); expect_match(e, directorio_datos(cfg), fixed = TRUE)
})
