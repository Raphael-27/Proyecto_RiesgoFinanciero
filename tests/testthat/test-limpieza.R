# ------------------------------------------------------------------ Tabla 4 del documento
test_that("Tabla 4: la serie sintética se limpia como describe el documento", {
  s <- serie_sintetica(); r <- limpiar_serie(s$raw, "SINT"); rep <- r$reporte; df <- r$df; base <- s$base

  expect_equal(c(rep$filas_entrada, rep$filas_salida), c(1002, 1000))
  expect_equal(rep$duplicados, 2)
  expect_equal(rep$incoherentes, 2)
  expect_equal(rep$errores_dato, 1)
  expect_equal(rep$filas_sin_precio, 0)          # los 2 nulos consecutivos se cubren, no se pierden filas

  spike <- base$fecha[POS$error_captura]; caida <- base$fecha[POS$caida]
  expect_true(df$error_dato[df$fecha == spike])
  # el error de captura se reemplaza por el último precio válido
  expect_equal(df$adj_close[df$fecha == spike], base$adj_close[POS$error_captura - 1])
  # la caída persistente del 30 % se marca pero se conserva
  expect_true(df$es_atipico[df$fecha == caida]); expect_false(df$error_dato[df$fecha == caida])
  expect_equal(tail(df$adj_close, 1), tail(base$adj_close, 1) * 0.7)
  expect_false(is.unsorted(df$fecha)); expect_false(anyDuplicated(df$fecha) > 0)
  expect_true(all(df[PRECIOS] > 0) && !anyNA(df[PRECIOS]))
})

test_that("duplicados: se conserva el último registro recibido", {
  s <- serie_sintetica(); df <- limpiar_serie(s$raw, "SINT")$df
  for (pos in POS$duplicados) {
    d <- s$base$fecha[pos]
    expect_equal(df$close[df$fecha == d], s$base$close[pos] * 1.001)
  }
})

test_that("duplicados: el orden de llegada se respeta con muchas filas", {
  df <- data.frame(fecha = as.Date(c(rep("2024-01-02", 50), rep("2024-01-03", 50))), close = 1:100, adj_close = 1:100)
  r <- eliminar_duplicados(estandarizar_esquema(df)$df)
  expect_equal(r$n, 98); expect_equal(r$df$close, c(50, 100))
})

test_that("una caída persistente no se elimina, pero un salto revertido sí es error", {
  set.seed(1); n <- 300; dias <- dias_bd("2020-01-01", "2021-06-30")[1:n]
  p <- 100 * exp(cumsum(rnorm(n, 0, 0.01))); p[151:n] <- p[151:n] * 0.65; p[221] <- p[221] * 8
  out <- marcar_atipicos(data.frame(fecha = dias, close = p, adj_close = p))
  expect_true(out$es_atipico[151]); expect_false(out$error_dato[151])
  expect_true(out$error_dato[221])
})

salto_en_v <- function(r1, r2, volumen_dia = NULL, n = 300, pos = 201) {
  set.seed(11); r <- rnorm(n, 0, 0.008); r[pos] <- r1; r[pos + 1] <- r2
  p <- 100 * exp(cumsum(r)); vol <- rep(1e6, n); if (!is.null(volumen_dia)) vol[pos] <- volumen_dia
  list(df = data.frame(fecha = dias_bd("2019-01-01", "2020-12-31")[1:n], close = p, adj_close = p, volume = vol), pos = pos)
}

test_that("un rebote en V de crisis real (12-13/mar/2020: -10 % y +8,5 %) NO es error de dato", {
  s <- salto_en_v(-0.10, 0.0855); o <- marcar_atipicos(s$df)
  expect_true(o$es_atipico[s$pos]); expect_false(o$error_dato[s$pos])
})
test_that("un salto enorme con volumen disparado es un movimiento real", {
  s <- salto_en_v(-0.7, 0.69, volumen_dia = 8e6); o <- marcar_atipicos(s$df)
  expect_true(o$es_atipico[s$pos]); expect_false(o$error_dato[s$pos])
})
test_that("un salto enorme sin volumen sí es error de captura", {
  s <- salto_en_v(-0.7, 0.69); expect_true(marcar_atipicos(s$df)$error_dato[s$pos])
})

# ------------------------------------------------------------------- esquema y fechas
test_that("el esquema exige una columna de precio", {
  expect_error(estandarizar_esquema(data.frame(fecha = as.Date("2024-01-02"), x = 1)), "close")
})

test_that("texto en columnas numéricas pasa a NA y se cuenta", {
  df <- data.frame(Fecha = c("2024-01-02", "2024-01-03", "2024-01-04"), Close = c("10.5", "abc", "11"),
                   `Adj Close` = c(10.5, 10.6, 11), check.names = FALSE)
  e <- estandarizar_esquema(df)
  expect_equal(sum(is.na(e$df$close)), 1); expect_equal(e$stats$valores_no_numericos, 1)
})

test_that("la fecha local del mercado no se corre de día (Tokio)", {
  x <- as.POSIXct("2024-03-05 00:00:00", tz = "Asia/Tokyo")
  e <- estandarizar_esquema(data.frame(fecha = x, close = 1), "Asia/Tokyo")
  expect_equal(e$df$fecha, as.Date("2024-03-05"))
})

test_that("zonas horarias mezcladas y fechas inválidas", {
  f <- c("2024-03-05 00:00:00-05:00", "2024-03-06", "2024-03-07T00:00:00+00:00", "no-es-fecha")
  e <- estandarizar_esquema(data.frame(fecha = f, close = c(1, 2, 3, 4)), "America/New_York")
  expect_equal(e$stats$fechas_invalidas, 1)
  # 00:00 UTC = 19:00 del día previo en Nueva York
  expect_equal(e$df$fecha, as.Date(c("2024-03-05", "2024-03-06", "2024-03-06")))
})

test_that("sin adj_close se usa close y se informa", {
  e <- estandarizar_esquema(data.frame(fecha = as.Date("2024-01-02"), close = 1))
  expect_equal(e$df$adj_close, 1); expect_true(e$stats$adj_close_es_close)
})

# --------------------------------------------------------------- calendario y huecos
test_that("se excluyen fines de semana y feriados", {
  df <- data.frame(fecha = as.Date(c("2024-07-03", "2024-07-04", "2024-07-06", "2024-07-08")), close = 1)
  r <- excluir_no_habiles(df, "NYSE")                         # 4-jul feriado, 6-jul sábado
  expect_equal(r$n, 2); expect_equal(format(r$df$fecha, "%d"), c("03", "08"))
  r2 <- excluir_no_habiles(df, NULL)                          # sin calendario: solo fin de semana
  expect_equal(r2$n, 1); expect_true(as.Date("2024-07-04") %in% r2$df$fecha)
})

test_that("el calendario conoce cierres extraordinarios (Carter 2025, Sandy 2012, 11-S)", {
  esp <- dias_esperados("2025-01-06", "2025-01-10", "NYSE")
  expect_false(as.Date("2025-01-09") %in% esp)
  expect_false(as.Date("2012-10-29") %in% dias_esperados("2012-10-26", "2012-10-31", "NYSE"))
  expect_false(as.Date("2001-09-11") %in% dias_esperados("2001-09-10", "2001-09-12", "NYSE"))
  expect_equal(length(dias_esperados("2024-01-01", "2024-01-07", "24/7")), 7)
  expect_error(dias_esperados("2024-01-01", "2024-01-07", "MARTE"), "no soportado")
})

test_that("un hueco corto se completa y uno largo se descarta", {
  dias <- dias_dias <- dias_esperados("2024-01-02", "2024-02-29", "NYSE")
  base <- data.frame(fecha = dias, close = 100, adj_close = 100, open = 100, high = 101, low = 99)
  base <- base[-c(6, 7, 21:25), ]                              # hueco de 2 días y otro de 5
  c1 <- completar_calendario(base, "NYSE"); expect_gt(c1$n, 0)
  out <- tratar_nulos(cbind(c1$df, es_atipico = FALSE, error_dato = FALSE), 3)$df
  expect_true(all(out$relleno[out$fecha %in% dias[6:7]]))      # 2 días: cubiertos
  expect_false(any(dias[24:25] %in% out$fecha))                # 5 días: solo 3 se cubren
  expect_true(out$relleno[out$fecha == dias[23]])
})

test_that("no se rellena hacia atrás", {
  df <- data.frame(fecha = as.Date(c("2024-01-02", "2024-01-03")), close = c(NA, 10), adj_close = c(NA, 10))
  r <- tratar_nulos(df); expect_equal(r$n, 1); expect_equal(nrow(r$df), 1)
})

test_that("coherencia OHLC", {
  df <- data.frame(open = c(10, 10, 10, 10), high = c(11, 9, 11, 11), low = c(9, 10, 9, 9),
                   close = c(10, 10, 12, 10), adj_close = 10, volume = c(1, 1, 1, -5))
  r <- validar_coherencia_ohlc(df)
  expect_equal(r$n, 3); expect_equal(!is.na(r$df$close), c(TRUE, FALSE, FALSE, FALSE))
})

test_that("los parámetros se leen de la configuración", {
  p <- parametros_limpieza(cargar_config())
  expect_equal(p$ventana, 60); expect_equal(p$umbral_atipico, 8); expect_equal(p$salto_minimo_error, 0.405)
  expect_equal(p$tolerancia_ohlc, 1e-6)
})

# ------------------------------------------------------------------------------ macro
precios_prueba <- function(inicio = "2024-01-02", n = 120) data.frame(fecha = seq_dias_habiles(as.Date(inicio), n), adj_close = 1)
valor_en <- function(df, fecha, col) df[[col]][df$fecha == as.Date(fecha)]

test_that("macro: sin look-ahead y con rezago por serie", {
  macro <- data.frame(serie = "ipc", fecha = as.Date(c("2024-01-01", "2024-02-01")), valor = c(100, 200))
  out <- alinear_macro(precios_prueba(), macro, rezagos = list(ipc = 45))
  # el dato de enero (01-01) solo es utilizable desde el 15-feb; el de febrero desde el 17-mar
  expect_true(is.na(valor_en(out, "2024-02-14", "macro_ipc")))
  expect_equal(valor_en(out, "2024-02-15", "macro_ipc"), 100)
  expect_equal(valor_en(out, "2024-03-15", "macro_ipc"), 100)
  expect_equal(valor_en(out, "2024-03-18", "macro_ipc"), 200)
})

test_that("macro: fecha de publicación y primera versión (las revisiones no se usan)", {
  macro <- data.frame(serie = "pib", fecha = as.Date(c("2024-01-01", "2024-01-01")),
                      fecha_publicacion = as.Date(c("2024-02-20", "2024-03-25")), valor = c(1, 1.5))
  out <- alinear_macro(precios_prueba(), macro)
  expect_equal(valor_en(out, "2024-02-21", "macro_pib"), 1)
  expect_equal(valor_en(out, "2024-04-01", "macro_pib"), 1)
})

test_that("macro: formato ancho (compatibilidad con el código original)", {
  ancho <- data.frame(fecha = as.Date(c("2024-01-02", "2024-01-09")), vix = c(10, 20))
  out <- alinear_macro(precios_prueba(), ancho, rezago_dias = 1)
  expect_equal(valor_en(out, "2024-01-03", "macro_vix"), 10)
  expect_equal(valor_en(out, "2024-01-10", "macro_vix"), 20)
  expect_true(is.na(valor_en(out, "2024-01-02", "macro_vix")))
})

test_that("macro: la tolerancia limita la antigüedad del dato", {
  macro <- data.frame(serie = "x", fecha = as.Date("2024-01-02"), valor = 5)
  out <- alinear_macro(precios_prueba(), macro, rezago_dias = 0, tolerancia = 10)
  expect_equal(valor_en(out, "2024-01-05", "macro_x"), 5); expect_true(is.na(valor_en(out, "2024-02-01", "macro_x")))
})

test_that("limpiar_macro: los '.' de FRED se descartan y se conserva la primera publicación", {
  df <- data.frame(fecha = c("2024-01-01", "2024-01-02", "2024-01-03", "2024-01-03"), valor = c("1.5", ".", "2.0", "2.1"),
                   fecha_publicacion = c("2024-01-02", "2024-01-03", "2024-01-04", "2024-01-20"))
  r <- limpiar_macro(df, "vix")
  expect_equal(r$reporte$nulos_descartados, 1); expect_equal(r$reporte$duplicados, 1)
  expect_equal(r$df$valor[r$df$fecha == as.Date("2024-01-03")], 2.0)
})
