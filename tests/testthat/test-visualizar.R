skip_if_not_installed("ggplot2"); skip_if_not_installed("patchwork")

datos_demo <- function() {
  demo <- suppressMessages(preparar_demo(cfg_temporal(), file.path(tempfile("demo_"))))
  suppressMessages(for (f in list(limpiar_datos, validar_calidad, transformar_datos, cargar_datos)) f(fecha = "2026-11-02", config = demo))
  demo
}

test_that("leer, graficar y exportar", {
  demo <- datos_demo(); dfs <- list(SPY = leer_caracteristicas(demo, "SPY"), AAPL = leer_caracteristicas(demo, "AAPL"))
  expect_equal(nrow(dfs$SPY), 1000); expect_type(dfs$SPY$es_atipico, "logical")

  carpeta <- tempfile("g_")
  p1 <- grafico_riesgo(dfs$SPY, "SPY", file.path(carpeta, "riesgo.png"))          # incluye panel VIX (hay macro)
  p2 <- grafico_comparacion(dfs, file.path(carpeta, "comparacion.png"))
  expect_gt(file.size(p1), 20000); expect_gt(file.size(p2), 20000)

  r <- resumen_riesgo(dfs); expect_equal(rownames(r), c("SPY", "AAPL")); expect_true(all(r[["VaR 95%"]] > 0))
  csv <- exportar_csv(dfs$SPY, "SPY", file.path(carpeta, "x")); expect_equal(nrow(utils::read.csv(csv)), 1000)
})

test_that("las gráficas no emiten avisos (las etiquetas con acento no dependen de la codificación)", {
  demo <- datos_demo(); df <- leer_caracteristicas(demo, "SPY")
  expect_no_warning(grafico_riesgo(df, "SPY", tempfile(fileext = ".png")))
})

test_that("sin datos, el mensaje es claro", {
  expect_error(leer_caracteristicas(cfg_temporal(), "NOEXISTE"), "no tiene datos. Ejecuta primero el pipeline")
})

test_that("las marcas del eje logarítmico se adaptan al rango", {
  expect_gte(length(marcas_log(c(1, 600))), 4); expect_gte(length(marcas_log(c(52, 135))), 4)
})

test_that("las franjas en eje logarítmico usan límites finitos: sin NaN ni infinitos (ggplot2 >= 3.5)", {
  lim <- limites_log(c(1, 600))
  f <- franjas_crisis(as.Date("2005-01-01"), as.Date("2026-01-01"), TRUE, ymin = lim[1], ymax = lim[2])
  expect_true(all(is.finite(c(f[[1]]$data$y0, f[[1]]$data$y1)))); expect_true(lim[1] > 0 && lim[2] > lim[1])
})

test_that("los gráficos con eje logarítmico no emiten ningún aviso", {
  set.seed(2); dias <- seq_dias_habiles(as.Date("2005-01-03"), 5500)
  l <- limpiar_serie(serie_ohlc(dias, 20, 80), "SPY")$df; df <- calcular_caracteristicas(l)
  expect_no_warning(grafico_riesgo(df, "SPY", tempfile(fileext = ".png")))
  expect_no_warning(grafico_comparacion(list(SPY = df, AAPL = df), tempfile(fileext = ".png")))
})

test_that("las etiquetas finales se separan lo mínimo y quedan junto a sus líneas", {
  d <- as.Date("2024-01-02") + 0:9
  larga <- rbind(data.frame(fecha = d, valor = 100, serie = "a", etiqueta = "A"),
                 data.frame(fecha = d, valor = 101, serie = "b", etiqueta = "B"),
                 data.frame(fecha = d, valor = 102, serie = "c", etiqueta = "C"))
  larga$serie <- factor(larga$serie)
  capa <- etiquetas_finales(larga, max(d), lim_y = c(50, 400), log = TRUE)
  y <- capa$data$y_etiqueta[order(capa$data$etiqueta)]
  expect_true(all(diff(y) > 0))                                    # A < B < C, sin encimarse
  expect_lt(max(abs(log10(y) - log10(101))), 0.2)                  # cerca de sus líneas (no a una década)
})
