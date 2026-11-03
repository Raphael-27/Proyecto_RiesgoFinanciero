# Los clientes se prueban con respuestas simuladas (no hay acceso a Internet en las pruebas).
# Los formatos simulados siguen la documentación pública de cada API.

AV_OK <- list(`Meta Data` = list(`2. Symbol` = "IBM"), `Time Series (Daily)` = list(
  `2024-01-03` = list(`1. open` = "10", `2. high` = "12", `3. low` = "9", `4. close` = "11", `5. volume` = "1000"),
  `2024-01-02` = list(`1. open` = "9", `2. high` = "11", `3. low` = "8", `4. close` = "10", `5. volume` = "900")))
FRED_OBS <- list(
  list(realtime_start = "2024-02-01", realtime_end = "9999-12-31", date = "2024-01-01", value = "3.1"),
  list(realtime_start = "2024-02-02", realtime_end = "9999-12-31", date = "2024-01-02", value = "."),
  list(realtime_start = "2024-02-03", realtime_end = "9999-12-31", date = "2024-01-03", value = "3.3"))
FRED_OK <- list(count = 3, observations = FRED_OBS)

con_clave <- function(var, valor = "K") withr::local_envvar(stats::setNames(valor, var), .local_envir = parent.frame())
silencio <- function(expr) suppressMessages(expr)

# ------------------------------------------------------------------------ Alpha Vantage
test_that("Alpha Vantage: parsea, ordena y filtra por fecha de inicio", {
  con_clave("ALPHAVANTAGE_API_KEY", "SECRETO")
  http <- http_falso(resp_http(AV_OK))
  df <- cliente_alphavantage(cfg_temporal(), http$fn)$descargar("IBM", "2024-01-03")
  expect_equal(names(df), c("fecha", "open", "high", "low", "close", "volume"))
  expect_equal(df$fecha, as.Date("2024-01-03"))
  expect_equal(http$llamadas()[[1]]$params[["function"]], "TIME_SERIES_DAILY")
})

test_that("Alpha Vantage premium usa el endpoint ajustado", {
  con_clave("ALPHAVANTAGE_API_KEY"); cfg <- cfg_temporal(); cfg$fuentes$alphavantage$premium <- TRUE
  adj <- list(`Time Series (Daily)` = list(`2024-01-02` = list(`1. open` = "9", `2. high` = "11", `3. low` = "8",
    `4. close` = "10", `5. adjusted close` = "9.5", `6. volume` = "9", `7. dividend amount` = "0", `8. split coefficient` = "1")))
  http <- http_falso(resp_http(adj))
  df <- cliente_alphavantage(cfg, http$fn)$descargar("IBM")
  expect_equal(http$llamadas()[[1]]$params[["function"]], "TIME_SERIES_DAILY_ADJUSTED"); expect_equal(df$adj_close, 9.5)
})

test_that("Alpha Vantage: los errores permanentes no se reintentan", {
  con_clave("ALPHAVANTAGE_API_KEY")
  for (r in list(list(`Error Message` = "Invalid API call"), list(Information = "This is a premium endpoint"))) {
    http <- http_falso(resp_http(r))
    e <- tryCatch(silencio(cliente_alphavantage(cfg_temporal(), http$fn)$descargar("XXX")), error = function(e) e)
    expect_s3_class(e, "error_api"); expect_false(inherits(e, "error_api_temporal")); expect_length(http$llamadas(), 1)
  }
})

test_that("Alpha Vantage: el límite de uso se reintenta y se recupera", {
  con_clave("ALPHAVANTAGE_API_KEY")
  limite <- resp_http(list(Note = "Thank you ... our standard API rate limit is 25 requests per day."))
  http <- http_falso(limite, limite, resp_http(AV_OK))
  df <- silencio(cliente_alphavantage(cfg_temporal(), http$fn)$descargar("IBM"))
  expect_equal(nrow(df), 2); expect_length(http$llamadas(), 3)
})

test_that("los reintentos se agotan (reintentos + 1 intentos en total)", {
  con_clave("ALPHAVANTAGE_API_KEY"); cfg <- cfg_temporal()
  http <- http_falso(resp_http(status = 503, texto = ""))
  expect_error(silencio(cliente_alphavantage(cfg, http$fn)$descargar("IBM")), class = "error_api_temporal")
  expect_length(http$llamadas(), cfg$extraccion$reintentos + 1)
})

test_that("la clave nunca aparece en el error", {
  con_clave("ALPHAVANTAGE_API_KEY", "CLAVE-ULTRA-SECRETA"); cfg <- cfg_temporal(); cfg$extraccion$reintentos <- 0
  http <- http_falso(simpleError("fallo en https://x/query?apikey=CLAVE-ULTRA-SECRETA&symbol=IBM"))
  e <- tryCatch(cliente_alphavantage(cfg, http$fn)$descargar("IBM"), error = function(e) e)
  expect_s3_class(e, "error_api_temporal"); expect_false(grepl("CLAVE-ULTRA-SECRETA", conditionMessage(e), fixed = TRUE))
})

test_that("sin clave falla con un mensaje claro", {
  withr::local_envvar(c(ALPHAVANTAGE_API_KEY = ""))
  expect_error(cliente_alphavantage(cfg_temporal(), http_falso(resp_http(AV_OK))$fn)$descargar("IBM"), "ALPHAVANTAGE_API_KEY")
})

# ----------------------------------------------------------------------------------- FRED
test_that("FRED: parsea valores y faltantes", {
  con_clave("FRED_API_KEY"); http <- http_falso(resp_http(FRED_OK))
  df <- cliente_fred(cfg_temporal(), http$fn)$descargar("VIXCLS", "2024-01-01")
  expect_equal(names(df), c("fecha", "valor")); expect_equal(df$valor, c("3.1", ".", "3.3"))
  p <- http$llamadas()[[1]]$params
  expect_equal(c(p$series_id, p$file_type, p$observation_start), c("VIXCLS", "json", "2024-01-01")); expect_null(p$realtime_start)
})

test_that("FRED vintage trae la fecha de publicación", {
  con_clave("FRED_API_KEY"); http <- http_falso(resp_http(FRED_OK))
  df <- cliente_fred(cfg_temporal(), http$fn)$descargar("CPIAUCSL", vintage = TRUE)
  expect_equal(df$fecha_publicacion[1], "2024-02-01"); expect_equal(http$llamadas()[[1]]$params$realtime_end, "9999-12-31")
})

test_that("FRED pagina hasta completar", {
  con_clave("FRED_API_KEY")
  http <- http_falso(resp_http(list(count = 3, observations = FRED_OBS[1:2])), resp_http(list(count = 3, observations = FRED_OBS[3])))
  df <- cliente_fred(cfg_temporal(), http$fn)$descargar("X")
  expect_equal(nrow(df), 3); expect_equal(vapply(http$llamadas(), function(l) l$params$offset, numeric(1)), c(0, 2))
})

test_that("FRED: una serie inexistente es un error permanente", {
  con_clave("FRED_API_KEY")
  http <- http_falso(resp_http(list(error_code = 400, error_message = "Bad Request. The series does not exist.")))
  e <- tryCatch(cliente_fred(cfg_temporal(), http$fn)$descargar("NOEXISTE"), error = function(e) e)
  expect_s3_class(e, "error_api"); expect_false(inherits(e, "error_api_temporal")); expect_length(http$llamadas(), 1)
})

# ---------------------------------------------------------------------------------- Yahoo
CHART_OK <- list(chart = list(error = NULL, result = list(list(
  meta = list(exchangeTimezoneName = "America/New_York"),
  timestamp = list(1704205800, 1704292200, 1704378600),   # 2024-01-02 14:30 UTC = 09:30 en Nueva York
  indicators = list(quote = list(list(open = list(1, 2, NULL), high = list(2, 3, 4), low = list(0.5, 1, 2),
                                      close = list(1.5, 2.5, 3.5), volume = list(10, 20, 30))),
                    adjclose = list(list(adjclose = list(1.4, 2.4, 3.4))))))))

test_that("Yahoo (motor chart): fechas en la zona del mercado, nulos como NA y adj_close", {
  cfg <- cfg_temporal(); cfg$fuentes$yahoo$motor <- "chart"
  df <- cliente_yahoo(cfg, http_falso(resp_http(CHART_OK))$fn)$descargar("AAPL", "2024-01-01")
  expect_equal(df$fecha, as.Date(c("2024-01-02", "2024-01-03", "2024-01-04")))
  expect_true(is.na(df$open[3])); expect_equal(df$adj_close, c(1.4, 2.4, 3.4))
  expect_equal(names(df), c("fecha", "open", "high", "low", "close", "volume", "adj_close"))
})

test_that("Yahoo (motor chart): error de símbolo = permanente; serie vacía = temporal", {
  cfg <- cfg_temporal(); cfg$fuentes$yahoo$motor <- "chart"
  malo <- resp_http(list(chart = list(result = NULL, error = list(code = "Not Found", description = "No data found"))))
  e <- tryCatch(cliente_yahoo(cfg, http_falso(malo)$fn)$descargar("ZZZZ", "2024-01-01"), error = function(e) e)
  expect_s3_class(e, "error_api"); expect_false(inherits(e, "error_api_temporal"))
  vacio <- resp_http(list(chart = list(error = NULL, result = list(list(meta = list(), timestamp = list())))))
  http <- http_falso(vacio)
  expect_error(silencio(cliente_yahoo(cfg, http$fn)$descargar("AAPL", "2024-01-01")), class = "error_api_temporal")
  expect_length(http$llamadas(), cfg$extraccion$reintentos + 1)
})

test_that("Yahoo (motor quantmod): reintenta ante límite de uso y no ante símbolo inexistente", {
  cfg <- cfg_temporal(); n <- 0
  flojo <- function(ticker, inicio, fin = NULL) { n <<- n + 1; if (n < 3) stop(error_api("Yahoo limitó las solicitudes", TRUE)); data.frame(fecha = as.Date("2024-01-02"), close = 1) }
  expect_equal(nrow(silencio(cliente_yahoo(cfg, descargar_quantmod = flojo)$descargar("AAPL", "2024-01-01"))), 1); expect_equal(n, 3)
  m <- 0
  inexistente <- function(ticker, inicio, fin = NULL) { m <<- m + 1; stop(error_api("Símbolo inexistente")) }
  expect_error(cliente_yahoo(cfg, descargar_quantmod = inexistente)$descargar("ZZZZ", "2024-01-01"), class = "error_api"); expect_equal(m, 1)
})

test_that("fuente desconocida", {
  expect_error(extraer_activo(cfg_temporal(), list(ticker = "X", fuente = "bloomberg")), class = "error_api")
})

test_that("con_reintentos no oculta errores que no son de la API", {
  f <- con_reintentos(function() stop("bug del programa"), cfg_temporal())
  expect_error(f(), "bug del programa")
})
