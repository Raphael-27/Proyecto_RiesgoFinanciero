# Extracción desde APIs de mercado y macroeconómicas.
#
# Clientes: Yahoo Finance (paquete quantmod, o la API pública `chart` de Yahoo), Alpha Vantage y
# FRED. Todos
#   * reintentan con espera creciente ante fallos temporales (red, HTTP 429/5xx, límites de uso),
#   * fallan de inmediato ante errores permanentes (clave inválida, símbolo inexistente),
#   * leen sus claves de variables de entorno y NUNCA las incluyen en logs ni en errores,
#   * devuelven los datos tal como llegan (la limpieza ocurre después, sobre copias).
#
# Bloomberg no se incluye (requiere licencia y SDK propietario). El Banco de la República tampoco:
# no hay un endpoint público verificado (FRED publica series colombianas como DEXCOUS).
# IMPORTANTE: las APIs reales no se pudieron probar en vivo durante el desarrollo (el entorno
# bloqueaba esos sitios); los clientes se prueban con respuestas simuladas.

error_api <- function(mensaje, temporal = FALSE) {
  structure(class = c(if (temporal) "error_api_temporal", "error_api", "error", "condition"),
            list(message = mensaje, call = NULL))
}

redactar <- function(texto, secretos) {
  for (s in secretos) if (!is.null(s) && nzchar(s)) texto <- gsub(s, "***", texto, fixed = TRUE)
  texto
}

#' Envuelve `fn` con reintentos de espera creciente (5, 10, 20... s), configurable en `extraccion`.
con_reintentos <- function(fn, cfg) {
  e <- cfg$extraccion %||% list()
  n <- as.integer(e$reintentos %||% 3)
  base <- as.numeric(e$espera_base_segundos %||% 5)
  maxima <- as.numeric(e$espera_maxima_segundos %||% 120)
  function(...) {
    for (intento in seq_len(n + 1L)) {
      r <- tryCatch(fn(...), error_api_temporal = function(err) err)
      if (!inherits(r, "error_api_temporal")) return(r)
      if (intento > n) stop(r)
      espera <- min(base * 2^(intento - 1L), maxima)
      message(sprintf("Reintento %d/%d en %g s: %s", intento, n, espera, conditionMessage(r)))
      if (espera > 0) Sys.sleep(espera)
    }
  }
}

# ------------------------------------------------------------------------------------ HTTP
# `http_get(url, params, timeout)` devuelve list(status, texto). Es inyectable para las pruebas.
http_get_defecto <- function(url, params, timeout) {
  req <- httr2::request(url)
  req <- do.call(httr2::req_url_query, c(list(req), params))
  req <- httr2::req_timeout(req, timeout)
  # Yahoo rechaza clientes sin User-Agent de navegador. No se envía ningún dato personal.
  req <- httr2::req_user_agent(req, "Mozilla/5.0 (compatible; riesgo-financiero-etl)")
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  resp <- httr2::req_perform(req)
  list(status = httr2::resp_status(resp), texto = httr2::resp_body_string(resp))
}

# GET que clasifica los fallos y oculta las claves (las librerías HTTP incluyen la URL en sus errores).
get_json <- function(url, params, timeout, secretos = character(), http_get = http_get_defecto) {
  resp <- tryCatch(http_get(url, params, timeout), error = function(e) {
    stop(error_api(redactar(paste0(class(e)[1], ": ", conditionMessage(e)), secretos), temporal = TRUE))
  })
  if (resp$status == 429 || resp$status >= 500) stop(error_api(sprintf("HTTP %d en %s", resp$status, url), TRUE))
  if (resp$status >= 400) {
    stop(error_api(sprintf("HTTP %d en %s: %s", resp$status, url, redactar(substr(resp$texto, 1, 200), secretos))))
  }
  tryCatch(jsonlite::fromJSON(resp$texto, simplifyVector = FALSE),
           error = function(e) stop(error_api(sprintf("Respuesta no JSON desde %s", url), TRUE)))
}

timeout_cfg <- function(cfg) as.numeric(cfg$extraccion$timeout_segundos %||% 30)

num_o_na <- function(lista) vapply(lista, function(v) if (is.null(v)) NA_real_ else as.numeric(v), numeric(1))

# ------------------------------------------------------------------------------- Yahoo
#' Motor quantmod: precios diarios con Open/High/Low/Close/Volume/Adjusted. El Close de Yahoo está
#' ajustado por splits pero no por dividendos; `Adjusted` sí incluye ambos.
descargar_yahoo_quantmod <- function(ticker, inicio, fin = NULL) {
  if (!requireNamespace("quantmod", quietly = TRUE)) {
    stop(error_api("Falta el paquete quantmod: install.packages('quantmod') (o usa fuentes.yahoo.motor: chart en la configuración)."))
  }
  x <- tryCatch(
    suppressWarnings(quantmod::getSymbols(ticker, src = "yahoo", from = inicio, to = fin %||% (Sys.Date() + 1),
                                          auto.assign = FALSE, warnings = FALSE, verbose = FALSE)),
    error = function(e) {
      msg <- conditionMessage(e)
      temporal <- grepl("429|Too Many|timed out|timeout|Could not resolve|cannot open|connection|HTTP error 5|Couldn't connect",
                        msg, ignore.case = TRUE)
      stop(error_api(sprintf("Yahoo/quantmod falló para %s: %s", ticker, substr(msg, 1, 300)), temporal))
    })
  if (is.null(x) || NROW(x) == 0) stop(error_api(sprintf("Yahoo devolvió una serie vacía para %s", ticker), TRUE))
  df <- data.frame(fecha = as.Date(zoo::index(x)), zoo::coredata(x), check.names = FALSE)
  nombres <- tolower(sub("^.*\\.", "", names(df)))      # 'SPY.Adjusted' -> 'adjusted'
  nombres[nombres == "adjusted"] <- "adj_close"
  names(df) <- c("fecha", nombres[-1])
  df
}

#' Motor chart: API pública `v8/finance/chart` de Yahoo (alternativa si quantmod deja de funcionar).
descargar_yahoo_chart <- function(ticker, inicio, fin = NULL, timeout = 30, http_get = http_get_defecto) {
  url <- paste0("https://query1.finance.yahoo.com/v8/finance/chart/", utils::URLencode(ticker, reserved = TRUE))
  fin_fecha <- if (is.null(fin)) Sys.Date() + 1 else as.Date(fin)
  epoca <- function(d) as.integer(as.numeric(as.POSIXct(as.Date(d), tz = "UTC")))
  params <- list(period1 = epoca(inicio), period2 = epoca(fin_fecha),
                 interval = "1d", events = "div,splits", includeAdjustedClose = "true")
  datos <- get_json(url, params, timeout, http_get = http_get)

  err <- datos$chart$error
  if (!is.null(err)) {
    msg <- sprintf("Yahoo: %s (%s)", err$description %||% "error", err$code %||% "?")
    stop(error_api(msg, grepl("Too Many|rate", paste(err$code, err$description), ignore.case = TRUE)))
  }
  res <- datos$chart$result[[1]]
  if (is.null(res) || length(res$timestamp) == 0) stop(error_api(sprintf("Yahoo devolvió una serie vacía para %s", ticker), TRUE))
  ts <- as.numeric(unlist(res$timestamp))
  zona <- res$meta$exchangeTimezoneName %||% "UTC"
  q <- res$indicators$quote[[1]]
  df <- data.frame(
    fecha = as.Date(format(as.POSIXct(ts, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d", tz = zona)),
    open = num_o_na(q$open), high = num_o_na(q$high), low = num_o_na(q$low), close = num_o_na(q$close),
    volume = num_o_na(q$volume))
  adj <- res$indicators$adjclose[[1]]$adjclose
  df$adj_close <- if (!is.null(adj)) num_o_na(adj) else df$close
  df
}

cliente_yahoo <- function(cfg, http_get = http_get_defecto, descargar_quantmod = descargar_yahoo_quantmod) {
  motor <- cfg$fuentes$yahoo$motor %||% "quantmod"
  fn <- if (motor == "chart") {
    function(ticker, inicio, fin = NULL) descargar_yahoo_chart(ticker, inicio, fin, timeout_cfg(cfg), http_get)
  } else {
    function(ticker, inicio, fin = NULL) descargar_quantmod(ticker, inicio, fin)
  }
  list(nombre = "yahoo", descargar = con_reintentos(fn, cfg))
}

# ------------------------------------------------------------------------ Alpha Vantage
#' Interpreta la respuesta de Alpha Vantage. Los errores NO llegan como códigos HTTP: la API
#' responde 200 con las claves "Error Message", "Note" o "Information" (límite de uso).
parsear_alphavantage <- function(datos, ticker = "") {
  if (!is.null(datos[["Error Message"]])) {
    stop(error_api(sprintf("Alpha Vantage rechazó la consulta de %s: %s", ticker, datos[["Error Message"]])))
  }
  aviso <- datos[["Note"]] %||% datos[["Information"]]
  if (!is.null(aviso)) {
    if (grepl("premium", aviso, ignore.case = TRUE)) {
      stop(error_api(sprintf("Alpha Vantage: función no disponible en tu plan: %s", aviso)))
    }
    stop(error_api(sprintf("Alpha Vantage limitó las solicitudes: %s", aviso), TRUE))
  }
  clave <- grep("^Time Series", names(datos), value = TRUE)[1]
  if (is.na(clave) || length(datos[[clave]]) == 0) {
    stop(error_api(sprintf("Alpha Vantage no devolvió series para %s", ticker), TRUE))
  }
  serie <- datos[[clave]]
  campos <- names(serie[[1]])
  nombres <- tolower(gsub(" ", "_", sub("^\\d+\\.\\s*", "", campos)))   # "1. open" -> "open"
  nombres[nombres == "adjusted_close"] <- "adj_close"
  m <- do.call(rbind, lapply(serie, function(x) as.numeric(unlist(x[campos]))))
  df <- data.frame(fecha = as.Date(names(serie)), m)
  names(df) <- c("fecha", nombres)
  df[order(df$fecha), , drop = FALSE]
}

cliente_alphavantage <- function(cfg, http_get = http_get_defecto) {
  opciones <- cfg$fuentes$alphavantage %||% list()
  fn <- function(ticker, inicio = NULL, fin = NULL) {
    clave <- obtener_secreto("ALPHAVANTAGE_API_KEY")
    premium <- isTRUE(opciones$premium)
    params <- list(`function` = if (premium) "TIME_SERIES_DAILY_ADJUSTED" else "TIME_SERIES_DAILY",
                   symbol = ticker, outputsize = opciones$outputsize %||% "full", datatype = "json", apikey = clave)
    datos <- get_json("https://www.alphavantage.co/query", params, timeout_cfg(cfg), clave, http_get)
    df <- parsear_alphavantage(datos, ticker)
    if (!is.null(inicio)) df <- df[df$fecha >= as.Date(inicio), , drop = FALSE]
    if (!is.null(fin)) df <- df[df$fecha < as.Date(fin), , drop = FALSE]
    df
  }
  list(nombre = "alphavantage", descargar = con_reintentos(fn, cfg))
}

# ------------------------------------------------------------------------------- FRED
#' FRED `series/observations`. Los faltantes llegan como "." (se limpian después). Con
#' vintage = TRUE se piden todas las versiones para conocer la fecha real de primera publicación.
cliente_fred <- function(cfg, http_get = http_get_defecto) {
  url <- cfg$fuentes$fred$url %||% "https://api.stlouisfed.org/fred/series/observations"
  PAGINA <- 100000L
  fn <- function(serie, inicio = NULL, fin = NULL, vintage = FALSE) {
    clave <- obtener_secreto("FRED_API_KEY")
    base <- list(series_id = serie, api_key = clave, file_type = "json", limit = PAGINA)
    if (!is.null(inicio)) base$observation_start <- inicio
    if (!is.null(fin)) base$observation_end <- fin
    if (vintage) { base$realtime_start <- "1776-07-04"; base$realtime_end <- "9999-12-31" }

    filas <- list(); offset <- 0L
    repeat {
      datos <- get_json(url, c(base, list(offset = offset)), timeout_cfg(cfg), clave, http_get)
      if (!is.null(datos$error_code)) {
        codigo <- as.integer(datos$error_code)
        stop(error_api(sprintf("FRED error %d: %s", codigo, datos$error_message %||% ""), codigo %in% c(429L, 500L, 503L)))
      }
      obs <- datos$observations
      if (is.null(obs)) stop(error_api(sprintf("Respuesta inesperada de FRED para %s", serie), TRUE))
      filas <- c(filas, obs)
      offset <- offset + length(obs)
      if (length(obs) == 0 || offset >= as.integer(datos$count %||% offset)) break
    }
    if (length(filas) == 0) stop(error_api(sprintf("FRED devolvió cero observaciones para %s", serie), TRUE))
    campo <- function(n) vapply(filas, function(f) as.character(f[[n]] %||% NA_character_), character(1))
    out <- data.frame(fecha = campo("date"), valor = campo("value"), stringsAsFactors = FALSE)
    if (vintage) out$fecha_publicacion <- campo("realtime_start")   # primera fecha en que el dato fue conocido
    out
  }
  list(nombre = "fred", descargar = con_reintentos(fn, cfg))
}

# ---------------------------------------------------------------------- orquestación
#' Descarga un activo con el cliente de su fuente.
extraer_activo <- function(cfg, activo, clientes = list()) {
  fuente <- activo$fuente %||% "yahoo"
  inicio <- cfg$extraccion$fecha_inicio
  cliente <- switch(fuente,
    yahoo = clientes$yahoo %||% cliente_yahoo(cfg),
    alphavantage = clientes$alphavantage %||% cliente_alphavantage(cfg),
    stop(error_api(sprintf("Fuente desconocida para %s: %s", activo$ticker, fuente))))
  cliente$descargar(activo$ticker, inicio)
}

pausa_entre_llamadas <- function(cfg, fuente) {
  f <- cfg$fuentes[[fuente]] %||% list()
  segundos <- as.numeric(f$pausa_entre_llamadas_segundos %||% f$pausa_entre_activos_segundos %||% 0)
  if (segundos > 0) Sys.sleep(segundos)
}
