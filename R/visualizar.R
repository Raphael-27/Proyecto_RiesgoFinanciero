# Gráficas y exportación de los datos cargados en la base de datos (ggplot2 + patchwork).
#
# Cada gráfica tiene UN solo eje (sin doble eje), líneas finas, cuadrícula discreta y etiquetas
# directas; los colores siguen una paleta categórica de orden fijo validada contra daltonismo
# (el mismo activo conserva siempre su color). Escala logarítmica en precios de series largas.

SERIES <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300", "#6250d6", "#e34948")
SUPERFICIE <- "#fcfcfb"; TINTA <- "#0b0b0b"; TINTA_2 <- "#52514e"; TINTA_3 <- "#898880"
CUADRICULA <- "#e9e8e4"; EJE <- "#c9c8c3"; FRANJA <- "#efeeea"

# Episodios de crisis del documento (entrenamiento y backtesting). Fechas aproximadas.
CRISIS <- data.frame(nombre = c("Crisis 2008", "COVID-19"),
                     ini = as.Date(c("2008-09-01", "2020-02-19")),
                     fin = as.Date(c("2009-03-31", "2020-04-07")), stringsAsFactors = FALSE)

requerir_paquetes <- function() {
  faltan <- Filter(function(p) !requireNamespace(p, quietly = TRUE), c("ggplot2", "patchwork", "scales"))
  if (length(faltan)) {
    stop(sprintf("Faltan paquetes para las gráficas: %s. Instálalos con:  install.packages(c(%s))",
                 paste(faltan, collapse = ", "), paste0("\"", faltan, "\"", collapse = ", ")), call. = FALSE)
  }
}

# ----------------------------------------------------------------------------------- estilo
tema_etl <- function(ocultar_x = FALSE) {
  t <- ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = SUPERFICIE, color = NA),
      panel.background = ggplot2::element_rect(fill = SUPERFICIE, color = NA),
      panel.grid.major.x = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = CUADRICULA, linewidth = 0.35),
      axis.line = ggplot2::element_line(color = EJE, linewidth = 0.35),
      axis.ticks = ggplot2::element_line(color = EJE, linewidth = 0.35),
      axis.text = ggplot2::element_text(color = TINTA_2, size = 8.5), axis.title = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", size = 11.5, color = TINTA, hjust = 0),
      plot.subtitle = ggplot2::element_text(color = TINTA_2, size = 8.5, margin = ggplot2::margin(b = 6)),
      plot.title.position = "plot", plot.margin = ggplot2::margin(10, 40, 6, 8),
      legend.position = "top", legend.justification = "right", legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(color = TINTA_2, size = 8.5),
      legend.key.width = ggplot2::unit(1.4, "lines"), legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.box.margin = ggplot2::margin(0, 0, -6, 0), legend.box = "horizontal")
  if (ocultar_x) t <- t + ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank())
  t
}

# Años como marcas del eje X (sin marcas en el margen derecho reservado a las etiquetas)
marcas_anios <- function(inicio, fin) {
  anios <- as.integer(format(fin, "%Y")) - as.integer(format(inicio, "%Y"))
  paso <- max(1L, round(anios / 10))
  primero <- as.Date(sprintf("%d-01-01", as.integer(format(inicio, "%Y")) + ifelse(format(inicio, "%m-%d") == "01-01", 0L, 1L)))
  seq(primero, fin, by = sprintf("%d years", paso))
}

escala_x <- function(inicio, fin, extra = 0.13) {
  ggplot2::scale_x_date(limits = c(inicio, fin + as.numeric(fin - inicio) * extra), breaks = marcas_anios(inicio, fin),
                        labels = function(x) format(x, "%Y"), expand = ggplot2::expansion(0))
}

# Límites del eje logarítmico con holgura inferior (5 %) y superior (60 %, para las etiquetas finales)
limites_log <- function(valores) {
  r <- range(valores, na.rm = TRUE)
  c(r[1] * 0.9, r[2] * 1.6)
}

# Marcas del eje logarítmico: 1-2-5 por década; si el rango es angosto, se afinan.
marcas_log <- function(lim) {
  decadas <- 10^seq(floor(log10(lim[1])) - 1, ceiling(log10(lim[2])) + 1)
  for (m in list(c(1, 2, 5), c(1, 1.5, 2, 3, 5, 7), c(1, 1.2, 1.5, 2, 2.5, 3, 4, 5, 6, 7, 8, 9))) {
    b <- sort(as.vector(outer(m, decadas)))
    b <- b[b >= lim[1] & b <= lim[2]]
    if (length(b) >= 4) break
  }
  b
}

# ymin/ymax: en un eje logarítmico NO se pueden usar -Inf ni 0 (log10 da NaN o -Inf y ggplot2 avisa). En
# esos paneles se fijan los límites del eje (limites_log) y la franja se dibuja exactamente entre ellos.
franjas_crisis <- function(inicio, fin, rotular = FALSE, ymin = -Inf, ymax = Inf) {
  visibles <- CRISIS[CRISIS$fin >= inicio & CRISIS$ini <= fin, , drop = FALSE]
  if (nrow(visibles) == 0) return(list())
  visibles$y0 <- ymin
  visibles$y1 <- ymax
  capas <- list(ggplot2::geom_rect(data = visibles, ggplot2::aes(xmin = ini, xmax = fin, ymin = y0, ymax = y1),
                                   inherit.aes = FALSE, fill = FRANJA))
  if (rotular) {
    visibles$centro <- visibles$ini + as.numeric(visibles$fin - visibles$ini) / 2
    # sin borde: el argumento se llama `linewidth` desde ggplot2 3.5.0 y `label.size` antes
    sin_borde <- if (utils::packageVersion("ggplot2") >= "3.5.0") list(linewidth = 0) else list(label.size = 0)
    capas <- c(capas, list(do.call(ggplot2::geom_label, c(list(
      data = visibles, mapping = ggplot2::aes(x = centro, y = Inf, label = nombre), inherit.aes = FALSE, vjust = 1.2,
      size = 2.7, color = TINTA_2, fill = SUPERFICIE, alpha = 0.85, label.padding = ggplot2::unit(0.12, "lines")),
      sin_borde))))
  }
  capas
}

# Etiquetas directas al final de cada línea, en tinta de texto. Si varias líneas terminan casi en el
# mismo punto, se separan lo MÍNIMO necesario (en la escala del eje) y el grupo se centra sobre sus
# líneas, para que cada etiqueta quede junto a la suya. `lim_y` son los límites del eje (si se fijaron).
etiquetas_finales <- function(larga, fin, lim_y = NULL, log = FALSE, separacion = 0.085) {
  ultimos <- do.call(rbind, unname(lapply(split(larga, larga$serie), function(g) g[which.max(g$fecha), , drop = FALSE])))
  tr <- if (log) log10 else identity
  inv <- if (log) function(x) 10^x else identity
  y <- tr(ultimos$valor)
  rango <- if (!is.null(lim_y)) diff(tr(lim_y)) else diff(range(tr(larga$valor))) * 1.13
  sep <- separacion * rango

  o <- order(y); ya <- y[o]
  if (length(ya) > 1) for (i in 2:length(ya)) ya[i] <- max(ya[i], ya[i - 1] + sep)
  ya <- ya + (mean(y[o]) - mean(ya))                         # centrar sobre las líneas
  if (!is.null(lim_y)) {                                      # que no se salgan del panel
    li <- tr(lim_y)[1] + 0.03 * rango; ls <- tr(lim_y)[2] - 0.03 * rango
    if (max(ya) > ls) ya <- ya - (max(ya) - ls)
    if (min(ya) < li) ya <- ya + (li - min(ya))
  }
  ultimos$y_etiqueta <- NA_real_
  ultimos$y_etiqueta[o] <- inv(ya)
  ultimos$x_etiqueta <- fin + as.numeric(fin - min(larga$fecha)) * 0.012
  ggplot2::geom_text(data = ultimos, ggplot2::aes(x = x_etiqueta, y = y_etiqueta, label = etiqueta),
                     inherit.aes = FALSE, hjust = 0, size = 3, color = TINTA_2, show.legend = FALSE)
}

# `claves` (sin acentos) identifican la serie dentro de ggplot; `etiquetas` es el texto que se muestra.
# Así la gráfica no depende de la codificación del equipo para hacer coincidir escalas.
linea_larga <- function(df, columnas, claves, etiquetas) {
  partes <- Map(function(col, clave, etiqueta) {
    if (!col %in% names(df)) return(NULL)
    d <- data.frame(fecha = df$fecha, valor = df[[col]], serie = clave, etiqueta = etiqueta, stringsAsFactors = FALSE)
    d[!is.na(d$valor), , drop = FALSE]
  }, columnas, claves, etiquetas)
  out <- do.call(rbind, unname(Filter(Negate(is.null), partes)))
  out$serie <- factor(out$serie, levels = claves)
  out
}

# ------------------------------------------------------------------------- panel de riesgo
panel_precio <- function(df, inicio, fin) {
  claves <- c("precio", "ma50", "ma200")
  etiquetas <- c("Precio", "Media 50 días", "Media 200 días")
  larga <- linea_larga(df, c("adj_close", "ma_50", "ma_200"), claves, etiquetas)
  colores <- stats::setNames(SERIES[1:3], claves)
  lim_y <- limites_log(larga$valor)
  g <- ggplot2::ggplot(larga, ggplot2::aes(fecha, valor, color = serie)) +
    franjas_crisis(inicio, fin, rotular = TRUE, ymin = lim_y[1], ymax = lim_y[2]) +
    ggplot2::geom_line(linewidth = 0.55, lineend = "round") +
    ggplot2::scale_color_manual(values = colores, labels = stats::setNames(etiquetas, claves)) +
    ggplot2::scale_y_log10(limits = lim_y, breaks = marcas_log, labels = scales::label_comma(), expand = ggplot2::expansion(0)) +
    escala_x(inicio, fin) + etiquetas_finales(larga, fin, lim_y, log = TRUE) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(title = "Precio ajustado y medias móviles",
                  subtitle = "Cierre ajustado por splits y dividendos · escala logarítmica") +
    tema_etl(ocultar_x = TRUE)

  marcas <- NULL
  if ("es_atipico" %in% names(df) && any(df$es_atipico, na.rm = TRUE)) {
    marcas <- rbind(marcas, data.frame(fecha = df$fecha[df$es_atipico], valor = df$adj_close[df$es_atipico], marca = "atipico"))
  }
  if ("error_dato" %in% names(df) && any(df$error_dato, na.rm = TRUE)) {
    marcas <- rbind(marcas, data.frame(fecha = df$fecha[df$error_dato], valor = df$adj_close[df$error_dato], marca = "error"))
  }
  if (!is.null(marcas)) {
    g <- g + ggplot2::geom_point(data = marcas, ggplot2::aes(fecha, valor, shape = marca, fill = marca), inherit.aes = FALSE,
                                 color = TINTA, size = 1.9, stroke = 0.6) +
      ggplot2::scale_shape_manual(values = c(atipico = 21, error = 23),
                                  labels = c(atipico = "Retorno atípico", error = "Error de dato (corregido)")) +
      ggplot2::scale_fill_manual(values = c(atipico = TINTA, error = SUPERFICIE),
                                 labels = c(atipico = "Retorno atípico", error = "Error de dato (corregido)"))
  }
  g
}

panel_var <- function(df, inicio, fin) {
  claves <- c("var95", "es95", "var99")
  etiquetas <- c("VaR 95%", "ES 95%", "VaR 99%")
  larga <- linea_larga(df, c("var_hist_95", "es_hist_95", "var_hist_99"), claves, etiquetas)
  ggplot2::ggplot(larga, ggplot2::aes(fecha, valor, color = serie)) +
    franjas_crisis(inicio, fin) +
    ggplot2::geom_line(linewidth = 0.55, lineend = "round") +
    ggplot2::scale_color_manual(values = stats::setNames(SERIES[1:3], claves), labels = stats::setNames(etiquetas, claves)) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 0.1), expand = ggplot2::expansion(mult = c(0.05, 0.08))) +
    escala_x(inicio, fin) + etiquetas_finales(larga, fin) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(title = "Valor en Riesgo (VaR) y Déficit Esperado (ES)",
                  subtitle = "Pérdida diaria en % del valor; ES = pérdida media cuando se supera el VaR") +
    tema_etl(ocultar_x = TRUE)
}

panel_simple <- function(df, columna, titulo, subtitulo, formato, inicio, fin, ocultar_x = TRUE) {
  d <- data.frame(fecha = df$fecha, valor = df[[columna]])
  d <- d[!is.na(d$valor), , drop = FALSE]
  ggplot2::ggplot(d, ggplot2::aes(fecha, valor)) +
    franjas_crisis(inicio, fin) +
    ggplot2::geom_line(color = SERIES[1], linewidth = 0.55, lineend = "round") +
    ggplot2::scale_y_continuous(labels = formato, expand = ggplot2::expansion(mult = c(0.05, 0.08))) +
    escala_x(inicio, fin) + ggplot2::labs(title = titulo, subtitle = subtitulo) + tema_etl(ocultar_x)
}

#' Panel de riesgo de un activo: precio, VaR/ES, volatilidad (y VIX si hay datos macro).
grafico_riesgo <- function(df, ticker, ruta) {
  requerir_paquetes()
  df <- df[order(df$fecha), , drop = FALSE]
  inicio <- min(df$fecha); fin <- max(df$fecha)
  vol <- grep("^vol_hist_", names(df), value = TRUE)[1]
  hay_vix <- "macro_vix" %in% names(df) && any(!is.na(df$macro_vix))

  paneles <- list(panel_precio(df, inicio, fin), panel_var(df, inicio, fin),
                  panel_simple(df, vol, "Volatilidad histórica anualizada",
                               "Desviación estándar de retornos diarios (21 días), anualizada",
                               scales::label_percent(accuracy = 1), inicio, fin, ocultar_x = hay_vix))
  if (hay_vix) {
    paneles[[4]] <- panel_simple(df, "macro_vix", "VIX: volatilidad implícita del S&P 500",
                                 "Puntos del índice; con rezago de 1 día para no mirar el futuro",
                                 scales::label_number(), inicio, fin, ocultar_x = FALSE)
  }
  g <- patchwork::wrap_plots(paneles, ncol = 1) +
    patchwork::plot_annotation(title = sprintf("%s: panel de riesgo", ticker), theme = ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14, color = TINTA),
      plot.background = ggplot2::element_rect(fill = SUPERFICIE, color = NA)))
  dir.create(dirname(ruta), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(ruta, g, width = 11, height = 2.9 * length(paneles) + 0.7, dpi = 140, bg = SUPERFICIE)
  invisible(ruta)
}

# ---------------------------------------------------------------------------- comparación
#' Última lectura de las métricas de riesgo de cada activo.
resumen_riesgo <- function(dfs) {
  filas <- lapply(names(dfs), function(t) {
    d <- dfs[[t]]
    validos <- d[!is.na(d$var_hist_95), , drop = FALSE]
    u <- if (nrow(validos)) validos[nrow(validos), ] else d[nrow(d), ]
    data.frame(ticker = t, fecha = format(u$fecha), `VaR 95%` = u$var_hist_95, `ES 95%` = u$es_hist_95,
               `VaR 99%` = u$var_hist_99, `Volatilidad anual` = u[[grep("^vol_hist_", names(u), value = TRUE)[1]]],
               check.names = FALSE, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, filas)
  rownames(out) <- out$ticker
  out$ticker <- NULL
  out
}

#' Evolución relativa (base 100) y VaR 95 % actual de todos los activos.
grafico_comparacion <- function(dfs, ruta) {
  requerir_paquetes()
  tickers <- names(dfs)
  if (length(tickers) > length(SERIES)) {
    log_warn("Se grafican solo los primeros %d activos (límite de colores distinguibles).", length(SERIES))
    tickers <- tickers[seq_along(SERIES)]
  }
  colores <- stats::setNames(SERIES[seq_along(tickers)], tickers)   # el color sigue al activo, no a su posición
  comun <- max(vapply(dfs[tickers], function(d) as.numeric(min(d$fecha)), numeric(1)))
  comun <- as.Date(comun, origin = "1970-01-01")
  fin <- max(do.call(c, lapply(dfs[tickers], function(d) max(d$fecha))))

  larga <- do.call(rbind, unname(lapply(tickers, function(t) {
    d <- dfs[[t]][dfs[[t]]$fecha >= comun & !is.na(dfs[[t]]$adj_close), , drop = FALSE]
    data.frame(fecha = d$fecha, valor = d$adj_close / d$adj_close[1] * 100, serie = t, etiqueta = t, stringsAsFactors = FALSE)
  })))
  larga$serie <- factor(larga$serie, levels = tickers)
  lim_y <- limites_log(larga$valor)

  p1 <- ggplot2::ggplot(larga, ggplot2::aes(fecha, valor, color = serie)) +
    franjas_crisis(comun, fin, rotular = TRUE, ymin = lim_y[1], ymax = lim_y[2]) +
    ggplot2::geom_line(linewidth = 0.55, lineend = "round") +
    ggplot2::scale_color_manual(values = colores) +
    ggplot2::scale_y_log10(limits = lim_y, breaks = marcas_log, labels = scales::label_comma(), expand = ggplot2::expansion(0)) +
    escala_x(comun, fin, 0.08) + etiquetas_finales(larga, fin, lim_y, log = TRUE) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(title = "Evolución relativa (base 100)",
                  subtitle = sprintf("Precio ajustado desde %s · escala logarítmica", format(comun))) +
    tema_etl()

  r <- resumen_riesgo(dfs[tickers])
  barras <- data.frame(ticker = rownames(r), valor = r[["VaR 95%"]], stringsAsFactors = FALSE)
  barras <- barras[!is.na(barras$valor), , drop = FALSE]
  barras$ticker <- factor(barras$ticker, levels = barras$ticker[order(barras$valor)])   # la mayor queda arriba
  p2 <- ggplot2::ggplot(barras, ggplot2::aes(ticker, valor)) +
    ggplot2::geom_col(width = 0.55, fill = SERIES[1]) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f%%", valor * 100)), hjust = -0.15, size = 3, color = TINTA) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 0.1), expand = ggplot2::expansion(mult = c(0, 0.25))) +
    ggplot2::labs(title = "VaR 95% actual", subtitle = "Pérdida diaria no superada el 95% de los días") +
    tema_etl() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_line(color = CUADRICULA, linewidth = 0.35),
                   axis.text.y = ggplot2::element_text(color = TINTA, size = 9.5), axis.line.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank())

  g <- p1 + p2 + patchwork::plot_layout(widths = c(2.3, 1)) +
    patchwork::plot_annotation(title = "Comparación de activos", theme = ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14, color = TINTA),
      plot.background = ggplot2::element_rect(fill = SUPERFICIE, color = NA)))
  dir.create(dirname(ruta), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(ruta, g, width = 12, height = 5, dpi = 140, bg = SUPERFICIE)
  invisible(ruta)
}

# ---------------------------------------------------------------------------------- CLI
exportar_csv <- function(df, ticker, carpeta) {
  dir.create(carpeta, recursive = TRUE, showWarnings = FALSE)
  ruta <- file.path(carpeta, paste0(ticker, "_caracteristicas.csv"))
  d <- df
  d$fecha <- format(d$fecha, "%Y-%m-%d")
  utils::write.csv(d, ruta, row.names = FALSE, fileEncoding = "UTF-8")
  invisible(ruta)
}

main_visualizar <- function(args = commandArgs(trailingOnly = TRUE)) {
  ticker <- NULL; demo <- FALSE; exportar <- FALSE; abrir <- FALSE; config <- NULL
  i <- 1L
  while (i <= length(args)) {
    a <- args[i]
    if (a == "--ticker") { ticker <- args[i + 1L]; i <- i + 1L }
    else if (a == "--config") { config <- args[i + 1L]; i <- i + 1L }
    else if (a == "--demo") demo <- TRUE
    else if (a == "--exportar") exportar <- TRUE
    else if (a == "--abrir") abrir <- TRUE
    else stop(sprintf("Argumento desconocido: %s (usa --ticker, --demo, --exportar, --abrir, --config)", a), call. = FALSE)
    i <- i + 1L
  }
  cfg <- cargar_config(config)
  if (demo) {
    cfg$proyecto$directorio_datos <- file.path(directorio_datos(cfg), "demo")
    cfg$carga$base_datos_url <- paste0("sqlite:///", file.path(cfg$proyecto$directorio_datos, "riesgo.db"))
  }
  base <- directorio_datos(cfg)

  tickers <- if (!is.null(ticker)) ticker else vapply(activos(cfg), function(a) a$ticker, character(1))
  dfs <- stats::setNames(lapply(tickers, function(t) leer_caracteristicas(cfg, t)), tickers)
  generados <- character()
  for (t in tickers) {
    generados <- c(generados, grafico_riesgo(dfs[[t]], t, file.path(base, "graficas", paste0("riesgo_", t, ".png"))))
    if (exportar) generados <- c(generados, exportar_csv(dfs[[t]], t, file.path(base, "exportes")))
  }
  if (length(dfs) > 1) generados <- c(generados, grafico_comparacion(dfs, file.path(base, "graficas", "comparacion.png")))

  cat("\nÚltima lectura de riesgo (pérdidas diarias en fracción; 0.02 = 2 %):\n")
  print(round(resumen_riesgo(dfs)[, -1], 6))
  cat("\nArchivos generados:\n"); cat(paste0("   ", generados), sep = "\n")
  if (abrir && .Platform$OS.type == "windows") for (g in generados[grepl("\\.png$", generados)]) shell.exec(normalizePath(g))
  invisible(0L)
}
