# Tareas del ETL (las que invoca `Rscript pipeline.R` o el programador de tareas).
#
# Cada tarea es independiente y se comunica con las demás a través de las capas en disco
# (raw -> clean -> curated -> base de datos): pueden reintentarse por separado y son idempotentes.
# Devuelven un resumen pequeño. Orden: extraer, limpiar, validar, transformar, cargar.

.cfg <- function(config) if (is.list(config)) config else cargar_config(config)
.fecha <- function(fecha) if (is.null(fecha)) format(Sys.Date()) else substr(as.character(fecha), 1, 10)
.macro_activo <- function(cfg) isTRUE(cfg$macro$habilitado %||% TRUE) && length(cfg$macro$series) > 0

# ---------------------------------------------------------------------------- 1. extraer
extraer_datos <- function(fecha = NULL, config = NULL, clientes = list(), ...) {
  cfg <- .cfg(config)
  fallidos <- list()
  extraidos <- character()

  for (activo in activos(cfg)) {
    t <- activo$ticker
    fuente <- activo$fuente %||% "yahoo"
    error <- tryCatch({
      df <- extraer_activo(cfg, activo, clientes)
      guardar_raw(df, cfg, fuente, t)
      log_info("Extraído %s: %d filas", t, nrow(df))
      NULL
    }, error_api = function(e) conditionMessage(e))
    if (is.null(error)) extraidos <- c(extraidos, t) else {
      log_warn("Falló la extracción de %s: %s", t, error)
      fallidos[[t]] <- error
    }
    pausa_entre_llamadas(cfg, fuente)
  }

  if (.macro_activo(cfg)) {
    obtener_secreto("FRED_API_KEY")          # falla pronto y con mensaje claro si falta
    fred <- clientes$fred %||% cliente_fred(cfg)
    vintage <- identical(cfg$macro$modo %||% "rezago", "vintage")
    for (serie in cfg$macro$series) {
      error <- tryCatch({
        df <- fred$descargar(serie$id, cfg$extraccion$fecha_inicio, vintage = vintage)
        guardar_raw(df, cfg, "fred", serie$id)
        NULL
      }, error_api = function(e) conditionMessage(e))
      if (is.null(error)) extraidos <- c(extraidos, serie$id) else {
        log_warn("Falló la extracción de %s: %s", serie$id, error)
        fallidos[[serie$id]] <- error
      }
    }
  }

  if (length(fallidos) && !isTRUE(cfg$extraccion$tolerar_fallos_parciales)) {
    detalle <- paste(sprintf("%s (%s)", names(fallidos), unlist(fallidos)), collapse = "; ")
    stop(error_api(paste("Extracción incompleta, fallaron:", detalle)))
  }
  list(fecha = .fecha(fecha), extraidos = as.list(extraidos), fallidos = fallidos)
}

# ----------------------------------------------------------------------------- 2. limpiar
limpiar_datos <- function(fecha = NULL, config = NULL, ...) {
  cfg <- .cfg(config)
  fecha <- .fecha(fecha)
  params <- parametros_limpieza(cfg)
  perfiles <- list(); reportes <- list()

  for (activo in activos(cfg)) {
    t <- activo$ticker
    fuente <- activo$fuente %||% "yahoo"
    crudo <- leer_raw_ultimo(cfg, fuente, t)
    perfiles[[t]] <- perfilar(crudo, t, activo$calendario, activo$zona_horaria)
    r <- limpiar_serie(crudo, t, params, zona_horaria = activo$zona_horaria,
                       calendario = activo$calendario, fuente = fuente)
    reportes[[t]] <- r$reporte
    log_info("Limpieza %s: entrada=%d salida=%d duplicados=%d incoherentes=%d atipicos=%d errores_dato=%d rellenadas=%d",
             t, r$reporte$filas_entrada, r$reporte$filas_salida, r$reporte$duplicados, r$reporte$incoherentes,
             r$reporte$atipicos, r$reporte$errores_dato, r$reporte$rellenadas)
    guardar_clean(r$df, cfg, t)
  }

  macro_reportes <- list()
  if (.macro_activo(cfg)) {
    partes <- list()
    for (serie in cfg$macro$series) {
      crudo <- leer_raw_ultimo(cfg, "fred", serie$id)
      r <- limpiar_macro(crudo, serie$nombre)
      macro_reportes[[serie$nombre]] <- r$reporte
      partes[[serie$nombre]] <- r$df
    }
    guardar_macro_clean(do.call(rbind, partes), cfg)
  }

  guardar_reporte(perfiles, cfg, paste0("perfil_", fecha))
  guardar_reporte(list(activos = reportes, macro = macro_reportes), cfg, paste0("limpieza_", fecha))
  list(fecha = fecha, limpiados = as.list(names(reportes)), macro = as.list(names(macro_reportes)))
}

# ----------------------------------------------------------------------------- 3. validar
#' Aplica la Tabla 3. Si una métrica bloqueante incumple, lanza un error de clase error_calidad
#' (el flujo se detiene antes de la carga). Las no bloqueantes solo generan alertas.
validar_calidad <- function(fecha = NULL, config = NULL, ahora = Sys.time(), ...) {
  cfg <- .cfg(config)
  fecha <- .fecha(fecha)
  limpieza <- leer_reporte(cfg, paste0("limpieza_", fecha))
  if (is.null(limpieza)) stop(sprintf("No existe el reporte de limpieza de %s; ejecuta 'limpiar' antes.", fecha), call. = FALSE)
  tol <- cfg$limpieza$tolerancia_ohlc %||% 1e-6

  resultados <- list()
  for (activo in activos(cfg)) {
    t <- activo$ticker
    resultados[[t]] <- evaluar_calidad(leer_clean(cfg, t), limpieza$activos[[t]], activo, cfg$calidad, ahora, tol)
  }
  guardar_reporte(resultados, cfg, paste0("calidad_", fecha))
  avisos <- alertas(resultados)
  for (a in avisos) log_warn("CALIDAD %s", a)
  exigir_calidad(resultados)
  list(fecha = fecha, activos = as.list(names(resultados)), alertas = as.list(avisos))
}

# -------------------------------------------------------------------------- 4. transformar
transformar_datos <- function(fecha = NULL, config = NULL, ...) {
  cfg <- .cfg(config)
  macro <- if (.macro_activo(cfg)) leer_macro_clean(cfg) else NULL
  hechos <- character()
  for (activo in activos(cfg)) {
    t <- activo$ticker
    guardar_curada(construir_curada(leer_clean(cfg, t), macro, cfg), cfg, t)
    hechos <- c(hechos, t)
  }
  list(fecha = .fecha(fecha), curados = as.list(hechos))
}

# ------------------------------------------------------------------------------ 5. modelar
#' Entrena y valida (walk-forward) los modelos de cada activo; guarda predicciones y métricas.
modelar_datos <- function(fecha = NULL, config = NULL, ...) {
  cfg <- .cfg(config)
  fecha <- .fecha(fecha)
  faltan <- Filter(function(p) !requireNamespace(p, quietly = TRUE), c("glmnet", "ranger"))
  if (length(faltan)) stop(sprintf("Faltan paquetes para el modelado: %s. Ejecuta instalar_paquetes.R", paste(faltan, collapse = ", ")), call. = FALSE)
  params <- parametros_modelado(cfg)
  preds <- list(); metr <- list(); imps <- list()
  for (activo in activos(cfg)) {
    t <- activo$ticker
    res <- entrenar_y_evaluar(leer_curada(cfg, t), t, params)
    preds[[t]] <- res$predicciones; metr[[t]] <- res$metricas; imps[[t]] <- res$importancias
  }
  predicciones <- do.call(rbind, unname(Filter(Negate(is.null), preds)))
  metricas <- do.call(rbind, unname(Filter(Negate(is.null), metr)))
  if (is.null(predicciones)) stop("Ningún activo tuvo datos suficientes para el modelado.", call. = FALSE)
  guardar_modelos(predicciones, cfg, "predicciones")
  guardar_modelos(metricas, cfg, "metricas")
  guardar_reporte(list(parametros = params, importancias = imps), cfg, paste0("modelos_", fecha))
  list(fecha = fecha, activos = as.list(names(metr)), modelos_recomendados = as.list(
    paste(metricas$ticker, metricas$objetivo, metricas$modelo)[metricas$recomendado]))
}

# ----------------------------------------------------------------------------- 6. backtest
#' Backtesting del VaR, de los modelos en las crisis (entrenados ANTES de cada una) y de la incertidumbre
#' de las métricas. Guarda tablas en data/backtesting y gráficas en data/graficas.
backtest_datos <- function(fecha = NULL, config = NULL, ...) {
  cfg <- .cfg(config)
  fecha <- .fecha(fecha)
  faltan <- Filter(function(p) !requireNamespace(p, quietly = TRUE), c("glmnet", "ranger", "zoo"))
  if (length(faltan)) stop(sprintf("Faltan paquetes para el backtesting: %s. Ejecuta instalar_paquetes.R", paste(faltan, collapse = ", ")), call. = FALSE)
  bp <- parametros_backtesting(cfg); mp <- parametros_modelado(cfg)
  preds <- leer_modelos(cfg, "predicciones")
  tablas <- list(var = list(), crisis = list(), series = list(), ic = list())
  carpeta_g <- file.path(directorio_datos(cfg), "graficas")

  for (activo in activos(cfg)) {
    t <- activo$ticker
    cur <- leer_curada(cfg, t)
    tablas$var[[t]] <- backtest_var(cur, t, bp)
    cr <- backtest_crisis_modelos(cur, t, mp, bp$crisis)
    if (is.null(cr)) log_warn("%s: los datos no cubren ninguna de las ventanas de crisis configuradas", t)
    else { tablas$crisis[[t]] <- cr$resumen; tablas$series[[t]] <- cr$series }

    if (!is.null(preds)) {
      p_t <- preds[preds$ticker == t & preds$origen == "walk_forward", , drop = FALSE]
      for (obj in unique(p_t$objetivo)) for (mod in unique(p_t$modelo)) {
        sub <- p_t[p_t$objetivo == obj & p_t$modelo == mod, , drop = FALSE]
        ic <- backtest_incertidumbre(sub, bp$bloque_bootstrap, bp$remuestreos, bp$semilla)
        if (!is.null(ic)) tablas$ic[[paste(t, obj, mod)]] <- ic
      }
    }
    # gráficas (si faltan paquetes de dibujo no se detiene el flujo)
    tryCatch({
      grafico_violaciones_var(cur, t, file.path(carpeta_g, paste0("backtest_var_", t, ".png")))
      if (!is.null(cr)) grafico_crisis(cr$series, cr$resumen, t, file.path(carpeta_g, paste0("backtest_crisis_", t, ".png")))
    }, error = function(e) log_warn("%s: no se pudieron dibujar las gráficas del backtesting (%s)", t, conditionMessage(e)))
    log_info("Backtesting %s listo", t)
  }
  juntar <- function(l) { l <- Filter(Negate(is.null), unname(l)); if (length(l)) do.call(rbind, l) else NULL }
  salida <- list(backtest_var = juntar(tablas$var), backtest_crisis = juntar(tablas$crisis),
                 backtest_crisis_series = juntar(tablas$series), backtest_modelos_ic = juntar(tablas$ic))
  for (n in names(salida)) if (!is.null(salida[[n]])) guardar_tabla(salida[[n]], cfg, "backtesting", n)

  v <- salida$backtest_var
  rechazos <- if (is.null(v)) 0L else sum(v$rechaza & v$ventana != "Toda la muestra" & v$metodo == "historico")
  list(fecha = fecha, tablas = as.list(names(Filter(Negate(is.null), salida))),
       pruebas_var_historico_rechazadas_en_crisis = rechazos)
}

# ------------------------------------------------------------------------------ 7. cargar
cargar_datos <- function(fecha = NULL, config = NULL, ...) {
  cfg <- .cfg(config)
  fecha <- .fecha(fecha)
  limpieza <- leer_reporte(cfg, paste0("limpieza_", fecha)) %||% list(activos = list())
  calidad <- leer_reporte(cfg, paste0("calidad_", fecha)) %||% list()
  filas <- list()
  con_conexion(cfg, function(con) {
    for (activo in activos(cfg)) {
      t <- activo$ticker
      n1 <- cargar_dataframe(con, "precios_limpios", leer_clean(cfg, t), t)
      n2 <- cargar_dataframe(con, "caracteristicas", leer_curada(cfg, t), t)
      cargar_reporte(con, fecha, t, limpieza$activos[[t]] %||% list(), calidad[[t]] %||% list())
      filas[[t]] <<- list(precios_limpios = n1, caracteristicas = n2)
    }
    # resultados de los modelos (si se ejecutó la etapa `modelar`)
    preds <- leer_modelos(cfg, "predicciones"); metr <- leer_modelos(cfg, "metricas")
    if (!is.null(preds)) for (t in unique(preds$ticker)) {
      filas[[t]]$predicciones <<- cargar_dataframe(con, "predicciones", preds[preds$ticker == t, ], t,
                                                    claves = c("ticker", "fecha", "objetivo", "modelo", "origen"))
      filas[[t]]$metricas_modelos <<- cargar_dataframe(con, "metricas_modelos", metr[metr$ticker == t, ], t, claves = NULL)
    }
    # resultados del backtesting (si se ejecutó la etapa `backtest`)
    for (nombre in c("backtest_var", "backtest_crisis", "backtest_modelos_ic")) {
      df <- leer_tabla(cfg, "backtesting", nombre)
      if (!is.null(df)) for (t in unique(df$ticker)) {
        filas[[t]][[nombre]] <<- cargar_dataframe(con, nombre, df[df$ticker == t, , drop = FALSE], t, claves = NULL)
      }
    }
  })
  list(fecha = fecha, filas = filas)
}

# ------------------------------------------------------------------------------- CLI
ETAPAS <- list(extraer = extraer_datos, limpiar = limpiar_datos, validar = validar_calidad,
               transformar = transformar_datos, modelar = modelar_datos, backtest = backtest_datos, cargar = cargar_datos)

ejecutar <- function(cfg, fecha = NULL, etapas = names(ETAPAS)) {
  resumen <- list()
  for (nombre in etapas) {
    log_info("== Etapa: %s", nombre)
    resumen[[nombre]] <- ETAPAS[[nombre]](fecha = fecha, config = cfg)
  }
  resumen
}

# Lee argumentos de la forma: --demo --sin-macro --fecha 2026-11-02 --etapas limpiar validar
parsear_argumentos <- function(args) {
  out <- list(etapas = NULL, config = NULL, fecha = NULL, demo = FALSE, sin_macro = FALSE)
  i <- 1L
  while (i <= length(args)) {
    a <- args[i]
    if (a == "--demo") out$demo <- TRUE
    else if (a == "--sin-macro") out$sin_macro <- TRUE
    else if (a == "--config") { out$config <- args[i + 1L]; i <- i + 1L }
    else if (a == "--fecha") { out$fecha <- args[i + 1L]; i <- i + 1L }
    else if (a == "--etapas") {
      j <- i + 1L
      while (j <= length(args) && !startsWith(args[j], "--")) { out$etapas <- c(out$etapas, args[j]); j <- j + 1L }
      i <- j - 1L
    } else stop(sprintf("Argumento desconocido: %s (usa --demo, --sin-macro, --fecha, --etapas, --config)", a), call. = FALSE)
    i <- i + 1L
  }
  if (!is.null(out$etapas) && !all(out$etapas %in% names(ETAPAS))) {
    stop(sprintf("Etapas válidas: %s", paste(names(ETAPAS), collapse = ", ")), call. = FALSE)
  }
  out
}

main_pipeline <- function(args = commandArgs(trailingOnly = TRUE)) {
  a <- parsear_argumentos(args)
  cfg <- cargar_config(a$config)
  if (a$sin_macro) cfg$macro$habilitado <- FALSE
  etapas <- a$etapas
  if (a$demo) {
    cfg <- preparar_demo(cfg)
    etapas <- setdiff(etapas %||% names(ETAPAS), "extraer")
  }
  resultado <- tryCatch(ejecutar(cfg, a$fecha, etapas %||% names(ETAPAS)),
                        error_calidad = function(e) { message(conditionMessage(e)); NULL },
                        error_api = function(e) { message(conditionMessage(e)); NULL })
  if (is.null(resultado)) return(invisible(1L))
  cat(jsonlite::toJSON(resultado, auto_unbox = TRUE, pretty = TRUE, null = "null"), "\n")
  invisible(0L)
}
