# Modelado predictivo: pronóstico de episodios de alta volatilidad y caídas abruptas.
#
# Dos eventos a `horizonte_dias` (5) días:
#   y_vol_alta : la volatilidad realizada de los próximos 5 días supera el percentil 90 de su propio
#                pasado (umbral calculado SOLO con datos ya conocidos en cada fecha).
#   y_caida    : el retorno acumulado a 5 días es <= -5 %.
# Dos algoritmos: regresión logística regularizada (glmnet) y bosque aleatorio (ranger).
#
# Reglas anti-fuga de información (look-ahead), que son lo más importante de este módulo:
#  * Las etiquetas usan el futuro y por eso NUNCA son variables de entrada.
#  * Las variables de entrada en la fecha t usan solo información hasta t.
#  * Validación "walk-forward": se entrena con el pasado y se predice el periodo siguiente, que se
#    avanza en bloques anuales; nunca se mezclan fechas al azar.
#  * Purga: se descartan del entrenamiento las filas cuyas etiquetas se solapan con el periodo de prueba.
#  * Imputación, estandarización, hiperparámetros y umbral de alerta se calculan solo con datos de
#    entrenamiento de cada bloque.
# Cada modelo se compara con una línea base sencilla (persistencia de la volatilidad / VaR pasado):
# el aprendizaje automático solo aporta valor si le gana.

ETIQUETAS <- c("y_vol_alta", "y_caida")
COLUMNAS_ETIQUETA <- c(ETIQUETAS, "vol_fut", "ret_fut", "umbral_vol")
NOMBRE_OBJETIVO <- c(y_vol_alta = "Alta volatilidad", y_caida = "Caída abrupta")
NOMBRE_MODELO <- c(logistica = "Regresión logística (elastic net)", bosque = "Bosque aleatorio",
                   constante = "Frecuencia histórica")

parametros_modelado <- function(cfg = NULL) {
  p <- list(horizonte_dias = 5L, percentil_volatilidad = 0.90, umbral_caida = -0.05,
            minimo_historia_umbral = 250L, ventana_inicial = 1260L, paso_refit = 252L, purga_dias = 10L,
            semilla = 123L, modelos = c("logistica", "bosque"), proporcion_validacion = 0.2,
            logistica = list(alpha = c(0, 0.5, 1)),
            bosque = list(arboles = 200L, arboles_ajuste = 100L, mtry = c(3L, 6L), min_nodo = c(25L, 100L)))
  m <- cfg$modelado %||% list()
  for (k in names(p)) {
    if (is.null(m[[k]])) next
    p[[k]] <- if (is.list(p[[k]])) utils::modifyList(p[[k]], m[[k]]) else m[[k]]
  }
  p$modelos <- unlist(p$modelos)
  p$logistica$alpha <- unlist(p$logistica$alpha)
  p$bosque$mtry <- unlist(p$bosque$mtry)
  p$bosque$min_nodo <- unlist(p$bosque$min_nodo)
  if (p$purga_dias < p$horizonte_dias) stop("purga_dias debe ser >= horizonte_dias (si no, hay fuga de información).", call. = FALSE)
  p
}

# ---------------------------------------------------------------------- dataset
rezagar <- function(x, k) c(rep(NA_real_, k), x[seq_len(length(x) - k)])              # valor de hace k filas
adelantar <- function(x, k) c(x[(k + 1):length(x)], rep(NA_real_, k))                 # valor k filas adelante

#' Construye variables de entrada (solo pasado) y etiquetas (futuro) a partir de la capa curated.
construir_dataset_modelo <- function(curada, params = parametros_modelado()) {
  d <- curada[order(curada$fecha), , drop = FALSE]
  n <- nrow(d); h <- params$horizonte_dias
  if (n <= h + 30) stop("Muy pocas filas para construir el dataset de modelado.", call. = FALSE)
  precio <- d$adj_close
  r <- d$retorno_log

  X <- data.frame(fecha = d$fecha)
  X$retorno_log <- r
  for (m in grep("^ma_", names(d), value = TRUE)) X[[paste0("dist_", m)]] <- precio / d[[m]] - 1
  X$ret_5d <- precio / rezagar(precio, 5) - 1
  X$ret_21d <- precio / rezagar(precio, 21) - 1
  X$vol_5d <- ventana_movil(r, 5, stats::sd) * sqrt(252)
  vol21 <- grep("^vol_hist_", names(d), value = TRUE)[1]
  X[[vol21]] <- d[[vol21]]
  X$ratio_vol_5_21 <- X$vol_5d / d[[vol21]]
  X$dd_60 <- precio / ventana_movil(precio, 60, max) - 1
  # medidas de riesgo del documento (VaR, ES) y variables estandarizadas. Se omiten las versiones casi
  # idénticas (VaR 99 %, ES 99 %, VaR paramétrico): añaden ruido y no información nueva.
  for (c in intersect(c("var_hist_95", "es_hist_95", "retorno_z", "volumen_z"), names(d))) X[[c]] <- d[[c]]
  if ("es_atipico" %in% names(d)) X$es_atipico <- as.numeric(d$es_atipico)   # usa solo el pasado (no error_dato)
  for (m in grep("^macro_", names(d), value = TRUE)) {
    X[[m]] <- d[[m]]
    X[[paste0("d5_", m)]] <- d[[m]] - rezagar(d[[m]], 5)
  }

  # ---- etiquetas (usan el futuro: solo para entrenar y evaluar) ----
  vol_fut <- rep(NA_real_, n)
  for (t in seq_len(n - h)) vol_fut[t] <- stats::sd(r[(t + 1):(t + h)]) * sqrt(252)
  ret_fut <- adelantar(precio, h) / precio - 1

  # umbral de "alta volatilidad": percentil de las volatilidades futuras YA CONOCIDAS en t
  # (vol_fut[s] se conoce en s + h, así que en t solo valen las de s <= t - h). Se recalcula cada 10
  # filas: usar un valor calculado con menos datos sigue siendo información del pasado.
  umbral <- rep(NA_real_, n); ultimo <- NA_real_
  for (t in seq_len(n)) {
    if (t - h >= params$minimo_historia_umbral && (is.na(ultimo) || t %% 10 == 0)) {
      ultimo <- stats::quantile(vol_fut[seq_len(t - h)], params$percentil_volatilidad, na.rm = TRUE, names = FALSE)
    }
    umbral[t] <- ultimo
  }
  # La etiqueta compara contra `umbral`, que se conoce en t: la volatilidad actual RELATIVA a ese umbral
  # es la variable más directa para pronosticarla (sin fuga: el umbral solo usa el pasado).
  X$vol_rel_21 <- X[[vol21]] / umbral
  X$vol_rel_5 <- X$vol_5d / umbral
  X$y_vol_alta <- ifelse(is.na(vol_fut) | is.na(umbral), NA_real_, as.numeric(vol_fut > umbral))
  X$y_caida <- ifelse(is.na(ret_fut), NA_real_, as.numeric(ret_fut <= params$umbral_caida))
  X$vol_fut <- vol_fut; X$ret_fut <- ret_fut; X$umbral_vol <- umbral
  X$adj_close <- NULL
  X
}

columnas_features <- function(ds) setdiff(names(ds), c("fecha", COLUMNAS_ETIQUETA))

# Línea base: persistencia de la volatilidad (alta vol hoy => alta vol mañana) y VaR pasado (caída)
score_base <- function(ds, objetivo) {
  col <- if (objetivo == "y_vol_alta") grep("^vol_hist_", names(ds), value = TRUE)[1] else "var_hist_95"
  if (is.na(col) || !col %in% names(ds)) return(rep(NA_real_, nrow(ds)))
  ds[[col]]
}

# ------------------------------------------------------------------ particiones
#' Bloques walk-forward sobre las filas con etiqueta. Entrena con todo lo anterior al bloque de
#' prueba, menos `purga_dias` filas (las etiquetas de esas filas se solapan con la prueba).
particiones_walk_forward <- function(y, params) {
  ok <- which(!is.na(y))
  if (length(ok) < 200) return(list())
  ini <- min(params$ventana_inicial, floor(0.5 * length(ok)))
  inicios <- seq(ini + 1, length(ok), by = params$paso_refit)
  folds <- list()
  for (a in inicios) {
    prueba <- ok[a:min(a + params$paso_refit - 1, length(ok))]
    entrena <- ok[ok <= prueba[1] - params$purga_dias - 1]
    if (length(entrena) >= 100) folds[[length(folds) + 1]] <- list(train = entrena, test = prueba)
  }
  folds
}

# ----------------------------------------------------------------------- métricas
#' Área bajo la curva ROC (equivale a la probabilidad de ordenar bien un caso positivo y uno negativo).
auc_roc <- function(y, score) {
  ok <- !is.na(y) & !is.na(score); y <- y[ok]; s <- score[ok]
  n1 <- sum(y == 1); n0 <- sum(y == 0)
  if (n1 == 0 || n0 == 0) return(NA_real_)
  (sum(rank(s)[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

brier <- function(y, p) { ok <- !is.na(y) & !is.na(p); mean((p[ok] - y[ok])^2) }

metricas_clasificacion <- function(y, alerta) {
  ok <- !is.na(y) & !is.na(alerta); y <- y[ok]; a <- alerta[ok]
  tp <- sum(a & y == 1); fp <- sum(a & y == 0); fn <- sum(!a & y == 1)
  precision <- if (tp + fp > 0) tp / (tp + fp) else NA_real_
  recall <- if (tp + fn > 0) tp / (tp + fn) else NA_real_
  f1 <- if (!is.na(precision) && !is.na(recall) && precision + recall > 0) 2 * precision * recall / (precision + recall) else NA_real_
  c(precision = precision, recall = recall, f1 = f1, alertas = sum(a), n = length(y))
}

#' Umbral de alerta: la probabilidad que maximiza F1 entre las que alertan como máximo al `tasa_max`
#' de los días (un sistema de alertas que se dispara a diario no sirve). Si las probabilidades son
#' constantes (modelo sin información) devuelve un umbral inalcanzable: no hay nada que alertar.
umbral_f1 <- function(prob, y, tasa_max = 0.25) {
  ok <- !is.na(prob) & !is.na(y); prob <- prob[ok]; y <- y[ok]
  if (length(prob) < 20 || sum(y == 1) == 0) return(1.01)
  if (diff(range(prob)) < 1e-9) return(1.01)
  candidatos <- unique(stats::quantile(prob, seq(1 - tasa_max, 0.99, by = 0.01), names = FALSE))
  f <- vapply(candidatos, function(u) { m <- metricas_clasificacion(y, prob >= u)["f1"]; if (is.na(m)) 0 else m }, numeric(1))
  candidatos[which.max(f)]
}

# ------------------------------------------------------------ preparar y ajustar
preparar_matriz <- function(ds, filas, columnas, ref = NULL) {
  X <- as.matrix(ds[filas, columnas, drop = FALSE])
  X[!is.finite(X)] <- NA
  if (is.null(ref)) {
    med <- apply(X, 2, stats::median, na.rm = TRUE)
    desv <- apply(X, 2, stats::sd, na.rm = TRUE)
    usar <- which(colMeans(is.na(X)) <= 0.3 & is.finite(med) & is.finite(desv) & desv > 0)
    ref <- list(columnas = columnas[usar], med = med[usar], mu = colMeans(X[, usar, drop = FALSE], na.rm = TRUE), sd = desv[usar])
  }
  X <- X[, ref$columnas, drop = FALSE]
  for (j in seq_along(ref$columnas)) X[is.na(X[, j]), j] <- ref$med[j]      # imputación con la mediana del ENTRENAMIENTO
  list(X = X, ref = ref)
}

estandarizar <- function(X, ref) sweep(sweep(X, 2, ref$mu), 2, ref$sd, "/")

# glmnet avisa cuando una rama muy fina de lambda no converge o cuando un bloque de validación cruzada tiene
# pocos casos positivos. Es esperable en eventos raros y no invalida el ajuste (se devuelven las soluciones
# de lambdas mayores); se silencian SOLO esos dos avisos y cualquier otro se muestra.
sin_avisos_glmnet <- function(expr) {
  withCallingHandlers(expr, warning = function(w) {
    if (grepl("Convergence for|fewer than 8\\s+observations", conditionMessage(w))) invokeRestart("muffleWarning")
  })
}

modelo_constante <- function(y, tipo) list(tipo = "constante", pedido = tipo, p = mean(y), umbral = 1.01)

ajustar_bosque <- function(X, y, mtry, min_nodo, arboles, semilla) {
  d <- data.frame(y = factor(y, levels = c(0, 1)), X)
  ranger::ranger(y ~ ., data = d, probability = TRUE, num.trees = arboles, mtry = min(mtry, ncol(X)),
                 min.node.size = min_nodo, seed = semilla, importance = "impurity", num.threads = 2)
}
prob_bosque <- function(rf, X) stats::predict(rf, data = data.frame(X))$predictions[, "1"]

# Calibración de Platt: p' = plogis(a + b * logit(p)), con a y b ajustados en datos de validación.
# Un bosque suele dar probabilidades demasiado extremas; si no tiene habilidad (b <= 0) la calibración
# lo reduce a la frecuencia histórica, en vez de dejarlo dar probabilidades engañosas.
ajustar_platt <- function(prob, y) {
  p <- pmin(pmax(prob, 1e-4), 1 - 1e-4)
  # con separación casi perfecta glm avisa de no convergencia; el caso se maneja abajo (calibración neutra)
  g <- tryCatch(suppressWarnings(stats::glm(y ~ stats::qlogis(p), family = stats::binomial())), error = function(e) NULL)
  if (is.null(g)) return(c(a = stats::qlogis(mean(y)), b = 0))
  cf <- unname(stats::coef(g))
  if (anyNA(cf) || cf[2] < 0) return(c(a = stats::qlogis(min(max(mean(y), 1e-4), 1 - 1e-4)), b = 0))
  c(a = cf[1], b = cf[2])
}
aplicar_platt <- function(prob, cal) stats::plogis(cal["a"] + cal["b"] * stats::qlogis(pmin(pmax(prob, 1e-4), 1 - 1e-4)))

#' Ajusta un modelo con las filas `filas` (entrenamiento). Los hiperparámetros y el umbral de alerta se
#' eligen con una validación cronológica DENTRO del entrenamiento (último 20 %, con purga).
ajustar_modelo <- function(tipo, ds, filas, columnas, y, params, semilla = params$semilla, hp = NULL) {
  y <- as.numeric(y)
  if (sum(y == 1) < 10 || sum(y == 0) < 10) return(modelo_constante(y, tipo))   # muy pocos casos para aprender
  prep <- preparar_matriz(ds, filas, columnas)
  if (length(prep$ref$columnas) < 2) return(modelo_constante(y, tipo))

  if (tipo == "logistica") {
    Z <- estandarizar(prep$X, prep$ref)
    foldid <- pmin(5L, ceiling(seq_along(y) / (length(y) / 5)))               # 5 bloques cronológicos contiguos
    # alpha = 0 (ridge: reparte el peso entre variables correlacionadas) ... 1 (lasso: elige pocas).
    # Se elige alpha y lambda por la DEVIANZA (log-verosimilitud) en validación cruzada cronológica:
    # es una puntuación propia que premia probabilidades CALIBRADAS. (El AUC no sirve para esto: da lo
    # mismo un modelo muy encogido, con probabilidades casi constantes, que uno bien calibrado.)
    cv <- NULL; mejor_cvm <- Inf
    for (a in params$logistica$alpha) {
      set.seed(semilla)
      cv_a <- tryCatch(sin_avisos_glmnet(glmnet::cv.glmnet(Z, y, family = "binomial", alpha = a, foldid = foldid,
                                                           type.measure = "deviance", keep = TRUE)), error = function(e) NULL)
      if (!is.null(cv_a) && min(cv_a$cvm, na.rm = TRUE) < mejor_cvm) { cv <- cv_a; mejor_cvm <- min(cv_a$cvm, na.rm = TRUE); cv$alpha_elegido <- a }
    }
    if (is.null(cv)) return(modelo_constante(y, tipo))
    i <- match(cv$lambda.min, cv$lambda)
    umbral <- umbral_f1(stats::plogis(cv$fit.preval[, i]), y)
    return(list(tipo = "logistica", cv = cv, ref = prep$ref, umbral = umbral, alpha = cv$alpha_elegido))
  }

  if (tipo == "bosque") {
    n <- length(y)
    corte <- floor((1 - params$proporcion_validacion) * n)
    sub <- seq_len(max(corte - params$purga_dias, 1)); val <- (corte + 1):n
    doble <- length(unique(y[sub])) == 2 && length(unique(y[val])) == 2
    prob_val <- NULL
    if (is.null(hp)) {
      # hiperparámetros por Brier (puntuación propia) en una validación cronológica interna
      grid <- expand.grid(mtry = unique(pmin(params$bosque$mtry, ncol(prep$X))), min_nodo = params$bosque$min_nodo)
      mejor <- 1; mejor_brier <- Inf
      if (doble) for (g in seq_len(nrow(grid))) {
        rf <- ajustar_bosque(prep$X[sub, , drop = FALSE], y[sub], grid$mtry[g], grid$min_nodo[g], params$bosque$arboles_ajuste, semilla)
        pv <- prob_bosque(rf, prep$X[val, , drop = FALSE]); b <- brier(y[val], pv)
        if (!is.na(b) && b < mejor_brier) { mejor_brier <- b; mejor <- g; prob_val <- pv }
      }
      hp <- list(mtry = grid$mtry[mejor], min_nodo = grid$min_nodo[mejor])
    } else if (doble) {                       # hiperparámetros ya elegidos con datos anteriores
      rf <- ajustar_bosque(prep$X[sub, , drop = FALSE], y[sub], hp$mtry, hp$min_nodo, params$bosque$arboles_ajuste, semilla)
      prob_val <- prob_bosque(rf, prep$X[val, , drop = FALSE])
    }
    rf <- ajustar_bosque(prep$X, y, hp$mtry, hp$min_nodo, params$bosque$arboles, semilla)
    cal <- NULL; umbral <- 1.01
    if (!is.null(prob_val)) {
      cal <- ajustar_platt(prob_val, y[val])
      umbral <- umbral_f1(aplicar_platt(prob_val, cal), y[val])
    }
    return(list(tipo = "bosque", rf = rf, ref = prep$ref, umbral = umbral, hiperparametros = hp, calibracion = cal))
  }
  stop(sprintf("Modelo desconocido: %s (usa logistica o bosque)", tipo), call. = FALSE)
}

predecir_modelo <- function(mod, ds, filas) {
  if (mod$tipo == "constante") return(rep(mod$p, length(filas)))
  X <- preparar_matriz(ds, filas, mod$ref$columnas, ref = mod$ref)$X
  if (mod$tipo == "logistica") {
    return(as.numeric(stats::predict(mod$cv, estandarizar(X, mod$ref), s = "lambda.min", type = "response")))
  }
  p <- prob_bosque(mod$rf, X)
  if (!is.null(mod$calibracion)) p <- aplicar_platt(p, mod$calibracion)
  unname(p)
}

#' Importancia de las variables del modelo final (coeficientes estandarizados o importancia de Gini).
importancia_modelo <- function(mod, top = 10) {
  imp <- switch(mod$tipo,
    logistica = { b <- as.matrix(stats::coef(mod$cv, s = "lambda.min"))[-1, 1]; abs(b)[b != 0] },
    bosque = mod$rf$variable.importance,
    constante = numeric())
  if (length(imp) == 0) return(list())
  imp <- sort(imp, decreasing = TRUE)
  imp <- imp[seq_len(min(top, length(imp)))]
  as.list(round(imp / sum(imp), 4))
}

# ------------------------------------------------------------ walk-forward y resumen
walk_forward <- function(ds, objetivo, tipo, params) {
  y <- ds[[objetivo]]; cols <- columnas_features(ds)
  folds <- particiones_walk_forward(y, params)
  base <- score_base(ds, objetivo)
  out <- list(); hp <- NULL      # el bosque elige hiperparámetros en el primer bloque y los reutiliza (más rápido)
  for (k in seq_along(folds)) {
    f <- folds[[k]]
    mod <- ajustar_modelo(tipo, ds, f$train, cols, y[f$train], params, semilla = params$semilla + k, hp = hp)
    if (is.null(hp) && !is.null(mod$hiperparametros)) hp <- mod$hiperparametros
    out[[k]] <- data.frame(fecha = ds$fecha[f$test], objetivo = objetivo, modelo = tipo, ajuste = mod$tipo, y_real = y[f$test],
                           probabilidad = predecir_modelo(mod, ds, f$test), umbral = mod$umbral, score_base = base[f$test],
                           bloque = k, origen = "walk_forward", stringsAsFactors = FALSE)
  }
  if (length(out) == 0) return(NULL)
  res <- do.call(rbind, out)
  attr(res, "hp") <- hp
  res
}

#' Métricas fuera de muestra de un conjunto de predicciones walk-forward.
resumir_metricas <- function(pred) {
  y <- pred$y_real
  m <- metricas_clasificacion(y, pred$probabilidad >= pred$umbral)
  b <- brier(y, pred$probabilidad); b0 <- brier(y, rep(mean(y), length(y)))
  data.frame(objetivo = pred$objetivo[1], modelo = pred$modelo[1], n = length(y), prevalencia = mean(y),
             auc = auc_roc(y, pred$probabilidad), auc_base = auc_roc(y, pred$score_base),
             brier = b, brier_climatologia = b0, habilidad_brier = 1 - b / b0,
             bloques_constantes = mean(pred$ajuste == "constante"),
             precision = unname(m["precision"]), recall = unname(m["recall"]), f1 = unname(m["f1"]),
             alertas = unname(m["alertas"]), stringsAsFactors = FALSE)
}

#' Entrena y evalúa todos los modelos para un activo.
#' Devuelve list(predicciones, metricas, importancias, señal_actual).
entrenar_y_evaluar <- function(curada, ticker, params = parametros_modelado()) {
  ds <- construir_dataset_modelo(curada, params)
  cols <- columnas_features(ds)
  n <- nrow(ds)
  preds <- list(); metr <- list(); imps <- list()

  for (obj in ETIQUETAS) for (tipo in params$modelos) {
    wf <- walk_forward(ds, obj, tipo, params)
    if (is.null(wf)) { log_warn("%s: datos insuficientes para validar %s/%s", ticker, obj, tipo); next }
    wf$ticker <- ticker
    preds[[length(preds) + 1]] <- wf
    m <- resumir_metricas(wf); m$ticker <- ticker; metr[[length(metr) + 1]] <- m

    # modelo final con TODAS las filas ya etiquetadas -> señal de la última fecha
    filas <- which(!is.na(ds[[obj]]))
    mod <- ajustar_modelo(tipo, ds, filas, cols, ds[[obj]][filas], params, semilla = params$semilla, hp = attr(wf, "hp"))
    preds[[length(preds) + 1]] <- data.frame(
      fecha = ds$fecha[n], objetivo = obj, modelo = tipo, ajuste = mod$tipo, y_real = NA_real_,
      probabilidad = predecir_modelo(mod, ds, n), umbral = mod$umbral, score_base = score_base(ds, obj)[n],
      bloque = NA_integer_, origen = "actual", ticker = ticker, stringsAsFactors = FALSE)
    imps[[paste(obj, tipo, sep = "/")]] <- importancia_modelo(mod)
    log_info("Modelo %s %s/%s: AUC=%.3f (base %.3f) habilidad Brier=%+.1f%%", ticker, obj, tipo, m$auc, m$auc_base, 100 * m$habilidad_brier)
  }
  metricas <- if (length(metr)) marcar_recomendado(do.call(rbind, metr)) else NULL
  list(predicciones = if (length(preds)) do.call(rbind, preds) else NULL, metricas = metricas, importancias = imps)
}

#' Marca como `recomendado` el modelo con mayor habilidad de Brier (1 - Brier/Brier de la frecuencia
#' histórica) siempre que sea POSITIVA. Si ningún modelo le gana a repetir la frecuencia histórica, no se
#' recomienda ninguno y las alertas deben apoyarse en los umbrales de VaR y volatilidad.
marcar_recomendado <- function(metricas) {
  metricas$recomendado <- FALSE
  for (t in unique(metricas$ticker)) for (o in unique(metricas$objetivo)) {
    i <- which(metricas$ticker == t & metricas$objetivo == o & is.finite(metricas$habilidad_brier))
    if (length(i) == 0) next
    mejor <- i[which.max(metricas$habilidad_brier[i])]
    if (metricas$habilidad_brier[mejor] > 0) metricas$recomendado[mejor] <- TRUE
  }
  metricas
}
