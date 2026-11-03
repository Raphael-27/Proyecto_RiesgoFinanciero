# Calendarios de negociación (feriados bursátiles) con el paquete timeDate.

# Cierres extraordinarios que timeDate 4032.109 no trae (duelo nacional por Bush y Carter).
# Se suman a los definidos en config/config.yaml (calendarios$feriados_extra).
FERIADOS_ESPECIALES <- list(NYSE = c("2018-12-05", "2025-01-09"))

feriados_calendario <- function(calendario, anios) {
  calendario <- toupper(calendario)
  funcion <- switch(calendario,
    NYSE = timeDate::holidayNYSE, NASDAQ = timeDate::holidayNYSE,
    LONDON = timeDate::holidayLONDON, TSX = timeDate::holidayTSX, ZURICH = timeDate::holidayZURICH,
    stop(sprintf("Calendario no soportado: '%s'. Usa NYSE, NASDAQ, LONDON, TSX, 24/7 o null.", calendario),
         call. = FALSE))
  h <- as.Date(format(funcion(anios)))
  clave <- if (calendario == "NASDAQ") "NYSE" else calendario
  extra <- c(FERIADOS_ESPECIALES[[clave]], unlist(getOption("etl.feriados_extra")[[clave]]))
  sort(unique(c(h, as.Date(extra))))
}

#' Días de negociación esperados entre `inicio` y `fin` (inclusive).
#' - calendario NULL  -> NULL (no se conocen los feriados: no se completan huecos ni se mide completitud)
#' - "24/7"           -> todos los días
#' - otro nombre      -> lunes a viernes sin los feriados bursátiles
dias_esperados <- function(inicio, fin, calendario = NULL) {
  if (is.null(calendario) || is.na(calendario)) return(NULL)
  dias <- seq(as.Date(inicio), as.Date(fin), by = "day")
  if (toupper(calendario) == "24/7") return(dias)
  fines <- format(dias, "%u") %in% c("6", "7")
  anios <- seq(as.integer(format(min(dias), "%Y")), as.integer(format(max(dias), "%Y")))
  dias[!fines & !(dias %in% feriados_calendario(calendario, anios))]
}

es_dia_habil <- function(fecha, calendario = NULL) {
  fecha <- as.Date(fecha)
  if (is.null(calendario) || is.na(calendario)) return(!(format(fecha, "%u") %in% c("6", "7")))
  length(dias_esperados(fecha, fecha, calendario)) > 0
}
