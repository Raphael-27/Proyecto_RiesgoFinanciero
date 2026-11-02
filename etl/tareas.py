"""Tareas del ETL (las que invoca el DAG de Airflow o el CLI `python -m etl.pipeline`).

Cada tarea es independiente y se comunica con las demás a través de las capas en disco
(raw -> clean -> curated -> base de datos), no mediante XCom: así pueden reintentarse por
separado y son idempotentes. Devuelven un resumen pequeño y serializable.
"""

from __future__ import annotations

import logging
from datetime import date, datetime

import pandas as pd

from . import almacen
from .calidad import alertas, evaluar_calidad, exigir_calidad, perfilar
from .carga import cargar_dataframe, cargar_reporte, crear_motor
from .config import activos, cargar_config, obtener_secreto
from .extraccion import (ClienteFred, ErrorApi, extraer_activo, pausa_entre_llamadas)
from .limpieza import ParametrosLimpieza, limpiar_macro, limpiar_serie
from .transformacion import construir_curada

logger = logging.getLogger("etl.tareas")


def _cfg(config) -> dict:
    return config if isinstance(config, dict) else cargar_config(config)


def _fecha(fecha) -> str:
    if fecha is None:
        return date.today().isoformat()
    return str(fecha)[:10]


def _macro_activo(cfg: dict) -> bool:
    m = cfg.get("macro", {})
    return bool(m.get("habilitado", True)) and bool(m.get("series"))


# --------------------------------------------------------------------------- 1. extraer
def extraer_datos(fecha=None, config=None, clientes: dict | None = None, **_) -> dict:
    """Descarga activos y variables macro a la capa raw, sin modificar nada."""
    cfg = _cfg(config)
    fallidos, extraidos = {}, []

    for activo in activos(cfg):
        t = activo["ticker"]
        try:
            df = extraer_activo(cfg, activo, clientes)
            almacen.guardar_raw(df, cfg, activo.get("fuente", "yahoo"), t)
            extraidos.append(t)
            logger.info("Extraído %s: %s filas", t, len(df))
        except ErrorApi as exc:
            logger.error("Falló la extracción de %s: %s", t, exc)
            fallidos[t] = str(exc)
        pausa_entre_llamadas(cfg, activo.get("fuente", "yahoo"))

    if _macro_activo(cfg):
        obtener_secreto("FRED_API_KEY")  # falla pronto y con mensaje claro si falta
        fred = (clientes or {}).get("fred") or ClienteFred(cfg)
        vintage = cfg["macro"].get("modo", "rezago") == "vintage"
        for serie in cfg["macro"]["series"]:
            try:
                df = fred.descargar(serie["id"], cfg["extraccion"]["fecha_inicio"], vintage=vintage)
                almacen.guardar_raw(df.set_index("fecha"), cfg, "fred", serie["id"])
                extraidos.append(serie["id"])
            except ErrorApi as exc:
                logger.error("Falló la extracción de %s: %s", serie["id"], exc)
                fallidos[serie["id"]] = str(exc)

    resumen = {"fecha": _fecha(fecha), "extraidos": extraidos, "fallidos": fallidos}
    if fallidos and not cfg["extraccion"].get("tolerar_fallos_parciales", False):
        raise ErrorApi(f"Extracción incompleta, fallaron: {fallidos}")
    return resumen


# ---------------------------------------------------------------------------- 2. limpiar
def limpiar_datos(fecha=None, config=None, **_) -> dict:
    """Perfila y limpia cada serie; guarda la capa clean y los reportes de la ejecución."""
    cfg = _cfg(config)
    fecha = _fecha(fecha)
    params = ParametrosLimpieza.desde_config(cfg)
    perfiles, reportes = {}, {}

    for activo in activos(cfg):
        t, fuente = activo["ticker"], activo.get("fuente", "yahoo")
        crudo = almacen.leer_raw_ultimo(cfg, fuente, t)
        perfiles[t] = perfilar(crudo, t, activo.get("calendario"), activo.get("zona_horaria"))
        limpio, reportes[t] = limpiar_serie(
            crudo, t, params, zona_horaria=activo.get("zona_horaria"),
            calendario=activo.get("calendario"), fuente=fuente)
        almacen.guardar_clean(limpio, cfg, t)

    macro_reportes = {}
    if _macro_activo(cfg):
        partes = []
        for serie in cfg["macro"]["series"]:
            crudo = almacen.leer_raw_ultimo(cfg, "fred", serie["id"]).reset_index()
            limpia, macro_reportes[serie["nombre"]] = limpiar_macro(crudo, serie["nombre"])
            partes.append(limpia)
        almacen.guardar_macro_clean(pd.concat(partes, ignore_index=True), cfg)

    almacen.guardar_reporte(perfiles, cfg, f"perfil_{fecha}")
    almacen.guardar_reporte({"activos": reportes, "macro": macro_reportes}, cfg, f"limpieza_{fecha}")
    return {"fecha": fecha, "limpiados": list(reportes), "macro": list(macro_reportes)}


# ---------------------------------------------------------------------------- 3. validar
def validar_calidad(fecha=None, config=None, ahora: datetime | None = None, **_) -> dict:
    """Aplica la Tabla 3. Si una métrica bloqueante incumple, lanza ErrorCalidad (el DAG se
    detiene antes de la carga). Las métricas no bloqueantes solo generan alertas."""
    cfg = _cfg(config)
    fecha = _fecha(fecha)
    limpieza = almacen.leer_reporte(cfg, f"limpieza_{fecha}")
    if limpieza is None:
        raise FileNotFoundError(f"No existe el reporte de limpieza de {fecha}; ejecuta 'limpiar' antes.")
    tol = cfg.get("limpieza", {}).get("tolerancia_ohlc", 1e-6)

    resultados = {}
    for activo in activos(cfg):
        t = activo["ticker"]
        resultados[t] = evaluar_calidad(almacen.leer_clean(cfg, t), limpieza["activos"][t], activo,
                                        cfg.get("calidad", {}), ahora, tol)
    almacen.guardar_reporte(resultados, cfg, f"calidad_{fecha}")

    for aviso in alertas(resultados):
        logger.warning("ALERTA DE CALIDAD %s", aviso)
    exigir_calidad(resultados)
    return {"fecha": fecha, "activos": list(resultados), "alertas": alertas(resultados)}


# ------------------------------------------------------------------------- 4. transformar
def transformar_datos(fecha=None, config=None, **_) -> dict:
    """Calcula las variables derivadas y las une con las macro (capa curated)."""
    cfg = _cfg(config)
    macro = almacen.leer_macro_clean(cfg) if _macro_activo(cfg) else None
    hechos = []
    for activo in activos(cfg):
        t = activo["ticker"]
        curada = construir_curada(almacen.leer_clean(cfg, t), macro, cfg)
        almacen.guardar_curada(curada, cfg, t)
        hechos.append(t)
    return {"fecha": _fecha(fecha), "curados": hechos}


# ---------------------------------------------------------------------------- 5. cargar
def cargar_datos(fecha=None, config=None, **_) -> dict:
    """Carga clean, curated y reportes a la base de datos (idempotente)."""
    cfg = _cfg(config)
    fecha = _fecha(fecha)
    motor = crear_motor(cfg)
    limpieza = almacen.leer_reporte(cfg, f"limpieza_{fecha}") or {"activos": {}}
    calidad = almacen.leer_reporte(cfg, f"calidad_{fecha}") or {}
    filas = {}
    for activo in activos(cfg):
        t = activo["ticker"]
        n1 = cargar_dataframe(motor, "precios_limpios", almacen.leer_clean(cfg, t), t)
        n2 = cargar_dataframe(motor, "caracteristicas", almacen.leer_curada(cfg, t), t)
        cargar_reporte(motor, fecha, t, limpieza["activos"].get(t, {}), calidad.get(t, []))
        filas[t] = {"precios_limpios": n1, "caracteristicas": n2}
    return {"fecha": fecha, "filas": filas}
