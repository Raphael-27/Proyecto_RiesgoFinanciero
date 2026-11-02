"""Datos sintéticos para pruebas y demostraciones sin acceso a las APIs.

`serie_sintetica` reproduce la prueba de la Tabla 4 del documento: 1000 días hábiles con
dos fechas duplicadas, dos registros incoherentes, dos nulos consecutivos, un error de captura
aislado (precio x10 un solo día) y una caída persistente del 30 %.
"""

from __future__ import annotations

from pathlib import Path

import numpy as np
import pandas as pd

PRECIOS = ["open", "high", "low", "close", "adj_close"]


def serie_ohlc(dias: pd.DatetimeIndex, semilla: int = 42, precio_inicial: float = 100.0) -> pd.DataFrame:
    rng = np.random.default_rng(semilla)
    n = len(dias)
    cierre = precio_inicial * np.exp(np.cumsum(rng.normal(0.0003, 0.01, n)))
    apertura = np.r_[precio_inicial, cierre[:-1]] * (1 + rng.normal(0, 0.002, n))
    maximo = np.maximum(apertura, cierre) * (1 + np.abs(rng.normal(0, 0.004, n)))
    minimo = np.minimum(apertura, cierre) * (1 - np.abs(rng.normal(0, 0.004, n)))
    return pd.DataFrame({
        "open": apertura, "high": maximo, "low": minimo, "close": cierre, "adj_close": cierre,
        "volume": rng.integers(1_000_000, 5_000_000, n).astype(float),
    }, index=pd.DatetimeIndex(dias, name="fecha"))


# posiciones de las anomalías inyectadas (índice de fila de la serie base)
POS = {"duplicados": (200, 300), "negativo": 400, "high_menor_low": 450,
       "nulos": (500, 501), "error_captura": 600, "caida": 700}


def serie_sintetica(dias: pd.DatetimeIndex | None = None, semilla: int = 42,
                    con_anomalias: bool = True):
    """Devuelve (df_raw, base): `base` es la serie sin anomalías, `df_raw` la que se limpia."""
    if dias is None:
        dias = pd.bdate_range("2018-01-01", periods=1000)
    base = serie_ohlc(dias, semilla)
    if not con_anomalias:
        return base.copy(), base
    df = base.copy()

    # caída persistente del 30 % (crisis real) desde la fila 700
    df.iloc[POS["caida"]:, df.columns.get_indexer(PRECIOS)] *= 0.7
    # error de captura: un precio x10 durante un solo día
    df.iloc[POS["error_captura"], df.columns.get_indexer(PRECIOS)] *= 10
    # registros incoherentes
    df.iloc[POS["negativo"], df.columns.get_indexer(PRECIOS)] = -5.0
    i = POS["high_menor_low"]
    df.iloc[i, df.columns.get_loc("high")] = df.iloc[i]["low"] * 0.98
    # nulos consecutivos
    a, b = POS["nulos"]
    df.iloc[a:b + 1, df.columns.get_indexer(PRECIOS)] = np.nan

    # dos fechas duplicadas llegan DESPUÉS con valores corregidos (+0,1 %): debe ganar el último
    dup = df.iloc[list(POS["duplicados"])].copy()
    dup[PRECIOS] = dup[PRECIOS] * 1.001
    return pd.concat([df, dup]), base


def preparar_demo(cfg: dict, directorio: str = "data/demo") -> dict:
    """Prepara un entorno de demostración aislado (datos y base de datos en `directorio`) y escribe
    en la capa raw series sintéticas para cada activo y variable macro configurados.

    Devuelve una copia de la configuración apuntando al entorno de demostración.
    """
    import copy

    from .almacen import guardar_raw
    from .calendario import dias_esperados

    cfg = copy.deepcopy(cfg)
    cfg["proyecto"]["directorio_datos"] = str(directorio)
    cfg["carga"]["base_datos_url"] = f"sqlite:///{Path(directorio) / 'riesgo.db'}"

    for i, activo in enumerate(cfg["activos"]):
        dias = dias_esperados("2022-01-03", "2027-12-31", activo.get("calendario"))
        if dias is None:
            dias = pd.bdate_range("2022-01-03", periods=1000)
        raw, _ = serie_sintetica(dias[:1000], semilla=42 + i)
        guardar_raw(raw, cfg, activo.get("fuente", "yahoo"), activo["ticker"])

    rng = np.random.default_rng(7)
    for serie in cfg.get("macro", {}).get("series", []):
        mensual = serie.get("rezago_dias", 1) > 10
        fechas = pd.date_range("2021-06-01", "2026-10-31", freq="MS" if mensual else "B")
        valores = pd.Series(np.cumsum(rng.normal(0, 1, len(fechas))) + 20, index=fechas).round(3).astype(str)
        valores.iloc[3] = "."   # FRED marca los faltantes con "."
        df = pd.DataFrame({"valor": valores.to_numpy()}, index=pd.DatetimeIndex(fechas, name="fecha"))
        guardar_raw(df, cfg, "fred", serie["id"])
    return cfg
