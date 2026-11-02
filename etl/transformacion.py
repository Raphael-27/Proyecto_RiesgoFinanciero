"""Transformación: variables derivadas de riesgo (capa curated).

Retornos logarítmicos, medias móviles, volatilidad histórica, Valor en Riesgo (VaR) y Déficit
Esperado (Expected Shortfall) históricos y paramétricos, y variables normalizadas. Todas las
variables en la fecha t usan únicamente información disponible hasta t (sin look-ahead).
VaR y ES se expresan como pérdida positiva en fracción del valor (0.03 = 3 %).
"""

from __future__ import annotations

import warnings
from statistics import NormalDist

import numpy as np
import pandas as pd

from .limpieza import alinear_macro
from .utils import ventanas_moviles


def retornos_log(precio: pd.Series) -> pd.Series:
    return np.log(precio.where(precio > 0)).diff()


def var_es_historico(retornos: pd.Series, ventana: int = 250, niveles=(0.95, 0.99)) -> pd.DataFrame:
    """VaR y ES históricos sobre ventana móvil (exige ventana completa de datos)."""
    w = ventanas_moviles(retornos.to_numpy(), ventana)
    completa = np.sum(~np.isnan(w), axis=1) >= ventana
    out = {}
    with warnings.catch_warnings():
        warnings.simplefilter("ignore", RuntimeWarning)
        for nivel in niveles:
            etiqueta = int(round(nivel * 100))
            q = np.nanquantile(w, 1 - nivel, axis=1)
            cola = np.where(w <= q[:, None], w, np.nan)
            var = -q
            es = -np.nanmean(cola, axis=1)
            var[~completa] = np.nan
            es[~completa] = np.nan
            out[f"var_hist_{etiqueta}"] = var
            out[f"es_hist_{etiqueta}"] = es
    return pd.DataFrame(out, index=retornos.index)


def var_parametrico(retornos: pd.Series, ventana: int = 250, niveles=(0.95, 0.99)) -> pd.DataFrame:
    """VaR normal: -(media + z * desviación) con z = Φ⁻¹(1 - nivel)."""
    mu = retornos.rolling(ventana, min_periods=ventana).mean()
    sd = retornos.rolling(ventana, min_periods=ventana).std()
    return pd.DataFrame({
        f"var_param_{int(round(n * 100))}": -(mu + NormalDist().inv_cdf(1 - n) * sd) for n in niveles
    }, index=retornos.index)


def zscore_movil(serie: pd.Series, ventana: int = 252) -> pd.Series:
    """Estandarización con media/desviación de la ventana ANTERIOR (excluye el dato actual)."""
    previo = serie.shift(1)
    mu = previo.rolling(ventana, min_periods=max(ventana // 4, 2)).mean()
    sd = previo.rolling(ventana, min_periods=max(ventana // 4, 2)).std()
    return (serie - mu) / sd.where(sd > 0)


def calcular_caracteristicas(df_limpio: pd.DataFrame, cfg: dict | None = None) -> pd.DataFrame:
    """Construye las variables derivadas de un activo a partir de su serie limpia."""
    c = (cfg or {}).get("caracteristicas", {})
    medias = c.get("medias_moviles", [20, 50, 200])
    vent_vol = c.get("ventana_volatilidad", 21)
    anio = c.get("dias_anio", 252)
    vent_var = c.get("ventana_var", 250)
    niveles = c.get("niveles_var", [0.95, 0.99])
    vent_z = c.get("ventana_zscore", 252)

    df = df_limpio.sort_index().copy()
    precio = df["adj_close"]
    r = retornos_log(precio)

    out = df[["ticker", "adj_close", "volume", "es_atipico", "error_dato", "relleno"]].copy() \
        if "volume" in df else df[["ticker", "adj_close", "es_atipico", "error_dato", "relleno"]].copy()
    out["retorno_log"] = r
    for m in medias:
        out[f"ma_{m}"] = precio.rolling(m, min_periods=m).mean()
    out[f"vol_hist_{vent_vol}"] = r.rolling(vent_vol, min_periods=vent_vol).std() * np.sqrt(anio)
    out = out.join(var_es_historico(r, vent_var, niveles)).join(var_parametrico(r, vent_var, niveles))
    out["retorno_z"] = zscore_movil(r, vent_z)
    if "volume" in df:
        out["volumen_z"] = zscore_movil(np.log1p(df["volume"]), vent_z)
    return out


def construir_curada(df_limpio: pd.DataFrame, macro_largo: pd.DataFrame | None, cfg: dict | None = None,
                     rezagos: dict | None = None) -> pd.DataFrame:
    """Características + variables macro alineadas por fecha de disponibilidad.

    `rezagos` = {serie: días}. Si no se indica, se toma de `macro.series` de la configuración.
    """
    curada = calcular_caracteristicas(df_limpio, cfg)
    if macro_largo is not None and len(macro_largo):
        if rezagos is None:
            rezagos = {s["nombre"]: s.get("rezago_dias", 1) for s in (cfg or {}).get("macro", {}).get("series", [])}
        curada = alinear_macro(curada, macro_largo, rezago_dias=1, rezagos=rezagos)
    return curada
