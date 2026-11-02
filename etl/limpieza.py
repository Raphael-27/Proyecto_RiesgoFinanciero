"""Módulo de limpieza del ETL - Sistema de Predicción de Riesgo Financiero.

Cada regla (Tabla 2 del documento) vive en una función independiente y devuelve cuántos
registros tocó, para poder construir el reporte de calidad de cada ejecución.

Correcciones respecto al Código 1 original:
  * `sort_index()` no era estable, por lo que "conservar el último registro recibido" no
    estaba garantizado: ahora se ordena con kind="stable" antes de deduplicar.
  * Las fechas con zona horaria se convertían a UTC antes de normalizar, lo que corre un día
    a las bolsas asiáticas: ahora se toma la fecha local del mercado.
  * El z-score robusto incluía el propio dato evaluado en su ventana (se "auto-contaminaba")
    y usaba `close`; ahora la ventana es solo pasado y se usa `adj_close` (splits/dividendos).
  * Se implementan las reglas que el documento describe pero el código no tenía: días no
    hábiles/feriados, completar huecos de calendario, columnas ticker/fuente/relleno.
  * `alinear_macro` solo aplicaba un rezago fijo; ahora admite fecha de publicación y rezago
    por serie, y evita el fallo de `merge_asof` por resolución de fechas en pandas >= 3.
"""

from __future__ import annotations

import logging
import warnings
from dataclasses import dataclass

import numpy as np
import pandas as pd

from .calendario import dias_esperados
from .utils import a_ns, ventanas_moviles

logger = logging.getLogger("etl.limpieza")

COLUMNAS_PRECIO = ["open", "high", "low", "close", "adj_close"]
COLUMNAS_NUMERICAS = COLUMNAS_PRECIO + ["volume"]


class ErrorEsquema(Exception):
    """La serie no tiene las columnas mínimas para ser limpiada."""


@dataclass(frozen=True)
class ParametrosLimpieza:
    ventana: int = 60
    min_periodos: int = 20
    umbral_atipico: float = 8.0
    tolerancia_reversion: float = 0.25
    salto_minimo_error: float = 0.405   # |retorno log| mínimo (~ +/-50 %) para poder ser error de captura
    factor_volumen_real: float = 3.0    # volumen >= 3x su mediana previa => movimiento real, no error
    limite_ffill: int = 3
    tolerancia_ohlc: float = 1e-6

    @classmethod
    def desde_config(cls, cfg: dict) -> "ParametrosLimpieza":
        c = cfg.get("limpieza", {})
        campos = cls.__dataclass_fields__.keys()
        valores = {
            "umbral_atipico": c.get("umbral_atipico", c.get("umbral")),
            **{k: c[k] for k in campos if k in c},
        }
        return cls(**{k: v for k, v in valores.items() if v is not None and k in campos})


# --------------------------------------------------------------------------- esquema
def _fecha_local(valor, zona_horaria):
    """Fecha calendario de un valor suelto: con zona -> fecha local del mercado; sin zona -> tal cual."""
    ts = pd.to_datetime(valor, errors="coerce")
    if pd.isna(ts):
        return pd.NaT
    if ts.tzinfo is not None:
        ts = ts.tz_convert(zona_horaria) if zona_horaria else ts
        ts = ts.tz_localize(None)
    return ts


def _indice_a_fechas(indice: pd.Index, zona_horaria: str | None) -> pd.DatetimeIndex:
    """Convierte cualquier índice (texto, mixto, con/sin zona horaria) en fechas sin zona."""
    try:
        idx = pd.to_datetime(indice, errors="coerce")
        # Con formatos o zonas horarias mezclados, pandas puede convertir en NaT, SIN avisar, las
        # fechas que no coinciden con el formato de la primera: se detecta y se reanaliza una a una.
        if isinstance(idx, pd.DatetimeIndex) and (idx.isna() & ~pd.isna(indice)).any():
            idx = None
    except (ValueError, TypeError):
        idx = None
    if isinstance(idx, pd.DatetimeIndex):
        if idx.tz is not None:
            idx = idx.tz_convert(zona_horaria) if zona_horaria else idx
            idx = idx.tz_localize(None)
    else:
        idx = pd.DatetimeIndex([_fecha_local(v, zona_horaria) for v in indice])
    return a_ns(idx.normalize())


def estandarizar_esquema(df: pd.DataFrame, zona_horaria: str | None = None):
    """Normaliza nombres de columnas, tipos de dato e índice temporal.

    Devuelve (df, stats) con stats = {"fechas_invalidas", "valores_no_numericos"}.
    El orden de llegada de las filas se conserva (necesario para decidir cuál duplicado es
    "el último recibido").
    """
    df = df.copy()
    df.columns = [str(c).strip().lower().replace(" ", "_") for c in df.columns]

    fechas = _indice_a_fechas(df.index, zona_horaria)
    df.index = pd.DatetimeIndex(fechas, name="fecha")
    validas = df.index.notna()
    stats = {"fechas_invalidas": int((~validas).sum())}
    df = df[validas]

    no_numericos = 0
    for col in COLUMNAS_NUMERICAS:
        if col in df.columns:
            antes = df[col].notna()
            df[col] = pd.to_numeric(df[col], errors="coerce").astype("float64")
            no_numericos += int((antes & df[col].isna()).sum())
    stats["valores_no_numericos"] = no_numericos

    if "close" not in df.columns and "adj_close" not in df.columns:
        raise ErrorEsquema("La serie no tiene columna 'close' ni 'adj_close'.")
    if "close" not in df.columns:
        df["close"] = df["adj_close"]
    if "adj_close" not in df.columns:
        # Fuente sin ajuste (p. ej. Alpha Vantage gratuito): se usa close y se deja constancia.
        df["adj_close"] = df["close"]
        stats["adj_close_es_close"] = True

    return df.sort_index(kind="stable"), stats


# --------------------------------------------------------------------------- reglas
def eliminar_duplicados(df: pd.DataFrame):
    """Conserva el último registro recibido para cada fecha (requiere orden estable)."""
    mascara = df.index.duplicated(keep="last")
    return df[~mascara], int(mascara.sum())


def excluir_no_habiles(df: pd.DataFrame, calendario: str | None = None):
    """Elimina fines de semana y, si hay calendario, feriados bursátiles."""
    if len(df) == 0:
        return df, 0
    if calendario is None:
        habil = df.index.day_of_week < 5
    else:
        esperados = dias_esperados(df.index.min(), df.index.max(), calendario)
        habil = df.index.isin(esperados)
    return df[habil], int((~habil).sum())


def _mascara_incoherente(df: pd.DataFrame, tol: float) -> pd.Series:
    precios = [c for c in COLUMNAS_PRECIO if c in df.columns]
    invalido = (df[precios] <= 0).any(axis=1)
    if {"high", "low"} <= set(df.columns):
        invalido |= df["high"] < df["low"] * (1 - tol)
        invalido |= df["close"] > df["high"] * (1 + tol)
        invalido |= df["close"] < df["low"] * (1 - tol)
    if "volume" in df.columns:
        invalido |= df["volume"] < 0
    return invalido


def validar_coherencia_ohlc(df: pd.DataFrame, tol: float = 1e-6):
    """Invalida (pone en NaN) los registros que violan reglas básicas de un mercado real."""
    invalido = _mascara_incoherente(df, tol)
    columnas = [c for c in COLUMNAS_NUMERICAS if c in df.columns]
    df = df.copy()
    df.loc[invalido, columnas] = np.nan
    return df, int(invalido.sum())


def completar_calendario(df: pd.DataFrame, calendario: str | None):
    """Agrega filas vacías para los días de negociación que faltan dentro del rango.

    Solo se hace con calendario conocido; así los feriados reales NO se rellenan (evita
    retornos artificiales de valor cero). Las filas nuevas se cubren luego con `tratar_nulos`
    si el hueco es corto, o se descartan si es largo.
    """
    if calendario is None or len(df) == 0:
        return df, 0
    esperados = dias_esperados(df.index.min(), df.index.max(), calendario)
    faltantes = esperados.difference(df.index)
    if len(faltantes) == 0:
        return df, 0
    df = df.reindex(df.index.union(faltantes)).rename_axis("fecha")
    return df, int(len(faltantes))


def _mediana_y_mad_movil(x: np.ndarray, ventana: int, min_periodos: int):
    """Mediana y MAD exactos por ventana móvil (con tolerancia a NaN)."""
    w = ventanas_moviles(x, ventana)
    cuenta = np.sum(~np.isnan(w), axis=1)
    with warnings.catch_warnings():
        warnings.simplefilter("ignore", RuntimeWarning)  # ventanas totalmente NaN
        mediana = np.nanmedian(w, axis=1)
        mad = np.nanmedian(np.abs(w - mediana[:, None]), axis=1)
    mediana[cuenta < min_periodos] = np.nan
    mad[cuenta < min_periodos] = np.nan
    return mediana, mad


def marcar_atipicos(df: pd.DataFrame, ventana: int = 60, umbral: float = 8.0,
                    tol: float = 0.25, min_periodos: int = 20,
                    salto_minimo_error: float = 0.405, factor_volumen: float = 3.0) -> pd.DataFrame:
    """Detecta saltos extremos con z-score robusto (mediana y MAD) sobre retornos logarítmicos.

    - `es_atipico`: retorno extremo (se conserva siempre; es información de riesgo).
    - `error_dato`: extremo que se revierte casi por completo al día siguiente (error de
      captura) Y que es desproporcionado (|retorno log| >= `salto_minimo_error`, ~50 %) Y no
      viene acompañado de un volumen disparado. Las caídas persistentes (crisis reales) y los
      rebotes en V de un mercado en pánico (p. ej. 12-13 de marzo de 2020: -9,5 % y +8,5 %) NO
      se marcan como error: son información de riesgo.

    La ventana usa solo observaciones pasadas (excluye el dato evaluado). La comprobación de
    reversión usa el retorno siguiente: es un criterio retrospectivo propio de la limpieza
    histórica, por lo que el último día de una serie no puede clasificarse como error.
    """
    df = df.copy()
    precio = df["adj_close"] if "adj_close" in df.columns else df["close"]
    r = np.log(precio.where(precio > 0)).diff()

    mediana, mad = _mediana_y_mad_movil(r.shift(1).to_numpy(), ventana, min_periodos)
    mad = np.where(mad > 0, mad, np.nan)
    z = 0.6745 * (r.to_numpy() - mediana) / mad

    extremo = pd.Series(np.abs(z) > umbral, index=df.index)  # NaN -> False
    reversion = (r + r.shift(-1)).abs() < tol * r.abs()
    desproporcionado = r.abs() >= salto_minimo_error
    if "volume" in df.columns and factor_volumen:
        vol_previo = df["volume"].shift(1).rolling(ventana, min_periods=min_periodos).median()
        volumen_real = (df["volume"] >= factor_volumen * vol_previo).fillna(False)
    else:
        volumen_real = pd.Series(False, index=df.index)

    df["es_atipico"] = extremo
    df["error_dato"] = extremo & reversion.fillna(False) & desproporcionado & ~volumen_real
    return df


def tratar_nulos(df: pd.DataFrame, limite_ffill: int = 3):
    """Propaga el último precio válido (máx. `limite_ffill` días; nunca hacia atrás).

    Devuelve (df, descartadas). Agrega `relleno` = True en las filas cuyo precio fue propagado.
    """
    precios = [c for c in COLUMNAS_PRECIO if c in df.columns]
    df = df.copy()
    vacio_antes = df["close"].isna()
    df[precios] = df[precios].ffill(limit=limite_ffill)
    df["relleno"] = vacio_antes & df["close"].notna()
    n_inicial = len(df)
    df = df.dropna(subset=["close", "adj_close"])
    return df, n_inicial - len(df)


# --------------------------------------------------------------------------- orquestación
def limpiar_serie(df: pd.DataFrame, ticker: str, params: ParametrosLimpieza | None = None,
                  zona_horaria: str | None = None, calendario: str | None = None,
                  fuente: str | None = None):
    """Orquesta la limpieza de una serie y devuelve (df_limpio, reporte)."""
    p = params or ParametrosLimpieza()
    reporte = {"ticker": ticker, "fuente": fuente, "filas_entrada": len(df)}

    df, stats = estandarizar_esquema(df, zona_horaria)
    reporte.update(stats)
    df, reporte["duplicados"] = eliminar_duplicados(df)
    df, reporte["no_habiles"] = excluir_no_habiles(df, calendario)
    df, reporte["incoherentes"] = validar_coherencia_ohlc(df, p.tolerancia_ohlc)
    df, reporte["dias_completados"] = completar_calendario(df, calendario)

    df = marcar_atipicos(df, p.ventana, p.umbral_atipico, p.tolerancia_reversion, p.min_periodos,
                         p.salto_minimo_error, p.factor_volumen_real)
    reporte["atipicos"] = int(df["es_atipico"].sum())
    reporte["errores_dato"] = int(df["error_dato"].sum())
    precios = [c for c in COLUMNAS_PRECIO if c in df.columns]
    df.loc[df["error_dato"], precios] = np.nan

    df, reporte["filas_sin_precio"] = tratar_nulos(df, p.limite_ffill)
    reporte["rellenadas"] = int(df["relleno"].sum())
    reporte["filas_salida"] = len(df)

    # Filas de la entrada que terminaron eliminadas o corregidas (métrica "impacto", Tabla 3).
    modificadas = (reporte["duplicados"] + reporte["no_habiles"]
                   + reporte["filas_sin_precio"] + reporte["rellenadas"])
    reporte["filas_modificadas"] = int(modificadas)
    reporte["pct_modificadas"] = round(modificadas / max(reporte["filas_entrada"], 1), 6)

    df["ticker"] = ticker
    df["fuente"] = fuente
    logger.info("Limpieza %s: %s", ticker, reporte)
    return df, reporte


# --------------------------------------------------------------------------- macro
def limpiar_macro(df: pd.DataFrame, serie: str):
    """Limpia una serie macro en formato largo: columnas `fecha`, `valor` [, `fecha_publicacion`].

    FRED usa "." para los valores faltantes; se convierten a nulo y se descartan. Si hay
    varias versiones de una misma fecha se conserva la de primera publicación.
    """
    df = df.copy()
    reporte = {"serie": serie, "filas_entrada": len(df)}
    df["fecha"] = a_ns(pd.to_datetime(df["fecha"], errors="coerce"))
    df["valor"] = pd.to_numeric(df["valor"], errors="coerce")
    if "fecha_publicacion" in df.columns:
        df["fecha_publicacion"] = a_ns(pd.to_datetime(df["fecha_publicacion"], errors="coerce"))
        df = df.sort_values(["fecha", "fecha_publicacion"], kind="stable")
    else:
        df = df.sort_values("fecha", kind="stable")
    antes = len(df)
    df = df.dropna(subset=["fecha", "valor"])
    reporte["nulos_descartados"] = antes - len(df)
    antes = len(df)
    # primera publicación de cada fecha (evita usar revisiones que aún no se conocían)
    df = df.drop_duplicates(subset="fecha", keep="first")
    reporte["duplicados"] = antes - len(df)
    df["serie"] = serie
    reporte["filas_salida"] = len(df)
    return df.reset_index(drop=True), reporte


def alinear_macro(df_precios: pd.DataFrame, df_macro: pd.DataFrame, rezago_dias: int = 1,
                  rezagos: dict | None = None, tolerancia: pd.Timedelta | None = None,
                  prefijo: str = "macro_") -> pd.DataFrame:
    """Une variables macro respetando su fecha de publicación (evita look-ahead bias).

    `df_macro` puede venir en:
      * formato largo: columnas `serie`, `fecha`, `valor` y opcionalmente `fecha_publicacion`;
      * formato ancho: índice de fechas y una columna por serie (compatibilidad con el
        Código 1 original).
    Un valor es utilizable a partir de `fecha (o fecha_publicacion) + rezago`. Cada fecha de
    precios recibe el último valor ya disponible (unión hacia atrás). `tolerancia` limita la
    antigüedad máxima admitida del dato.
    """
    rezagos = rezagos or {}
    macro = df_macro.copy()
    if "serie" not in macro.columns:  # ancho -> largo
        macro = macro.rename_axis("fecha").reset_index().melt(
            id_vars="fecha", var_name="serie", value_name="valor")
    macro["fecha"] = a_ns(pd.to_datetime(macro["fecha"]))
    macro["valor"] = pd.to_numeric(macro["valor"], errors="coerce")

    if "fecha_publicacion" in macro.columns:
        base = a_ns(pd.to_datetime(macro["fecha_publicacion"]))
        rezago_defecto = 0
    else:
        base = macro["fecha"]
        rezago_defecto = rezago_dias
    dias = macro["serie"].map(lambda s: rezagos.get(s, rezago_defecto)).astype(int)
    macro["disponible"] = a_ns(base + pd.to_timedelta(dias, unit="D"))
    macro = macro.dropna(subset=["valor", "disponible"])

    salida = df_precios.sort_index().copy()
    izquierda = pd.DataFrame({"fecha": a_ns(salida.index)})
    for serie, grupo in macro.groupby("serie", sort=False):
        grupo = (grupo.sort_values(["fecha", "disponible"], kind="stable")
                 .drop_duplicates(subset="fecha", keep="first")   # primera publicación
                 .sort_values("disponible", kind="stable"))
        unido = pd.merge_asof(
            izquierda, grupo[["disponible", "valor"]],
            left_on="fecha", right_on="disponible", direction="backward", tolerance=tolerancia,
        )
        salida[f"{prefijo}{serie}"] = unido["valor"].to_numpy()
    return salida
