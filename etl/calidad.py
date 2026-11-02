"""Perfilado inicial y validaciones automáticas de calidad (Tabla 3 del documento)."""

from __future__ import annotations

from datetime import datetime, timezone

import numpy as np
import pandas as pd

from .calendario import dias_esperados, es_dia_habil
from .limpieza import COLUMNAS_PRECIO, _mascara_incoherente, estandarizar_esquema

DEFECTO = {
    "completitud_min": 0.99, "duplicados_max": 0, "incoherentes_max": 0,
    "huecos_max_dias_habiles": 3, "oportunidad_max_horas": 2, "impacto_max_pct": 0.02,
}


class ErrorCalidad(Exception):
    """Alguna métrica bloqueante incumplió su umbral: el flujo se detiene antes de la carga."""


# ----------------------------------------------------------------------------- perfilado
def _frecuencia(indice: pd.DatetimeIndex) -> str:
    if len(indice) < 3:
        return "indeterminada"
    mediana = pd.Series(indice).diff().dropna().median() / pd.Timedelta(days=1)
    if mediana <= 0.5:
        return "intradia"
    if mediana <= 4:
        return "diaria"
    if mediana <= 10:
        return "semanal"
    if mediana <= 40:
        return "mensual"
    return "trimestral_o_menor"


def perfilar(df_raw: pd.DataFrame, ticker: str, calendario: str | None = None,
             zona_horaria: str | None = None) -> dict:
    """Línea base de calidad de una serie ANTES de limpiar (completitud, unicidad, rango,
    cobertura temporal y frecuencia)."""
    df, stats = estandarizar_esquema(df_raw, zona_horaria)
    perfil: dict = {"ticker": ticker, "filas": int(len(df)), **stats}

    perfil["completitud_columnas"] = {
        c: round(float(df_raw[c].notna().mean()), 6) if c in df_raw.columns else None
        for c in df.columns if c in COLUMNAS_PRECIO + ["volume"]}
    perfil["fechas_duplicadas"] = int(df.index.duplicated().sum())

    precio = df["adj_close"].where(df["adj_close"] > 0)
    retornos = np.log(precio).diff().replace([np.inf, -np.inf], np.nan).dropna()
    rangos = {}
    for nombre, serie in (("adj_close", precio), ("volume", df.get("volume")), ("retorno_log", retornos)):
        if serie is None or serie.dropna().empty:
            continue
        s = serie.dropna()
        rangos[nombre] = {
            "min": float(s.min()), "p01": float(s.quantile(.01)), "p50": float(s.quantile(.5)),
            "p99": float(s.quantile(.99)), "max": float(s.max()), "std": float(s.std()),
        }
    perfil["rangos"] = rangos

    unicas = df.index.unique().sort_values()
    perfil["cobertura"] = {
        "inicio": str(unicas.min().date()) if len(unicas) else None,
        "fin": str(unicas.max().date()) if len(unicas) else None,
        "dias_recibidos": int(len(unicas)),
    }
    if len(unicas):
        esperados = dias_esperados(unicas.min(), unicas.max(), calendario)
        n_esp = len(esperados) if esperados is not None else len(pd.bdate_range(unicas.min(), unicas.max()))
        perfil["cobertura"]["dias_habiles_esperados"] = int(n_esp)
    perfil["frecuencia"] = _frecuencia(unicas)
    return perfil


# ---------------------------------------------------------------------- validaciones
def _resultado(dimension, metrica, valor, umbral, cumple, bloqueante, detalle=""):
    return {"dimension": dimension, "metrica": metrica, "valor": valor, "umbral": umbral,
            "cumple": bool(cumple), "bloqueante": bool(bloqueante), "detalle": detalle}


def _huecos_largos(indice: pd.DatetimeIndex, max_dias: int) -> int:
    """Cantidad de huecos con más de `max_dias` días hábiles faltantes entre dos fechas seguidas."""
    if len(indice) < 2:
        return 0
    d = indice.to_numpy().astype("datetime64[D]")
    faltantes = np.busday_count(d[:-1], d[1:]) - 1
    return int((faltantes > max_dias).sum())


def evaluar_calidad(df: pd.DataFrame, reporte: dict, activo: dict, cfg_calidad: dict | None = None,
                    ahora: datetime | None = None, tolerancia_ohlc: float = 1e-6) -> list[dict]:
    """Calcula las seis dimensiones de la Tabla 3 para una serie ya limpia."""
    cfg_calidad = cfg_calidad or {}
    u = {**DEFECTO, **cfg_calidad.get("umbrales", {})}
    bloqueantes = set(cfg_calidad.get("bloqueantes", [
        "completitud", "unicidad", "validez", "consistencia", "oportunidad"]))
    calendario = activo.get("calendario")
    res = []

    if len(df) == 0:
        return [_resultado("completitud", "serie_vacia", 0, u["completitud_min"], False, True,
                           "La serie quedó vacía tras la limpieza")]

    # Completitud: % de días hábiles esperados con cierre válido (sin contar los rellenados)
    esperados = dias_esperados(df.index.min(), df.index.max(), calendario)
    if esperados is None:
        res.append(_resultado("completitud", "pct_dias_con_cierre_valido", None, u["completitud_min"],
                              True, False, "No evaluada: el activo no tiene calendario de feriados"))
    else:
        validos = df.index[~df["relleno"].to_numpy()] if "relleno" in df else df.index
        valor = float(len(validos.intersection(esperados)) / max(len(esperados), 1))
        res.append(_resultado("completitud", "pct_dias_con_cierre_valido", round(valor, 6),
                              u["completitud_min"], valor >= u["completitud_min"],
                              "completitud" in bloqueantes))

    dup = int(df.index.duplicated().sum())
    res.append(_resultado("unicidad", "fechas_duplicadas", dup, u["duplicados_max"],
                          dup <= u["duplicados_max"], "unicidad" in bloqueantes))

    inc = int(_mascara_incoherente(df, tolerancia_ohlc).sum())
    res.append(_resultado("validez", "registros_incoherentes_ohlc", inc, u["incoherentes_max"],
                          inc <= u["incoherentes_max"], "validez" in bloqueantes))

    fuera_de_orden = int((np.diff(df.index.to_numpy().astype("int64")) < 0).sum())
    huecos = _huecos_largos(df.index, int(u["huecos_max_dias_habiles"]))
    res.append(_resultado("consistencia", "fechas_fuera_de_orden_o_huecos_largos", fuera_de_orden + huecos,
                          0, fuera_de_orden + huecos == 0, "consistencia" in bloqueantes,
                          f"fuera_de_orden={fuera_de_orden}, huecos>{u['huecos_max_dias_habiles']}d={huecos}"))

    res.append(_evaluar_oportunidad(df, activo, cfg_calidad, u, ahora, "oportunidad" in bloqueantes))

    pct = float(reporte.get("pct_modificadas", 0.0))
    res.append(_resultado("impacto_limpieza", "pct_filas_modificadas_o_descartadas", round(pct, 6),
                          u["impacto_max_pct"], pct <= u["impacto_max_pct"], "impacto" in bloqueantes,
                          "Alerta si supera el umbral"))
    return res


def _evaluar_oportunidad(df, activo, cfg_calidad, u, ahora, bloqueante) -> dict:
    nombre = ("oportunidad", "horas_desde_cierre_hasta_dato_limpio")
    if not cfg_calidad.get("evaluar_oportunidad", False):
        return _resultado(*nombre, None, u["oportunidad_max_horas"], True, False,
                          "No evaluada (calidad.evaluar_oportunidad = false)")
    zona = activo.get("zona_horaria", "UTC")
    hh, mm = (int(x) for x in str(activo.get("cierre", "16:00")).split(":"))
    ahora = pd.Timestamp(ahora or datetime.now(timezone.utc))
    ahora = ahora.tz_localize("UTC") if ahora.tzinfo is None else ahora
    local = ahora.tz_convert(zona)
    hoy = pd.Timestamp(local.date())
    cierre = local.normalize() + pd.Timedelta(hours=hh, minutes=mm)
    calendario = activo.get("calendario")

    if not es_dia_habil(hoy, calendario) or local < cierre:
        return _resultado(*nombre, None, u["oportunidad_max_horas"], True, False,
                          "No evaluada: hoy no hay sesión cerrada")
    if df.index.max() < hoy:
        return _resultado(*nombre, None, u["oportunidad_max_horas"], False, bloqueante,
                          "Falta el dato de la sesión de hoy")
    horas = float((local - cierre) / pd.Timedelta(hours=1))
    return _resultado(*nombre, round(horas, 3), u["oportunidad_max_horas"],
                      horas <= u["oportunidad_max_horas"], bloqueante)


def exigir_calidad(resultados: dict[str, list[dict]]) -> None:
    """Lanza ErrorCalidad si alguna métrica bloqueante de algún activo no cumple."""
    fallos = [f"{t}: {r['dimension']}={r['valor']} (umbral {r['umbral']}) {r['detalle']}".strip()
              for t, lista in resultados.items() for r in lista if not r["cumple"] and r["bloqueante"]]
    if fallos:
        raise ErrorCalidad("Calidad insuficiente, se detiene antes de la carga:\n  - " + "\n  - ".join(fallos))


def alertas(resultados: dict[str, list[dict]]) -> list[str]:
    """Métricas no bloqueantes incumplidas (se notifican pero no detienen el flujo)."""
    return [f"{t}: {r['dimension']}={r['valor']} supera/incumple el umbral {r['umbral']}"
            for t, lista in resultados.items() for r in lista if not r["cumple"] and not r["bloqueante"]]
