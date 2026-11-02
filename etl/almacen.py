"""Capas de datos tipo medallón (Tabla 1): raw -> clean -> curated, sobre Parquet.

raw     data/raw/<fuente>/<ticker>/<AAAAMMDDTHHMMSSZ>.parquet   (respuesta original, intacta)
clean   data/clean/ticker=<T>/year=<AAAA>/*.parquet              (particionado por activo y año)
curated data/curated/<ticker>.parquet                            (características + macro)
        data/reportes/                                           (perfilado y calidad por ejecución)
Los datos raw nunca se modifican: cada extracción crea un archivo nuevo.
"""

from __future__ import annotations

import json
import shutil
from datetime import datetime, timezone
from pathlib import Path

import pandas as pd

from .config import directorio_datos
from .utils import seguro_para_ruta


def _raiz(cfg: dict, capa: str) -> Path:
    ruta = directorio_datos(cfg) / capa
    ruta.mkdir(parents=True, exist_ok=True)
    return ruta


# ------------------------------------------------------------------------------ raw
def guardar_raw(df: pd.DataFrame, cfg: dict, fuente: str, clave: str, ahora: datetime | None = None) -> Path:
    ahora = ahora or datetime.now(timezone.utc)
    carpeta = _raiz(cfg, "raw") / fuente / seguro_para_ruta(clave)
    carpeta.mkdir(parents=True, exist_ok=True)
    out = df.copy()
    out.index.name = "fecha"
    out = out.reset_index()
    out["fecha_descarga"] = pd.Timestamp(ahora).tz_localize(None) if ahora.tzinfo is None else \
        pd.Timestamp(ahora).tz_convert("UTC").tz_localize(None)
    out["fuente"] = fuente
    ruta = carpeta / f"{ahora.strftime('%Y%m%dT%H%M%S%f')}Z.parquet"
    out.to_parquet(ruta, index=False)
    return ruta


def leer_raw_ultimo(cfg: dict, fuente: str, clave: str) -> pd.DataFrame:
    carpeta = _raiz(cfg, "raw") / fuente / seguro_para_ruta(clave)
    archivos = sorted(carpeta.glob("*.parquet"))
    if not archivos:
        raise FileNotFoundError(f"No hay datos raw de {fuente}/{clave}. Ejecuta primero la extracción.")
    df = pd.read_parquet(archivos[-1])
    return df.set_index("fecha").drop(columns=["fecha_descarga", "fuente"], errors="ignore")


# ---------------------------------------------------------------------------- clean
def guardar_clean(df: pd.DataFrame, cfg: dict, ticker: str) -> Path:
    """Reescribe por completo la partición del activo (idempotente)."""
    carpeta = _raiz(cfg, "clean") / f"ticker={seguro_para_ruta(ticker)}"
    if carpeta.exists():
        shutil.rmtree(carpeta)
    out = df.rename_axis("fecha").reset_index()
    out["year"] = out["fecha"].dt.year
    out.drop(columns=["ticker"], errors="ignore").to_parquet(carpeta, partition_cols=["year"], index=False)
    return carpeta


def leer_clean(cfg: dict, ticker: str) -> pd.DataFrame:
    carpeta = _raiz(cfg, "clean") / f"ticker={seguro_para_ruta(ticker)}"
    if not carpeta.exists():
        raise FileNotFoundError(f"No hay capa clean para {ticker}. Ejecuta primero la limpieza.")
    df = pd.read_parquet(carpeta).drop(columns=["year"], errors="ignore")
    df["ticker"] = ticker
    return df.set_index("fecha").sort_index()


def guardar_macro_clean(df: pd.DataFrame, cfg: dict) -> Path:
    ruta = _raiz(cfg, "clean") / "macro.parquet"
    df.to_parquet(ruta, index=False)
    return ruta


def leer_macro_clean(cfg: dict) -> pd.DataFrame | None:
    ruta = _raiz(cfg, "clean") / "macro.parquet"
    return pd.read_parquet(ruta) if ruta.exists() else None


# -------------------------------------------------------------------------- curated
def guardar_curada(df: pd.DataFrame, cfg: dict, ticker: str) -> Path:
    ruta = _raiz(cfg, "curated") / f"{seguro_para_ruta(ticker)}.parquet"
    df.rename_axis("fecha").reset_index().to_parquet(ruta, index=False)
    return ruta


def leer_curada(cfg: dict, ticker: str) -> pd.DataFrame:
    ruta = _raiz(cfg, "curated") / f"{seguro_para_ruta(ticker)}.parquet"
    if not ruta.exists():
        raise FileNotFoundError(f"No hay capa curated para {ticker}. Ejecuta primero la transformación.")
    return pd.read_parquet(ruta).set_index("fecha")


# ------------------------------------------------------------------------- reportes
def guardar_reporte(obj, cfg: dict, nombre: str) -> Path:
    carpeta = _raiz(cfg, "reportes")
    ruta = carpeta / f"{nombre}.json"
    ruta.write_text(json.dumps(obj, indent=2, ensure_ascii=False, default=_json_default), encoding="utf-8")
    return ruta


def leer_reporte(cfg: dict, nombre: str):
    ruta = _raiz(cfg, "reportes") / f"{nombre}.json"
    return json.loads(ruta.read_text(encoding="utf-8")) if ruta.exists() else None


def _json_default(o):
    import numpy as np
    if isinstance(o, (np.integer,)):
        return int(o)
    if isinstance(o, (np.floating,)):
        return None if np.isnan(o) else float(o)
    if isinstance(o, (np.bool_,)):
        return bool(o)
    if isinstance(o, (pd.Timestamp, datetime)):
        return o.isoformat()
    raise TypeError(f"No serializable: {type(o)}")
