"""Carga a base de datos relacional (SQLite por defecto, PostgreSQL en producción).

La carga es IDEMPOTENTE: para cada (tabla, activo) se borran las filas previas y se insertan
las nuevas dentro de una misma transacción; ejecutar dos veces el proceso para la misma fecha
deja exactamente el mismo contenido, sin duplicados.

No se implementó InfluxDB (el documento lo menciona como alternativa a PostgreSQL).
"""

from __future__ import annotations

import json
import logging

import pandas as pd
from sqlalchemy import create_engine, inspect, text
from sqlalchemy.engine import Engine

from .config import url_base_datos

logger = logging.getLogger("etl.carga")

TABLAS = {
    "precios_limpios": "capa clean (series de precios, Tabla 6)",
    "caracteristicas": "capa curated (retornos, medias móviles, VaR, ES, macro)",
    "reportes_calidad": "reporte de limpieza y calidad por ejecución",
}


def crear_motor(cfg: dict) -> Engine:
    return create_engine(url_base_datos(cfg), future=True)


def _ident(nombre: str) -> str:
    if not nombre.replace("_", "").isalnum():
        raise ValueError(f"Identificador SQL no permitido: {nombre!r}")
    return f'"{nombre}"'


def _asegurar_esquema(con, tabla: str, df: pd.DataFrame) -> None:
    """Crea la tabla si no existe y agrega columnas nuevas si la tabla evolucionó."""
    insp = inspect(con)
    if not insp.has_table(tabla):
        df.head(0).to_sql(tabla, con, if_exists="append", index=False)
        con.execute(text(f"CREATE UNIQUE INDEX IF NOT EXISTS {_ident('ux_' + tabla)} "
                         f"ON {_ident(tabla)} ({_ident('ticker')}, {_ident('fecha')})"))
        return
    existentes = {c["name"] for c in insp.get_columns(tabla)}
    for col in df.columns:
        if col not in existentes:
            serie = df[col].dropna()
            tipo = ("BOOLEAN" if len(serie) and isinstance(serie.iloc[0], (bool,))
                    else "DOUBLE PRECISION" if len(serie) and isinstance(serie.iloc[0], (int, float))
                    else "TEXT")
            con.execute(text(f"ALTER TABLE {_ident(tabla)} ADD COLUMN {_ident(col)} {tipo}"))


def cargar_dataframe(motor: Engine, tabla: str, df: pd.DataFrame, ticker: str) -> int:
    """Reemplaza atómicamente las filas de `ticker` en `tabla`. Devuelve filas insertadas."""
    # Se conservan los tipos nativos para que la tabla infiera DATE/FLOAT/BOOLEAN; inf -> NULL.
    tipados = df.rename_axis("fecha").reset_index().replace([float("inf"), float("-inf")], float("nan"))
    tipados["fecha"] = pd.to_datetime(tipados["fecha"]).dt.date
    tipados["ticker"] = ticker
    with motor.begin() as con:
        _asegurar_esquema(con, tabla, tipados)
        con.execute(text(f"DELETE FROM {_ident(tabla)} WHERE {_ident('ticker')} = :t"), {"t": ticker})
        if len(tipados):
            tipados.to_sql(tabla, con, if_exists="append", index=False, chunksize=5000)
    return len(tipados)


def cargar_reporte(motor: Engine, ejecucion: str, ticker: str, reporte: dict, calidad: list[dict]) -> None:
    """Guarda el reporte de la ejecución (se reemplaza si se repite la misma ejecución)."""
    with motor.begin() as con:
        con.execute(text("""CREATE TABLE IF NOT EXISTS reportes_calidad (
            ejecucion TEXT NOT NULL, ticker TEXT NOT NULL, reporte TEXT, calidad TEXT,
            PRIMARY KEY (ejecucion, ticker))"""))
        con.execute(text("DELETE FROM reportes_calidad WHERE ejecucion = :e AND ticker = :t"),
                    {"e": ejecucion, "t": ticker})
        con.execute(text("INSERT INTO reportes_calidad (ejecucion, ticker, reporte, calidad) "
                         "VALUES (:e, :t, :r, :c)"),
                    {"e": ejecucion, "t": ticker, "r": json.dumps(reporte, default=str),
                     "c": json.dumps(calidad, default=str)})
