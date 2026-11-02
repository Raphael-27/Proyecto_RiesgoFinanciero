"""Utilidades numéricas y de fechas compartidas por los módulos del ETL."""

from __future__ import annotations

import numpy as np
import pandas as pd
from numpy.lib.stride_tricks import sliding_window_view


def a_ns(valores):
    """Fuerza resolución datetime64[ns].

    pandas >= 3 infiere resoluciones distintas (s, ms, us, ns) según el origen del dato y
    `merge_asof` falla al unir claves con resoluciones diferentes.
    """
    if isinstance(valores, (pd.DatetimeIndex, pd.Series)):
        return valores.astype("datetime64[ns]")
    return pd.to_datetime(valores).astype("datetime64[ns]")


def ventanas_moviles(x: np.ndarray, ventana: int) -> np.ndarray:
    """Matriz (n, ventana): la fila i contiene x[i-ventana+1 .. i] (NaN al inicio)."""
    x = np.asarray(x, dtype=float)
    relleno = np.full(ventana - 1, np.nan)
    return sliding_window_view(np.concatenate([relleno, x]), ventana)


def seguro_para_ruta(texto: str) -> str:
    """Convierte un ticker (p. ej. '^GSPC', 'EUR/USD') en un nombre de carpeta válido."""
    return "".join(c if c.isalnum() or c in "-_.^=" else "_" for c in texto)
