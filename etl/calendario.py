"""Calendarios de negociación (feriados bursátiles) con pandas_market_calendars."""

from __future__ import annotations

import logging

import pandas as pd

logger = logging.getLogger("etl.calendario")


def dias_esperados(inicio, fin, calendario: str | None) -> pd.DatetimeIndex | None:
    """Días de negociación esperados entre `inicio` y `fin` (inclusive).

    - `calendario=None`  -> devuelve None (no se conocen los feriados; no se puede completar
      ni medir completitud).
    - cualquier otro valor -> nombre de pandas_market_calendars (NYSE, NASDAQ, 24/7...).
    """
    if calendario is None:
        return None
    import pandas_market_calendars as mcal

    cal = mcal.get_calendar(calendario)
    dias = cal.valid_days(start_date=pd.Timestamp(inicio), end_date=pd.Timestamp(fin))
    dias = pd.DatetimeIndex(dias)
    if dias.tz is not None:
        dias = dias.tz_localize(None)
    return dias.normalize().astype("datetime64[ns]")


def es_dia_habil(fecha, calendario: str | None) -> bool:
    fecha = pd.Timestamp(fecha).normalize()
    if calendario is None:
        return fecha.day_of_week < 5
    return len(dias_esperados(fecha, fecha, calendario)) > 0
