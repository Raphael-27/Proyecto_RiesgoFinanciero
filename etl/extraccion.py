"""Extracción desde APIs de mercado y macroeconómicas.

Clientes: Yahoo Finance (yfinance), Alpha Vantage y FRED. Todos
  * reintentan con espera creciente ante fallos temporales (red, HTTP 429/5xx, límites de uso),
  * fallan de inmediato ante errores permanentes (clave inválida, símbolo inexistente),
  * leen sus claves de variables de entorno y NUNCA las incluyen en logs ni en excepciones,
  * devuelven los datos tal como llegan (la limpieza ocurre después, sobre copias).

Bloomberg no se incluye: requiere licencia y SDK propietario. El documento también menciona
al Banco de la República; no se implementó porque no hay un endpoint público verificado que
pueda codificarse aquí (FRED publica series colombianas como DEXCOUS para el tipo de cambio).
Para añadir otra fuente basta una clase con `descargar(...)` que devuelva un DataFrame.
"""

from __future__ import annotations

import logging
import time

import pandas as pd
import requests
from tenacity import (before_sleep_log, retry, retry_if_exception_type, stop_after_attempt,
                      wait_exponential)

from .config import obtener_secreto

logger = logging.getLogger("etl.extraccion")


class ErrorApi(Exception):
    """Error permanente: reintentar no ayuda (clave inválida, símbolo inexistente...)."""


class ErrorApiTemporal(ErrorApi):
    """Error transitorio: red caída, límite de uso, HTTP 429/5xx."""


def _redactar(texto: str, secretos: list[str]) -> str:
    for s in secretos:
        if s:
            texto = texto.replace(s, "***")
    return texto


def con_reintentos(cfg: dict):
    """Decorador de reintentos con espera creciente, configurable en `extraccion`."""
    e = cfg.get("extraccion", {})
    return retry(
        retry=retry_if_exception_type(ErrorApiTemporal),
        stop=stop_after_attempt(int(e.get("reintentos", 3)) + 1),
        wait=wait_exponential(multiplier=float(e.get("espera_base_segundos", 5)),
                              max=float(e.get("espera_maxima_segundos", 120))),
        before_sleep=before_sleep_log(logger, logging.WARNING),
        reraise=True,
    )


def _get_json(sesion: requests.Session, url: str, params: dict, timeout: float, secretos: list[str]):
    """GET que clasifica los fallos y oculta las claves (requests incluye la URL en sus errores)."""
    try:
        resp = sesion.get(url, params=params, timeout=timeout)
    except requests.RequestException as exc:
        raise ErrorApiTemporal(_redactar(f"{type(exc).__name__}: {exc}", secretos)) from None
    if resp.status_code == 429 or resp.status_code >= 500:
        raise ErrorApiTemporal(f"HTTP {resp.status_code} en {url}")
    if resp.status_code >= 400:
        raise ErrorApi(f"HTTP {resp.status_code} en {url}: {_redactar(resp.text[:200], secretos)}")
    try:
        return resp.json()
    except ValueError:
        raise ErrorApiTemporal(f"Respuesta no JSON desde {url}") from None


# --------------------------------------------------------------------------- Yahoo
class ClienteYahoo:
    """Precios diarios con yfinance. Se pide `auto_adjust=False` para conservar `Close` (sin
    ajustar por dividendos) y `Adj Close`, necesarios para validar OHLC y calcular retornos."""

    nombre = "yahoo"

    def __init__(self, cfg: dict):
        self.cfg = cfg
        self.descargar = con_reintentos(cfg)(self._descargar)

    def _descargar(self, ticker: str, inicio: str, fin: str | None = None) -> pd.DataFrame:
        import yfinance as yf
        from yfinance import exceptions as yfe

        try:
            df = yf.Ticker(ticker).history(start=inicio, end=fin, interval="1d", auto_adjust=False,
                                           actions=False, raise_errors=True)
        except yfe.YFRateLimitError as exc:
            raise ErrorApiTemporal(f"Yahoo limitó las solicitudes: {exc}") from None
        except (yfe.YFTickerMissingError, yfe.YFInvalidPeriodError) as exc:
            raise ErrorApi(f"Símbolo o periodo inválido para {ticker}: {exc}") from None
        except Exception as exc:  # fallos de red u otros de yfinance: se reintentan
            raise ErrorApiTemporal(f"{type(exc).__name__}: {exc}") from None
        if df is None or df.empty:
            raise ErrorApiTemporal(f"Yahoo devolvió una serie vacía para {ticker}")
        return df.rename(columns=lambda c: str(c).strip().lower().replace(" ", "_"))


# ------------------------------------------------------------------- Alpha Vantage
class ClienteAlphaVantage:
    """TIME_SERIES_DAILY (gratuito, sin ajustar) o TIME_SERIES_DAILY_ADJUSTED (premium).

    Los errores NO llegan como códigos HTTP: la API responde 200 con las claves "Error Message",
    "Note" o "Information" (límite de uso), que aquí se traducen a excepciones.
    """

    nombre = "alphavantage"
    URL = "https://www.alphavantage.co/query"

    def __init__(self, cfg: dict, sesion: requests.Session | None = None):
        self.cfg = cfg
        self.sesion = sesion or requests.Session()
        self.opciones = cfg.get("fuentes", {}).get("alphavantage", {})
        self.descargar = con_reintentos(cfg)(self._descargar)

    def _descargar(self, ticker: str, inicio: str | None = None, fin: str | None = None) -> pd.DataFrame:
        clave = obtener_secreto("ALPHAVANTAGE_API_KEY")
        premium = bool(self.opciones.get("premium", False))
        params = {
            "function": "TIME_SERIES_DAILY_ADJUSTED" if premium else "TIME_SERIES_DAILY",
            "symbol": ticker, "outputsize": self.opciones.get("outputsize", "full"),
            "datatype": "json", "apikey": clave,
        }
        timeout = float(self.cfg.get("extraccion", {}).get("timeout_segundos", 30))
        datos = _get_json(self.sesion, self.URL, params, timeout, [clave])
        df = self.parsear(datos, ticker)
        if inicio:
            df = df[df.index >= pd.Timestamp(inicio)]
        if fin:
            df = df[df.index < pd.Timestamp(fin)]
        return df

    @staticmethod
    def parsear(datos: dict, ticker: str = "") -> pd.DataFrame:
        if "Error Message" in datos:
            raise ErrorApi(f"Alpha Vantage rechazó la consulta de {ticker}: {datos['Error Message']}")
        aviso = datos.get("Note") or datos.get("Information")
        if aviso:  # límite de llamadas por minuto/día o función premium
            texto = str(aviso).lower()
            if "premium" in texto:
                raise ErrorApi(f"Alpha Vantage: función no disponible en tu plan: {aviso}")
            raise ErrorApiTemporal(f"Alpha Vantage limitó las solicitudes: {aviso}")
        clave = next((k for k in datos if k.startswith("Time Series")), None)
        if not clave or not datos[clave]:
            raise ErrorApiTemporal(f"Alpha Vantage no devolvió series para {ticker}: {list(datos)[:3]}")
        df = pd.DataFrame.from_dict(datos[clave], orient="index")
        # columnas "1. open", "2. high", ..., "5. adjusted close", "6. volume"
        df.columns = [c.split(". ", 1)[-1].strip().lower().replace(" ", "_") for c in df.columns]
        df = df.rename(columns={"adjusted_close": "adj_close"})
        df.index = pd.to_datetime(df.index)
        return df.sort_index()


# ------------------------------------------------------------------------------ FRED
class ClienteFred:
    """FRED `series/observations`. Los faltantes llegan como "." (se limpian después).

    modo="vintage" pide todas las versiones (realtime_start/realtime_end) para conocer la
    fecha real de primera publicación de cada dato y evitar look-ahead bias.
    """

    nombre = "fred"
    PAGINA = 100_000  # máximo de observaciones por solicitud

    def __init__(self, cfg: dict, sesion: requests.Session | None = None):
        self.cfg = cfg
        self.sesion = sesion or requests.Session()
        self.url = cfg.get("fuentes", {}).get("fred", {}).get(
            "url", "https://api.stlouisfed.org/fred/series/observations")
        self.descargar = con_reintentos(cfg)(self._descargar)

    def _descargar(self, serie: str, inicio: str | None = None, fin: str | None = None,
                   vintage: bool = False) -> pd.DataFrame:
        clave = obtener_secreto("FRED_API_KEY")
        timeout = float(self.cfg.get("extraccion", {}).get("timeout_segundos", 30))
        base = {"series_id": serie, "api_key": clave, "file_type": "json", "limit": self.PAGINA}
        if inicio:
            base["observation_start"] = inicio
        if fin:
            base["observation_end"] = fin
        if vintage:
            base.update(realtime_start="1776-07-04", realtime_end="9999-12-31")

        filas, offset = [], 0
        while True:
            datos = _get_json(self.sesion, self.url, {**base, "offset": offset}, timeout, [clave])
            if "error_code" in datos:
                codigo = int(datos["error_code"])
                msg = f"FRED error {codigo}: {datos.get('error_message')}"
                raise (ErrorApiTemporal if codigo in (429, 500, 503) else ErrorApi)(msg)
            obs = datos.get("observations")
            if obs is None:
                raise ErrorApiTemporal(f"Respuesta inesperada de FRED para {serie}: {list(datos)[:3]}")
            filas.extend(obs)
            offset += len(obs)
            if not obs or offset >= int(datos.get("count", offset)):
                break
        if not filas:
            raise ErrorApiTemporal(f"FRED devolvió cero observaciones para {serie}")
        df = pd.DataFrame(filas)
        out = pd.DataFrame({"fecha": df["date"], "valor": df["value"]})
        if vintage:  # primera fecha en que cada dato fue conocido
            out["fecha_publicacion"] = df["realtime_start"]
        return out


# ------------------------------------------------------------------- orquestación
def extraer_activo(cfg: dict, activo: dict, clientes: dict | None = None) -> pd.DataFrame:
    """Descarga un activo con el cliente de su fuente."""
    fuente = activo.get("fuente", "yahoo")
    inicio = cfg["extraccion"]["fecha_inicio"]
    clientes = clientes or {}
    if fuente == "yahoo":
        cliente = clientes.get("yahoo") or ClienteYahoo(cfg)
    elif fuente == "alphavantage":
        cliente = clientes.get("alphavantage") or ClienteAlphaVantage(cfg)
    else:
        raise ErrorApi(f"Fuente desconocida para {activo['ticker']}: {fuente}")
    return cliente.descargar(activo["ticker"], inicio)


def pausa_entre_llamadas(cfg: dict, fuente: str) -> None:
    segundos = float(cfg.get("fuentes", {}).get(fuente, {}).get(
        "pausa_entre_llamadas_segundos", cfg.get("fuentes", {}).get(fuente, {}).get(
            "pausa_entre_activos_segundos", 0)))
    if segundos > 0:
        time.sleep(segundos)
