"""Los clientes se prueban con respuestas simuladas (no hay acceso a Internet en CI).
Los formatos simulados siguen la documentación pública de cada API."""

import pandas as pd
import pytest
import requests

from etl import extraccion
from etl.config import ErrorConfiguracion
from etl.extraccion import ClienteAlphaVantage, ClienteFred, ClienteYahoo, ErrorApi, ErrorApiTemporal
from tests.conftest import RespuestaFalsa, SesionFalsa

AV_OK = {"Meta Data": {"2. Symbol": "IBM"}, "Time Series (Daily)": {
    "2024-01-03": {"1. open": "10", "2. high": "12", "3. low": "9", "4. close": "11", "5. volume": "1000"},
    "2024-01-02": {"1. open": "9", "2. high": "11", "3. low": "8", "4. close": "10", "5. volume": "900"}}}
FRED_OK = {"count": 3, "observations": [
    {"realtime_start": "2024-02-01", "realtime_end": "9999-12-31", "date": "2024-01-01", "value": "3.1"},
    {"realtime_start": "2024-02-02", "realtime_end": "9999-12-31", "date": "2024-01-02", "value": "."},
    {"realtime_start": "2024-02-03", "realtime_end": "9999-12-31", "date": "2024-01-03", "value": "3.3"}]}


# ------------------------------------------------------------------ Alpha Vantage
def test_alphavantage_parsea_y_ordena(cfg, monkeypatch):
    monkeypatch.setenv("ALPHAVANTAGE_API_KEY", "SECRETO")
    s = SesionFalsa(RespuestaFalsa(AV_OK))
    df = ClienteAlphaVantage(cfg, s).descargar("IBM", "2024-01-03")
    assert list(df.columns) == ["open", "high", "low", "close", "volume"]
    assert list(df.index) == [pd.Timestamp("2024-01-03")]          # filtra por fecha de inicio
    assert s.llamadas[0][1]["function"] == "TIME_SERIES_DAILY"


def test_alphavantage_premium_usa_endpoint_ajustado(cfg, monkeypatch):
    monkeypatch.setenv("ALPHAVANTAGE_API_KEY", "K")
    cfg["fuentes"]["alphavantage"]["premium"] = True
    adj = {"Time Series (Daily)": {"2024-01-02": {
        "1. open": "9", "2. high": "11", "3. low": "8", "4. close": "10", "5. adjusted close": "9.5",
        "6. volume": "9", "7. dividend amount": "0", "8. split coefficient": "1"}}}
    s = SesionFalsa(RespuestaFalsa(adj))
    df = ClienteAlphaVantage(cfg, s).descargar("IBM")
    assert s.llamadas[0][1]["function"] == "TIME_SERIES_DAILY_ADJUSTED" and df["adj_close"].iloc[0] == "9.5"


def test_alphavantage_errores(cfg, monkeypatch):
    monkeypatch.setenv("ALPHAVANTAGE_API_KEY", "K")
    cliente = lambda r: ClienteAlphaVantage(cfg, SesionFalsa(r))  # noqa: E731
    with pytest.raises(ErrorApi) as e:       # símbolo inexistente: permanente, no se reintenta
        cliente(RespuestaFalsa({"Error Message": "Invalid API call"})).descargar("XXX")
    assert not isinstance(e.value, ErrorApiTemporal)
    with pytest.raises(ErrorApi) as e:       # endpoint premium con clave gratuita
        cliente(RespuestaFalsa({"Information": "This is a premium endpoint"})).descargar("X")
    assert not isinstance(e.value, ErrorApiTemporal)


def test_alphavantage_limite_de_uso_se_reintenta_y_se_recupera(cfg, monkeypatch):
    monkeypatch.setenv("ALPHAVANTAGE_API_KEY", "K")
    limite = RespuestaFalsa({"Note": "Thank you ... our standard API rate limit is 25 requests per day."})
    s = SesionFalsa(limite, limite, RespuestaFalsa(AV_OK))
    df = ClienteAlphaVantage(cfg, s).descargar("IBM")
    assert len(df) == 2 and len(s.llamadas) == 3


def test_reintentos_se_agotan(cfg, monkeypatch):
    monkeypatch.setenv("ALPHAVANTAGE_API_KEY", "K")
    s = SesionFalsa(RespuestaFalsa(status=503))
    with pytest.raises(ErrorApiTemporal):
        ClienteAlphaVantage(cfg, s).descargar("IBM")
    assert len(s.llamadas) == cfg["extraccion"]["reintentos"] + 1


def test_la_clave_nunca_aparece_en_el_error(cfg, monkeypatch):
    monkeypatch.setenv("ALPHAVANTAGE_API_KEY", "CLAVE-ULTRA-SECRETA")
    err = requests.ConnectionError("fallo en https://x/query?apikey=CLAVE-ULTRA-SECRETA&symbol=IBM")
    cfg["extraccion"]["reintentos"] = 0
    with pytest.raises(ErrorApiTemporal) as e:
        ClienteAlphaVantage(cfg, SesionFalsa(err)).descargar("IBM")
    assert "CLAVE-ULTRA-SECRETA" not in str(e.value) and e.value.__cause__ is None


def test_sin_clave_falla_con_mensaje_claro(cfg, monkeypatch):
    monkeypatch.delenv("ALPHAVANTAGE_API_KEY", raising=False)
    with pytest.raises(ErrorConfiguracion, match="ALPHAVANTAGE_API_KEY"):
        ClienteAlphaVantage(cfg, SesionFalsa(RespuestaFalsa(AV_OK))).descargar("IBM")


# ------------------------------------------------------------------------- FRED
def test_fred_parsea_valores_y_faltantes(cfg, monkeypatch):
    monkeypatch.setenv("FRED_API_KEY", "K")
    s = SesionFalsa(RespuestaFalsa(FRED_OK))
    df = ClienteFred(cfg, s).descargar("VIXCLS", "2024-01-01")
    assert list(df.columns) == ["fecha", "valor"] and df["valor"].tolist() == ["3.1", ".", "3.3"]
    p = s.llamadas[0][1]
    assert p["series_id"] == "VIXCLS" and p["file_type"] == "json" and p["observation_start"] == "2024-01-01"
    assert "realtime_start" not in p


def test_fred_vintage_trae_fecha_de_publicacion(cfg, monkeypatch):
    monkeypatch.setenv("FRED_API_KEY", "K")
    s = SesionFalsa(RespuestaFalsa(FRED_OK))
    df = ClienteFred(cfg, s).descargar("CPIAUCSL", vintage=True)
    assert df["fecha_publicacion"].iloc[0] == "2024-02-01"
    assert s.llamadas[0][1]["realtime_end"] == "9999-12-31"


def test_fred_pagina_hasta_completar(cfg, monkeypatch):
    monkeypatch.setenv("FRED_API_KEY", "K")
    obs = FRED_OK["observations"]
    s = SesionFalsa(RespuestaFalsa({"count": 3, "observations": obs[:2]}),
                    RespuestaFalsa({"count": 3, "observations": obs[2:]}))
    df = ClienteFred(cfg, s).descargar("X")
    assert len(df) == 3 and [c[1]["offset"] for c in s.llamadas] == [0, 2]


def test_fred_error_de_serie_es_permanente(cfg, monkeypatch):
    monkeypatch.setenv("FRED_API_KEY", "K")
    s = SesionFalsa(RespuestaFalsa({"error_code": 400, "error_message": "Bad Request. The series does not exist."}))
    with pytest.raises(ErrorApi) as e:
        ClienteFred(cfg, s).descargar("NOEXISTE")
    assert not isinstance(e.value, ErrorApiTemporal) and len(s.llamadas) == 1


# ------------------------------------------------------------------------ Yahoo
class TickerFalso:
    def __init__(self, resultado, llamadas):
        self.resultado, self.llamadas = resultado, llamadas

    def history(self, **kw):
        self.llamadas.append(kw)
        if isinstance(self.resultado, Exception):
            raise self.resultado
        return self.resultado


def _yahoo(monkeypatch, resultado):
    import yfinance
    llamadas = []
    monkeypatch.setattr(yfinance, "Ticker", lambda t: TickerFalso(resultado, llamadas))
    return llamadas


def test_yahoo_pide_precios_sin_ajustar_y_normaliza_columnas(cfg, monkeypatch):
    idx = pd.DatetimeIndex(["2024-01-02", "2024-01-03"], tz="America/New_York")
    crudo = pd.DataFrame({"Open": [1., 2.], "High": [2., 3.], "Low": [.5, 1.], "Close": [1.5, 2.5],
                          "Adj Close": [1.4, 2.4], "Volume": [10, 20]}, index=idx)
    llamadas = _yahoo(monkeypatch, crudo)
    df = ClienteYahoo(cfg).descargar("AAPL", "2024-01-01")
    assert llamadas[0]["auto_adjust"] is False and llamadas[0]["raise_errors"] is True
    assert list(df.columns) == ["open", "high", "low", "close", "adj_close", "volume"]


def test_yahoo_serie_vacia_y_limite_de_uso_se_reintentan(cfg, monkeypatch):
    llamadas = _yahoo(monkeypatch, pd.DataFrame())
    with pytest.raises(ErrorApiTemporal):
        ClienteYahoo(cfg).descargar("AAPL", "2024-01-01")
    assert len(llamadas) == cfg["extraccion"]["reintentos"] + 1

    from yfinance.exceptions import YFRateLimitError
    _yahoo(monkeypatch, YFRateLimitError())
    cfg["extraccion"]["reintentos"] = 1
    with pytest.raises(ErrorApiTemporal, match="limitó"):
        ClienteYahoo(cfg).descargar("AAPL", "2024-01-01")


def test_yahoo_simbolo_inexistente_no_se_reintenta(cfg, monkeypatch):
    from yfinance.exceptions import YFTickerMissingError
    llamadas = _yahoo(monkeypatch, YFTickerMissingError("ZZZZ", "no existe"))
    with pytest.raises(ErrorApi) as e:
        ClienteYahoo(cfg).descargar("ZZZZ", "2024-01-01")
    assert not isinstance(e.value, ErrorApiTemporal) and len(llamadas) == 1


def test_fuente_desconocida(cfg):
    with pytest.raises(ErrorApi):
        extraccion.extraer_activo(cfg, {"ticker": "X", "fuente": "bloomberg"})
