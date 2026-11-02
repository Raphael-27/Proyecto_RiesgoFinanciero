import numpy as np
import pandas as pd
import pytest

from etl.limpieza import (ErrorEsquema, alinear_macro, completar_calendario, eliminar_duplicados,
                          estandarizar_esquema, excluir_no_habiles, limpiar_macro, limpiar_serie,
                          marcar_atipicos, tratar_nulos, validar_coherencia_ohlc)
from etl.sintetico import POS, serie_sintetica


# ----------------------------------------------------------- Tabla 4 del documento
def test_tabla4_serie_sintetica():
    raw, base = serie_sintetica()
    df, rep = limpiar_serie(raw, "SINT")

    assert (rep["filas_entrada"], rep["filas_salida"]) == (1002, 1000)
    assert rep["duplicados"] == 2
    assert rep["incoherentes"] == 2
    assert rep["errores_dato"] == 1
    assert rep["filas_sin_precio"] == 0            # los 2 nulos consecutivos se cubren, no se pierden filas

    dias = base.index
    spike, caida = dias[POS["error_captura"]], dias[POS["caida"]]
    assert df.loc[spike, "error_dato"]
    # el error de captura se reemplaza por el último precio válido
    assert df.loc[spike, "adj_close"] == pytest.approx(base["adj_close"].iloc[POS["error_captura"] - 1])
    # la caída persistente del 30 % se marca pero se conserva
    assert df.loc[caida, "es_atipico"] and not df.loc[caida, "error_dato"]
    assert df["adj_close"].iloc[-1] == pytest.approx(base["adj_close"].iloc[-1] * 0.7)
    # la limpieza deja el conjunto en orden, sin duplicados ni precios inválidos
    assert df.index.is_monotonic_increasing and not df.index.has_duplicates
    assert (df[["open", "high", "low", "close", "adj_close"]] > 0).all().all()


def test_duplicados_conserva_el_ultimo_recibido():
    raw, base = serie_sintetica()
    df, _ = limpiar_serie(raw, "SINT")
    for pos in POS["duplicados"]:
        d = base.index[pos]
        assert df.loc[d, "close"] == pytest.approx(base.loc[d, "close"] * 1.001)


def test_duplicados_con_orden_inestable_de_llegada():
    # 'keep=last' debe respetar el orden de llegada aun con muchas filas (quicksort no es estable)
    idx = pd.DatetimeIndex(["2024-01-02"] * 50 + ["2024-01-03"] * 50)
    df = pd.DataFrame({"close": np.arange(100.0) + 1, "adj_close": np.arange(100.0) + 1}, index=idx)
    out, n = eliminar_duplicados(estandarizar_esquema(df)[0])
    assert n == 98 and list(out["close"]) == [50.0, 100.0]


def test_caida_persistente_no_se_elimina_pero_salto_revertido_si():
    dias = pd.bdate_range("2020-01-01", periods=300)
    rng = np.random.default_rng(1)
    p = 100 * np.exp(np.cumsum(rng.normal(0, 0.01, 300)))
    p[150:] *= 0.65            # crisis real: persiste
    p[220] *= 8                # error: se revierte
    df = pd.DataFrame({"close": p, "adj_close": p}, index=dias)
    out = marcar_atipicos(df)
    assert out.loc[dias[150], "es_atipico"] and not out.loc[dias[150], "error_dato"]
    assert out.loc[dias[220], "error_dato"]


# ------------------------------------------------------------------ esquema y fechas
def test_esquema_exige_columna_de_precio():
    with pytest.raises(ErrorEsquema):
        estandarizar_esquema(pd.DataFrame({"x": [1]}, index=pd.to_datetime(["2024-01-02"])))


def test_texto_en_columnas_numericas_pasa_a_nulo_y_se_cuenta():
    df = pd.DataFrame({"Close": ["10.5", "abc", "11"], "Adj Close": [10.5, 10.6, 11]},
                      index=["2024-01-02", "2024-01-03", "2024-01-04"])
    out, stats = estandarizar_esquema(df)
    assert out["close"].isna().sum() == 1 and stats["valores_no_numericos"] == 1


def test_fecha_local_del_mercado_no_se_corre_de_dia():
    # Tokio: medianoche local = 15:00 UTC del día anterior. Convertir a UTC cambiaría la fecha.
    idx = pd.DatetimeIndex(["2024-03-05"], tz="Asia/Tokyo")
    out, _ = estandarizar_esquema(pd.DataFrame({"close": [1.0]}, index=idx), "Asia/Tokyo")
    assert out.index[0] == pd.Timestamp("2024-03-05")


def test_zonas_horarias_mezcladas_y_fechas_invalidas():
    idx = ["2024-03-05 00:00:00-05:00", "2024-03-06", "2024-03-07T00:00:00+00:00", "no-es-fecha"]
    df = pd.DataFrame({"close": [1.0, 2.0, 3.0, 4.0]}, index=idx)
    out, stats = estandarizar_esquema(df, "America/New_York")
    assert stats["fechas_invalidas"] == 1
    assert list(out.index) == [pd.Timestamp("2024-03-05"), pd.Timestamp("2024-03-06"),
                               pd.Timestamp("2024-03-06")]  # 00:00 UTC = 19:00 del día previo en NY


def test_sin_adj_close_se_usa_close_y_se_informa():
    out, stats = estandarizar_esquema(pd.DataFrame({"close": [1.0]}, index=pd.to_datetime(["2024-01-02"])))
    assert out["adj_close"].iloc[0] == 1.0 and stats["adj_close_es_close"]


# ------------------------------------------------------------- calendario y huecos
def test_excluye_fines_de_semana_y_feriados():
    dias = pd.DatetimeIndex(["2024-07-03", "2024-07-04", "2024-07-06", "2024-07-08"])  # 4-jul feriado, 6-jul sábado
    df = pd.DataFrame({"close": 1.0}, index=dias)
    out, n = excluir_no_habiles(df, "NYSE")
    assert n == 2 and list(out.index.day) == [3, 8]
    out2, n2 = excluir_no_habiles(df, None)   # sin calendario solo excluye fin de semana
    assert n2 == 1 and 4 in out2.index.day


def test_hueco_corto_se_completa_y_largo_se_descarta():
    dias = pd.DatetimeIndex(pd.bdate_range("2024-01-02", "2024-02-29"))
    base = pd.DataFrame({"close": 100.0, "adj_close": 100.0, "open": 100., "high": 101., "low": 99.}, index=dias)
    base = base.drop(base.index[[5, 6]]).drop(base.index[20:25])   # hueco de 2 días y otro de 5
    out, n = completar_calendario(base, "NYSE")
    assert n > 0
    out, _ = tratar_nulos(out.assign(es_atipico=False, error_dato=False), limite_ffill=3)
    assert out.loc[dias[5], "relleno"] and out.loc[dias[6], "relleno"]      # 2 días: cubiertos
    assert dias[23] not in out.index and dias[24] not in out.index          # 5 días: solo 3 se cubren
    assert dias[20] in out.index and out.loc[dias[22], "relleno"]


def test_no_rellena_hacia_atras():
    df = pd.DataFrame({"close": [np.nan, 10.0], "adj_close": [np.nan, 10.0]},
                      index=pd.to_datetime(["2024-01-02", "2024-01-03"]))
    out, n = tratar_nulos(df)
    assert n == 1 and len(out) == 1


def test_coherencia_ohlc():
    df = pd.DataFrame({"open": [10, 10, 10, 10.], "high": [11, 9, 11, 11.], "low": [9, 10, 9, 9.],
                       "close": [10, 10, 12, 10.], "adj_close": [10, 10, 10, 10.],
                       "volume": [1, 1, 1, -5.]})
    out, n = validar_coherencia_ohlc(df)
    assert n == 3 and out["close"].notna().tolist() == [True, False, False, False]


# ---------------------------------------------------------------------------- macro
def _precios(inicio="2024-01-02", n=120):
    return pd.DataFrame({"adj_close": 1.0}, index=pd.bdate_range(inicio, periods=n))


def test_macro_no_tiene_look_ahead_con_rezago_por_serie():
    macro = pd.DataFrame({"serie": "ipc", "fecha": pd.to_datetime(["2024-01-01", "2024-02-01"]),
                          "valor": [100.0, 200.0]})
    out = alinear_macro(_precios(), macro, rezagos={"ipc": 45})
    # el dato de enero (fecha 01-01) solo es utilizable desde el 15-feb; el de febrero desde el 17-mar
    assert out.loc["2024-02-14", "macro_ipc"] != out.loc["2024-02-14", "macro_ipc"]  # NaN
    assert out.loc["2024-02-15", "macro_ipc"] == 100.0
    assert out.loc["2024-03-15", "macro_ipc"] == 100.0
    assert out.loc["2024-03-18", "macro_ipc"] == 200.0


def test_macro_con_fecha_de_publicacion_y_primera_version():
    macro = pd.DataFrame({
        "serie": "pib", "fecha": pd.to_datetime(["2024-01-01", "2024-01-01"]),
        "fecha_publicacion": pd.to_datetime(["2024-02-20", "2024-03-25"]),
        "valor": [1.0, 1.5],   # primera publicación y revisión posterior
    })
    out = alinear_macro(_precios(), macro)
    assert out.loc["2024-02-21", "macro_pib"] == 1.0
    assert out.loc["2024-04-01", "macro_pib"] == 1.0   # la revisión no se usa: se conserva la primera versión


def test_macro_formato_ancho_compatible_y_resoluciones_distintas():
    # Código 1 original: DataFrame ancho. Con pandas >= 3 las fechas pueden venir en s/us/ns.
    ancho = pd.DataFrame({"vix": [10.0, 20.0]},
                         index=pd.DatetimeIndex(["2024-01-02", "2024-01-09"]).astype("datetime64[s]"))
    precios = _precios()
    precios.index = precios.index.astype("datetime64[us]")
    out = alinear_macro(precios, ancho, rezago_dias=1)
    assert out.loc["2024-01-03", "macro_vix"] == 10.0 and out.loc["2024-01-10", "macro_vix"] == 20.0
    assert np.isnan(out.loc["2024-01-02", "macro_vix"])


def test_limpiar_macro_puntos_de_fred():
    df = pd.DataFrame({"fecha": ["2024-01-01", "2024-01-02", "2024-01-03", "2024-01-03"],
                       "valor": ["1.5", ".", "2.0", "2.1"],
                       "fecha_publicacion": ["2024-01-02", "2024-01-03", "2024-01-04", "2024-01-20"]})
    out, rep = limpiar_macro(df, "vix")
    assert rep["nulos_descartados"] == 1 and rep["duplicados"] == 1
    assert out.loc[out["fecha"] == "2024-01-03", "valor"].iloc[0] == 2.0   # primera publicación


def _serie_con_salto(r1, r2, volumen_dia=None, n=300, pos=200):
    """Caminata aleatoria con dos retornos consecutivos r1, r2 (caída/rebote en V) en `pos`."""
    rng = np.random.default_rng(11)
    r = rng.normal(0, 0.008, n)
    r[pos], r[pos + 1] = r1, r2
    p = 100 * np.exp(np.cumsum(r))
    vol = np.full(n, 1_000_000.0)
    if volumen_dia:
        vol[pos] = volumen_dia
    dias = pd.bdate_range("2019-01-01", periods=n)
    return pd.DataFrame({"close": p, "adj_close": p, "volume": vol}, index=dias), dias[pos]


def test_rebote_en_v_de_crisis_real_no_es_error_de_dato():
    """12-13/mar/2020 en el S&P 500: -9,5 % y +8,5 %. Es riesgo real: se marca pero no se corrige."""
    df, dia = _serie_con_salto(-0.10, 0.0855)
    out = marcar_atipicos(df)
    assert out.loc[dia, "es_atipico"] and not out.loc[dia, "error_dato"]


def test_salto_enorme_con_volumen_disparado_es_movimiento_real():
    df, dia = _serie_con_salto(-0.7, 0.69, volumen_dia=8_000_000)
    out = marcar_atipicos(df)
    assert out.loc[dia, "es_atipico"] and not out.loc[dia, "error_dato"]


def test_salto_enorme_sin_volumen_si_es_error_de_captura():
    df, dia = _serie_con_salto(-0.7, 0.69)
    assert marcar_atipicos(df).loc[dia, "error_dato"]
