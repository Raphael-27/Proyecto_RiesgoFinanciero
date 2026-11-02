import copy

import numpy as np
import pandas as pd
import pytest
from sqlalchemy import text

from etl import almacen, tareas
from etl.calidad import ErrorCalidad, alertas, evaluar_calidad, exigir_calidad, perfilar
from etl.carga import cargar_dataframe, crear_motor
from etl.limpieza import limpiar_serie
from etl.sintetico import preparar_demo, serie_sintetica
from etl.transformacion import calcular_caracteristicas, var_es_historico, var_parametrico, zscore_movil

ACTIVO = {"ticker": "T", "calendario": None, "zona_horaria": "America/New_York", "cierre": "16:00"}


def _limpia():
    raw, _ = serie_sintetica()
    return limpiar_serie(raw, "T", fuente="sintetico")


# --------------------------------------------------------------- características de riesgo
def test_var_y_es_historicos_coinciden_con_el_calculo_manual():
    r = pd.Series(np.random.default_rng(3).normal(0, 0.01, 600))
    out = var_es_historico(r, ventana=250, niveles=(0.95, 0.99))
    ventana = r.iloc[-250:]
    q = np.quantile(ventana, 0.05)
    assert out["var_hist_95"].iloc[-1] == pytest.approx(-q)
    assert out["es_hist_95"].iloc[-1] == pytest.approx(-ventana[ventana <= q].mean())
    assert out["var_hist_99"].iloc[-1] > out["var_hist_95"].iloc[-1]
    assert (out["es_hist_95"].dropna() >= out["var_hist_95"].dropna()).all()
    assert out["var_hist_95"].iloc[:249].isna().all()      # sin ventana completa no se calcula


def test_var_parametrico_cercano_al_teorico():
    r = pd.Series(np.random.default_rng(5).normal(0, 0.02, 5000))
    assert var_parametrico(r, 1000)["var_param_95"].iloc[-1] == pytest.approx(1.645 * 0.02, rel=0.1)


def test_caracteristicas_sin_look_ahead():
    """El valor en t no puede cambiar si se descartan los datos posteriores a t."""
    df, _ = _limpia()
    completa = calcular_caracteristicas(df)
    corte = df.index[600]
    parcial = calcular_caracteristicas(df.loc[:corte])
    cols = [c for c in completa.columns if c not in ("ticker",)]
    pd.testing.assert_series_equal(completa.loc[corte, cols].astype(float), parcial.loc[corte, cols].astype(float),
                                   check_names=False)


def test_zscore_excluye_el_dato_actual():
    s = pd.Series(np.r_[np.zeros(300), 5.0])
    z = zscore_movil(s, 252)
    assert np.isnan(z.iloc[10])        # sin historial suficiente no se estandariza
    # con media/desv. calculadas sin el dato actual, un dato extremo no se diluye en su propia ventana
    s2 = pd.Series(np.random.default_rng(0).normal(0, 1, 301))
    s2.iloc[-1] = 20.0
    assert zscore_movil(s2, 252).iloc[-1] > 10


# ---------------------------------------------------------------------------------- calidad
def test_calidad_de_la_serie_sintetica_cumple_tabla3():
    df, rep = _limpia()
    res = evaluar_calidad(df, rep, ACTIVO)
    exigir_calidad({"T": res})                      # no lanza
    assert alertas({"T": res}) == []
    assert {r["dimension"] for r in res} == {"completitud", "unicidad", "validez", "consistencia",
                                             "oportunidad", "impacto_limpieza"}


def test_calidad_detecta_huecos_largos_y_bloquea():
    df, rep = _limpia()
    df = df.drop(df.index[300:306])                  # 6 días hábiles seguidos faltantes
    res = evaluar_calidad(df, rep, ACTIVO)
    with pytest.raises(ErrorCalidad, match="consistencia"):
        exigir_calidad({"T": res})


def test_calidad_detecta_duplicados_e_incoherentes_residuales():
    df, rep = _limpia()
    malo = pd.concat([df, df.iloc[[10]]]).sort_index()
    malo.iloc[5, malo.columns.get_loc("close")] = -1
    res = {r["dimension"]: r for r in evaluar_calidad(malo, rep, ACTIVO)}
    assert not res["unicidad"]["cumple"] and not res["validez"]["cumple"]


def test_completitud_con_calendario_penaliza_dias_rellenados():
    dias = pd.DatetimeIndex(pd.bdate_range("2024-01-02", "2024-06-28"))
    dias = dias[~dias.isin(pd.to_datetime(["2024-01-15", "2024-02-19", "2024-03-29", "2024-05-27", "2024-06-19"]))]
    raw, _ = serie_sintetica(dias, con_anomalias=False)
    raw.iloc[40:44, :5] = np.nan                     # 4 días sin precio (se rellenan, no son "válidos")
    activo = {**ACTIVO, "calendario": "NYSE"}
    df, rep = limpiar_serie(raw, "T", calendario="NYSE")
    res = {r["dimension"]: r for r in evaluar_calidad(df, rep, activo)}
    # 4 rellenados de ~124 sesiones -> 96.8 % < 99 % -> bloquea
    assert res["completitud"]["valor"] < 0.99 and not res["completitud"]["cumple"]


def test_oportunidad_mide_horas_desde_el_cierre():
    df, rep = _limpia()
    hoy = df.index.max()
    activo = {**ACTIVO}
    ny = lambda h, m: pd.Timestamp(f"{hoy.date()} {h:02d}:{m:02d}", tz="America/New_York")  # noqa: E731
    r1 = evaluar_calidad(df, rep, activo, {"evaluar_oportunidad": True}, ahora=ny(17, 30))
    r2 = evaluar_calidad(df, rep, activo, {"evaluar_oportunidad": True}, ahora=ny(22, 0))
    op = lambda r: next(x for x in r if x["dimension"] == "oportunidad")  # noqa: E731
    assert op(r1)["valor"] == pytest.approx(1.5) and op(r1)["cumple"]
    assert op(r2)["valor"] == pytest.approx(6.0) and not op(r2)["cumple"]   # el cron del documento (22:00) incumple


def test_perfilado():
    raw, _ = serie_sintetica()
    p = perfilar(raw, "T")
    assert p["filas"] == 1002 and p["fechas_duplicadas"] == 2 and p["frecuencia"] == "diaria"
    assert p["cobertura"]["dias_recibidos"] == 1000
    assert p["completitud_columnas"]["close"] < 1.0           # los 2 nulos inyectados


# ------------------------------------------------------------------------------------ carga
def test_carga_idempotente_y_evolucion_de_esquema(cfg):
    df, _ = _limpia()
    motor = crear_motor(cfg)
    n1 = cargar_dataframe(motor, "precios_limpios", df, "T")
    n2 = cargar_dataframe(motor, "precios_limpios", df, "T")           # misma fecha dos veces
    cargar_dataframe(motor, "precios_limpios", df, "OTRO")
    with motor.connect() as con:
        assert con.execute(text("select count(*) from precios_limpios where ticker='T'")).scalar() == n1 == n2
        assert con.execute(text("select count(*) from precios_limpios")).scalar() == 2 * n1
        # nueva columna en una ejecución posterior: la tabla se amplía sin perder datos
    df2 = df.assign(nueva_variable=1.5)
    cargar_dataframe(motor, "precios_limpios", df2, "T")
    with motor.connect() as con:
        assert con.execute(text("select count(nueva_variable) from precios_limpios where ticker='T'")).scalar() == n1
        assert con.execute(text("select count(*) from precios_limpios")).scalar() == 2 * n1


def test_indice_unico_impide_duplicados(cfg):
    df, _ = _limpia()
    motor = crear_motor(cfg)
    cargar_dataframe(motor, "precios_limpios", df, "T")
    with motor.begin() as con, pytest.raises(Exception):
        con.execute(text("insert into precios_limpios (ticker, fecha) select ticker, fecha from precios_limpios limit 1"))


# ------------------------------------------------------------------------ pipeline completo
@pytest.fixture
def demo(cfg, tmp_path):
    return preparar_demo(cfg, directorio=str(tmp_path / "demo"))


def _todo(cfg, fecha="2026-11-02"):
    return [f(fecha=fecha, config=cfg) for f in
            (tareas.limpiar_datos, tareas.validar_calidad, tareas.transformar_datos, tareas.cargar_datos)]


def test_pipeline_completo_y_repetible(demo):
    _todo(demo)
    _todo(demo)                                                     # segunda ejecución: mismo resultado
    motor = crear_motor(demo)
    with motor.connect() as con:
        for tabla in ("precios_limpios", "caracteristicas"):
            assert con.execute(text(f"select count(*) from {tabla}")).scalar() == 3 * 1000
        assert con.execute(text("select count(*) from reportes_calidad")).scalar() == 3
        assert con.execute(text("select count(macro_vix) from caracteristicas")).scalar() == 3000
    clean = almacen.leer_clean(demo, "SPY")
    assert len(clean) == 1000 and clean.index.is_monotonic_increasing


def test_la_calidad_detiene_el_flujo_antes_de_la_carga(demo):
    tareas.limpiar_datos(fecha="2026-11-02", config=demo)
    limpio = almacen.leer_clean(demo, "SPY")
    almacen.guardar_clean(limpio.drop(limpio.index[200:210]), demo, "SPY")      # hueco de 10 sesiones
    with pytest.raises(ErrorCalidad, match="SPY"):
        tareas.validar_calidad(fecha="2026-11-02", config=demo)
    motor = crear_motor(demo)
    with motor.connect() as con:  # nada llegó a la base
        assert not con.dialect.has_table(con, "precios_limpios")


def test_extraer_con_clientes_simulados_y_fallos_parciales(cfg, monkeypatch):
    monkeypatch.setenv("FRED_API_KEY", "K")
    cfg["macro"]["habilitado"] = False
    raw, _ = serie_sintetica()

    class Cliente:
        def descargar(self, ticker, inicio):
            if ticker == "MSFT":
                from etl.extraccion import ErrorApi
                raise ErrorApi("símbolo inexistente")
            return raw

    from etl.extraccion import ErrorApi
    with pytest.raises(ErrorApi, match="MSFT"):
        tareas.extraer_datos(config=cfg, clientes={"yahoo": Cliente()})
    assert almacen.leer_raw_ultimo(cfg, "yahoo", "SPY") is not None      # los demás sí quedaron guardados

    cfg2 = copy.deepcopy(cfg)
    cfg2["extraccion"]["tolerar_fallos_parciales"] = True
    res = tareas.extraer_datos(config=cfg2, clientes={"yahoo": Cliente()})
    assert res["fallidos"].keys() == {"MSFT"}


def test_la_capa_raw_conserva_los_datos_originales(cfg):
    raw, _ = serie_sintetica()
    almacen.guardar_raw(raw, cfg, "yahoo", "SPY")
    leido = almacen.leer_raw_ultimo(cfg, "yahoo", "SPY")
    assert len(leido) == 1002 and leido.index.has_duplicates          # duplicados y anomalías intactos
    pd.testing.assert_frame_equal(leido.reset_index(drop=True), raw.reset_index(drop=True), check_freq=False)
