import pytest

pytest.importorskip("matplotlib")

from etl import tareas  # noqa: E402
from etl.carga import crear_motor  # noqa: E402
from etl.sintetico import preparar_demo  # noqa: E402
from etl.visualizar import (exportar_csv, grafico_comparacion, grafico_riesgo,  # noqa: E402
                            leer_caracteristicas, resumen_riesgo)


@pytest.fixture
def datos(cfg, tmp_path):
    demo = preparar_demo(cfg, directorio=str(tmp_path / "demo"))
    for f in (tareas.limpiar_datos, tareas.validar_calidad, tareas.transformar_datos, tareas.cargar_datos):
        f(fecha="2026-11-02", config=demo)
    return demo


def test_leer_graficar_y_exportar(datos, tmp_path):
    dfs = {t: leer_caracteristicas(datos, t) for t in ("SPY", "AAPL")}
    assert len(dfs["SPY"]) == 1000 and dfs["SPY"]["es_atipico"].dtype == bool

    p1 = grafico_riesgo(dfs["SPY"], "SPY", tmp_path / "g" / "riesgo.png")       # incluye panel VIX (hay macro)
    p2 = grafico_comparacion(dfs, tmp_path / "g" / "comparacion.png")
    assert p1.stat().st_size > 20_000 and p2.stat().st_size > 20_000

    resumen = resumen_riesgo(dfs)
    assert list(resumen.index) == ["SPY", "AAPL"] and (resumen["VaR 95%"] > 0).all()

    csv = exportar_csv(dfs["SPY"], "SPY", tmp_path / "x")
    assert csv.read_bytes().startswith(b"\xef\xbb\xbf")          # utf-8-sig: Excel abre bien los acentos


def test_sin_datos_da_mensaje_claro(cfg):
    crear_motor(cfg)  # base vacía
    with pytest.raises(SystemExit, match="aún no tiene datos"):
        leer_caracteristicas(cfg, "NOEXISTE")
