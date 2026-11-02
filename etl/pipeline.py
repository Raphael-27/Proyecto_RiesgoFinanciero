"""CLI para ejecutar el ETL sin Airflow.

    python -m etl.pipeline                        # todas las etapas con config/config.yaml
    python -m etl.pipeline --etapas limpiar validar
    python -m etl.pipeline --demo                 # datos sintéticos, sin APIs ni claves
    python -m etl.pipeline --sin-macro            # solo precios (no requiere FRED_API_KEY)
"""

from __future__ import annotations

import argparse
import json
import logging
import sys

from . import tareas
from .calidad import ErrorCalidad
from .config import cargar_config
from .extraccion import ErrorApi

ETAPAS = {
    "extraer": tareas.extraer_datos,
    "limpiar": tareas.limpiar_datos,
    "validar": tareas.validar_calidad,
    "transformar": tareas.transformar_datos,
    "cargar": tareas.cargar_datos,
}


def ejecutar(cfg: dict, fecha: str | None = None, etapas: list[str] | None = None) -> dict:
    resumen = {}
    for nombre in etapas or list(ETAPAS):
        logging.getLogger("etl.pipeline").info("== Etapa: %s", nombre)
        resumen[nombre] = ETAPAS[nombre](fecha=fecha, config=cfg)
    return resumen


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description="ETL del Sistema de Predicción de Riesgo Financiero")
    ap.add_argument("--config", help="ruta del YAML (por defecto config/config.yaml)")
    ap.add_argument("--fecha", help="fecha lógica AAAA-MM-DD (por defecto, hoy)")
    ap.add_argument("--etapas", nargs="+", choices=list(ETAPAS), help="subconjunto de etapas")
    ap.add_argument("--demo", action="store_true", help="usa datos sintéticos (no llama a las APIs)")
    ap.add_argument("--sin-macro", action="store_true", help="omite las variables macro (FRED)")
    ap.add_argument("-v", "--verbose", action="store_true")
    a = ap.parse_args(argv)

    logging.basicConfig(level=logging.DEBUG if a.verbose else logging.INFO,
                        format="%(asctime)s %(levelname)s %(name)s: %(message)s")
    cfg = cargar_config(a.config)
    if a.sin_macro:
        cfg.setdefault("macro", {})["habilitado"] = False

    etapas = a.etapas
    if a.demo:
        from .sintetico import preparar_demo
        cfg = preparar_demo(cfg)
        etapas = [e for e in (etapas or list(ETAPAS)) if e != "extraer"]

    try:
        resumen = ejecutar(cfg, a.fecha, etapas)
    except (ErrorCalidad, ErrorApi) as exc:
        logging.getLogger("etl.pipeline").error("%s", exc)
        return 1
    print(json.dumps(resumen, indent=2, ensure_ascii=False, default=str))
    return 0


if __name__ == "__main__":
    sys.exit(main())
