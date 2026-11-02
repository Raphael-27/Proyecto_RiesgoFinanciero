"""Carga de configuración y credenciales (nunca se guardan claves en el repositorio)."""

from __future__ import annotations

import os
from pathlib import Path

import yaml

try:  # opcional: lee un archivo .env si python-dotenv está instalado
    from dotenv import load_dotenv
except ImportError:  # pragma: no cover
    load_dotenv = None

RAIZ = Path(__file__).resolve().parents[1]


class ErrorConfiguracion(Exception):
    """Falta un parámetro o una credencial obligatoria."""


def cargar_config(ruta: str | os.PathLike | None = None) -> dict:
    """Lee el YAML de configuración. Prioridad: argumento > $ETL_CONFIG > config/config.yaml."""
    if load_dotenv is not None:
        load_dotenv(RAIZ / ".env", override=False)
    ruta = Path(ruta or os.environ.get("ETL_CONFIG") or RAIZ / "config" / "config.yaml")
    with open(ruta, encoding="utf-8") as f:
        cfg = yaml.safe_load(f)
    cfg["_raiz"] = str(ruta.resolve().parents[1])
    url = os.environ.get("DATABASE_URL")
    if url:
        cfg.setdefault("carga", {})["base_datos_url"] = url
    return cfg


def directorio_datos(cfg: dict) -> Path:
    base = Path(cfg["proyecto"].get("directorio_datos", "data"))
    if not base.is_absolute():
        base = Path(cfg.get("_raiz", RAIZ)) / base
    base.mkdir(parents=True, exist_ok=True)
    return base


def url_base_datos(cfg: dict) -> str:
    """URL SQLAlchemy; las rutas relativas de SQLite se resuelven contra la raíz del proyecto."""
    url = cfg["carga"]["base_datos_url"]
    prefijo = "sqlite:///"
    if url.startswith(prefijo) and not url.startswith("sqlite:////") and url != prefijo + ":memory:":
        ruta = Path(url[len(prefijo):])
        if not ruta.is_absolute():
            ruta = Path(cfg.get("_raiz", RAIZ)) / ruta
        ruta.parent.mkdir(parents=True, exist_ok=True)
        return prefijo + str(ruta)
    return url


def obtener_secreto(nombre: str) -> str:
    valor = os.environ.get(nombre)
    if not valor:
        raise ErrorConfiguracion(
            f"Falta la variable de entorno {nombre} (defínela en tu entorno, en .env o en el "
            "gestor de secretos; no la escribas en el repositorio)."
        )
    return valor


def activos(cfg: dict) -> list[dict]:
    return list(cfg.get("activos", []))
