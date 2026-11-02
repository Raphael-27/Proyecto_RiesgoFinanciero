import copy

import pytest

from etl.config import cargar_config


@pytest.fixture
def cfg(tmp_path):
    """Configuración real del proyecto, pero con datos y base de datos en un directorio temporal."""
    c = copy.deepcopy(cargar_config())
    c["proyecto"]["directorio_datos"] = str(tmp_path / "data")
    c["carga"]["base_datos_url"] = f"sqlite:///{tmp_path / 'test.db'}"
    c["extraccion"].update(espera_base_segundos=0, espera_maxima_segundos=0)
    for f in c["fuentes"].values():
        for k in ("pausa_entre_llamadas_segundos", "pausa_entre_activos_segundos"):
            if k in f:
                f[k] = 0
    return c


class RespuestaFalsa:
    def __init__(self, datos=None, status=200, texto=""):
        self._datos, self.status_code, self.text = datos, status, texto

    def json(self):
        if self._datos is None:
            raise ValueError("no json")
        return self._datos


class SesionFalsa:
    """Sesión HTTP que devuelve una cola de respuestas y registra las llamadas."""

    def __init__(self, *respuestas):
        self.respuestas, self.llamadas = list(respuestas), []

    def get(self, url, params=None, timeout=None):
        self.llamadas.append((url, dict(params or {})))
        r = self.respuestas.pop(0) if len(self.respuestas) > 1 else self.respuestas[0]
        if isinstance(r, Exception):
            raise r
        return r
