"""DAG de Airflow del ETL de riesgo financiero.

    extraer >> limpiar >> validar_calidad >> transformar >> cargar

Correcciones respecto al Código 2 del documento:
  * `etl.tareas` ahora existe (el original importaba funciones que no estaban implementadas).
  * Se agrega la tarea `validar_calidad`: si una métrica bloqueante falla, la tarea falla y
    `cargar` no se ejecuta (el documento exige detener el flujo antes de la carga).
  * Reintentos con espera CRECIENTE (retry_exponential_backoff), como pide el documento.
  * La hora se interpreta en la zona del mercado (el cron original se evaluaba en UTC) y se
    adelanta de 22:00 a 17:30 ET para cumplir la oportunidad ≤ 2 h tras el cierre (Tabla 3).
  * `max_active_runs=1` evita que dos ejecuciones escriban a la vez en la misma capa.
  * Compatible con Airflow 2.4+ y 3.x.
"""

from __future__ import annotations

import logging
from datetime import timedelta

import pendulum

try:  # Airflow 3.x
    from airflow.sdk import DAG
    from airflow.providers.standard.operators.python import PythonOperator
except ImportError:  # Airflow 2.4+
    from airflow import DAG
    from airflow.operators.python import PythonOperator

from etl.config import cargar_config
from etl.tareas import (cargar_datos, extraer_datos, limpiar_datos, transformar_datos,
                        validar_calidad)

logger = logging.getLogger("etl.dag")
_cfg = cargar_config()
_orq = _cfg.get("orquestacion", {})
_zona = _orq.get("zona_horaria", "America/New_York")
_correos = _orq.get("correo_alertas") or []


def _avisar_fallo(contexto):
    """Hook de notificación. Sustituir por Slack/Teams/PagerDuty si se desea."""
    ti = contexto.get("task_instance")
    logger.error("ETL riesgo financiero: falló la tarea %s (intento %s). %s",
                 getattr(ti, "task_id", "?"), getattr(ti, "try_number", "?"), contexto.get("exception"))


with DAG(
    dag_id="etl_riesgo_financiero",
    description="Extrae, limpia, valida, transforma y carga series financieras y macro.",
    start_date=pendulum.datetime(2026, 1, 1, tz=_zona),
    schedule=_orq.get("cron", "30 17 * * 1-5"),   # días hábiles, tras el cierre del mercado
    catchup=False,
    max_active_runs=1,
    tags=["riesgo", "etl"],
    default_args={
        "retries": 3,
        "retry_delay": timedelta(minutes=5),
        "retry_exponential_backoff": True,          # 5, 10, 20 min...
        "max_retry_delay": timedelta(minutes=30),
        "on_failure_callback": _avisar_fallo,
        "email": _correos,
        "email_on_failure": bool(_correos),
    },
) as dag:
    # data_interval_end = fecha de la corrida (ds sería la corrida anterior en cron clásico)
    kwargs = {"fecha": "{{ data_interval_end | ds }}"}

    extraer = PythonOperator(task_id="extraer", python_callable=extraer_datos, op_kwargs=kwargs)
    limpiar = PythonOperator(task_id="limpiar", python_callable=limpiar_datos, op_kwargs=kwargs)
    validar = PythonOperator(task_id="validar_calidad", python_callable=validar_calidad, op_kwargs=kwargs,
                             retries=0)  # un dato malo no se arregla reintentando
    transformar = PythonOperator(task_id="transformar", python_callable=transformar_datos, op_kwargs=kwargs)
    cargar = PythonOperator(task_id="cargar", python_callable=cargar_datos, op_kwargs=kwargs)

    extraer >> limpiar >> validar >> transformar >> cargar
