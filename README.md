# Sistema de Predicción de Riesgo Financiero — ETL

Implementación ejecutable de la etapa de **Extracción, Limpieza, Calidad, Transformación y Carga**
descrita en el documento del proyecto (Códigos 1 y 2, Tablas 1–6).

```
APIs (Yahoo / Alpha Vantage / FRED)
   └─ extraer ─► raw ─► limpiar ─► clean ─► validar_calidad ─► transformar ─► curated ─► cargar ─► BD
                (intacto)  (reglas Tabla 2)   (Tabla 3: detiene el flujo)   (VaR, ES, ...)       (idempotente)
```

## Inicio rápido

```bash
pip install -r requirements.txt
python -m etl.pipeline --demo        # prueba completa con datos sintéticos (sin APIs ni claves)
python -m pytest                     # 53 pruebas

cp .env.example .env                 # completa FRED_API_KEY (y ALPHAVANTAGE_API_KEY si la usas)
python -m etl.pipeline               # ejecución real con config/config.yaml
python -m etl.pipeline --sin-macro   # solo precios de Yahoo: no necesita ninguna clave
python -m etl.pipeline --etapas limpiar validar   # re-procesar sin volver a consultar las APIs
```

Salida: `data/raw|clean|curated|reportes` (Parquet/JSON) y `data/riesgo.db` (SQLite). Para
PostgreSQL: `DATABASE_URL=postgresql+psycopg2://usuario:clave@host:5432/riesgo` (y `pip install psycopg2-binary`).

## Visualizar y exportar los datos

```bash
python -m etl.visualizar                 # data/graficas/riesgo_<TICKER>.png y comparacion.png
python -m etl.visualizar --ticker SPY --abrir
python -m etl.visualizar --exportar      # además CSV por activo en data/exportes/ (Excel, Power BI)
python -m etl.visualizar --demo          # sobre la base de la demostración
```
Otras formas de consultar los datos: abrir `data/riesgo.db` con *DB Browser for SQLite* o conectar
Power BI / Excel / Tableau a esa base (o al PostgreSQL de producción); las tablas son
`precios_limpios`, `caracteristicas` (retornos, VaR, ES, volatilidad y macro) y `reportes_calidad`.

## Estructura

| Archivo | Qué hace | Sección del documento |
|---|---|---|
| `config/config.yaml` | Activos, parámetros de limpieza, umbrales, rezagos, cron | "Configuración externa" |
| `etl/extraccion.py` | Clientes Yahoo, Alpha Vantage, FRED con reintentos y ocultamiento de claves | Extracción, Origen de datos |
| `etl/limpieza.py` | Reglas de la Tabla 2 + `alinear_macro` | Código 1 |
| `etl/calidad.py` | Perfilado inicial y validaciones de la Tabla 3 | Perfilado, Control de calidad |
| `etl/transformacion.py` | Retornos log, medias móviles, volatilidad, VaR y ES, z-scores | Transformación |
| `etl/carga.py` | Carga idempotente a SQL (SQLite/PostgreSQL) | Carga |
| `etl/almacen.py` | Capas raw / clean / curated (Tabla 1) | Arquitectura por capas |
| `etl/tareas.py`, `etl/dag.py`, `etl/pipeline.py` | Tareas, DAG de Airflow y CLI | Código 2 |
| `etl/visualizar.py` | Gráficas de riesgo y exportación a CSV | Dashboard (primer paso) |
| `etl/sintetico.py` | Reproduce la prueba de la Tabla 4 | Validación con datos sintéticos |

## Qué se corrigió respecto al código del documento

**Fallos de funcionamiento**
- `dag.py` importaba `etl.tareas` con funciones que no existían → ahora están implementadas.
- `sort_index()` no es estable: "conservar el último registro recibido" podía conservar el equivocado. Ahora `kind="stable"` (hay prueba).
- Con pandas ≥ 3, `merge_asof` falla si las fechas tienen resoluciones distintas (s/ms/us/ns) → se fuerzan a `datetime64[ns]`.
- Con zonas horarias mezcladas, pandas 3 convierte en `NaT` las fechas de formato distinto **sin avisar**, y se perdían filas → se detecta y se reanaliza fecha por fecha.
- `validar_coherencia_ohlc` fallaba con `KeyError` si faltaban `high`/`low`.

**Errores metodológicos**
- El z-score robusto incluía el dato evaluado en su propia ventana y usaba `close`; ahora la ventana es solo pasado, la MAD es exacta por ventana y los retornos usan `adj_close` (splits/dividendos).
- Conversión a UTC antes de normalizar movía un día las bolsas asiáticas → se usa la fecha local del mercado.
- `alinear_macro` aplicaba un único rezago fijo → ahora admite rezago por serie y **fecha de publicación real** (modo `vintage` con FRED/ALFRED), usando siempre la primera versión publicada.
- **Falso positivo detectado con datos reales de Yahoo:** el criterio del documento (extremo + reversión) clasificaba como error de captura el 12-mar-2020 del S&P 500 (-9,5 % y rebote +8,5 %) y lo reemplazaba por el precio anterior. Ahora un error exige además un salto desproporcionado (|retorno log| ≥ 0,405) y que no haya volumen disparado (`salto_minimo_error`, `factor_volumen_real` en la configuración).
- Rellenar huecos sin calendario crea retornos artificiales en feriados → solo se completan días de negociación esperados (calendario real, p. ej. NYSE).

**Reglas descritas en el documento y no implementadas** (ahora sí): días no hábiles/feriados, columnas `ticker`/`fuente`/`relleno`, perfilado, las 6 métricas de calidad que detienen el flujo antes de la carga, configuración externa, reintentos con espera creciente, idempotencia, capas raw/clean/curated, reporte por ejecución.

**Inconsistencia del documento**: el cron de las 22:00 deja 6 h de retraso con cierre a las 16:00 ET, lo que incumple la métrica de oportunidad (≤ 2 h). Se programó a las 17:30 ET (configurable en `orquestacion.cron`).

## Limitaciones y puntos que debes revisar

- **Las APIs reales no se probaron en vivo**: el entorno de desarrollo bloqueaba el acceso a Yahoo, FRED y Alpha Vantage. Los clientes se probaron con respuestas simuladas siguiendo el formato documentado de cada API. Haz una primera corrida con `--sin-macro` y revisa `data/reportes/`.
- Yahoo no publica límites de uso y puede responder 429; hay reintentos, pero para producción considera un proveedor con contrato. Alpha Vantage gratuito **no ajusta** por splits/dividendos (`adj_close = close`, queda registrado en el reporte) y su límite diario es muy bajo; verifícalo en tu plan y ajusta `pausa_entre_llamadas_segundos`.
- **Los rezagos macro (`rezago_dias`) son aproximaciones conservadoras.** Para exactitud usa `macro.modo: vintage`. Los datos anteriores a la cobertura de ALFRED toman como fecha de publicación el inicio de esa cobertura.
- No incluidos: Bloomberg (licencia), Banco de la República (no hay un endpoint público verificado; FRED trae `DEXCOUS` para COP/USD), InfluxDB, datos alternativos (noticias/sentimiento), y las etapas de modelado, dashboard y backtesting.
- `evaluar_oportunidad` está desactivada por defecto (sin sentido en reprocesos o backtests); actívala en producción.
- El DAG se validó con un Airflow simulado (orden, reintentos, zona horaria), no en una instalación real de Airflow.
- Un error de captura de **dos o más días consecutivos** no se clasifica como error (solo se marca como atípico), porque no se "revierte" al día siguiente. Es un límite del criterio del documento.
- La extracción baja el historial completo en cada corrida (necesario para la ventana del z-score y para reflejar ajustes retroactivos de dividendos).
