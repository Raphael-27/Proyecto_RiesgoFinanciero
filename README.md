# Sistema de Predicción de Riesgo Financiero — ETL en R

Implementación ejecutable, **solo en R**, de la etapa de Extracción, Limpieza, Calidad,
Transformación y Carga descrita en el documento del proyecto (Códigos 1 y 2, Tablas 1–6).

```
APIs (Yahoo / Alpha Vantage / FRED)
  └─ extraer ─► raw ─► limpiar ─► clean ─► validar_calidad ─► transformar ─► curated ─► modelar ─► backtest ─► cargar ─► BD
               (intacto)  (Tabla 2)        (Tabla 3: detiene el flujo)   (VaR, ES, ...)   (ML)      (crisis)    (idempotente)
```

## Inicio rápido (Windows)

1. Instala **R** desde <https://cran.r-project.org> (y, si quieres, RStudio). Con R reciente (4.1 o superior) basta.
2. Abre el CMD en la carpeta del proyecto. Si `Rscript` no se reconoce, usa la ruta completa, por ejemplo
   `"C:\Program Files\R\R-4.4.2\bin\Rscript.exe"` en lugar de `Rscript`.

```bat
Rscript instalar_paquetes.R          :: una sola vez (tarda unos minutos)
Rscript tests/testthat.R             :: 87 pruebas, deben pasar todas
Rscript pipeline.R --demo            :: flujo completo con datos sintéticos (sin APIs ni claves)
Rscript visualizar.R --demo --abrir  :: gráficas de la demostración
Rscript ver_datos.R --demo           :: primeras filas de cada tabla

Rscript pipeline.R --sin-macro       :: datos REALES de Yahoo (no necesita ninguna clave)
Rscript visualizar.R --exportar      :: gráficas en data\graficas y CSV en data\exportes
```

Para las variables macro (VIX, tasas, tipo de cambio, inflación) pide una clave gratuita en
fred.stlouisfed.org, ejecuta `copy .env.example .env`, pega la clave en `FRED_API_KEY=` y ejecuta
`Rscript pipeline.R`. **En RStudio** (sin línea de comandos): `source("R/00_cargar.R")` y luego
`main_pipeline("--demo")`, `main_pipeline("--sin-macro")` o `main_visualizar("--demo")`.

Para re-procesar sin volver a descargar: `Rscript pipeline.R --etapas limpiar validar transformar cargar`.
Para ejecutarlo todos los días hábiles tras el cierre: `programar_windows.bat` (Programador de tareas;
en R no se usa Airflow, que es una herramienta de Python).

## Dónde viven los datos (importante al actualizar el código)

Por defecto los datos se guardan en `data\` dentro del proyecto, así que **borrar o reemplazar la carpeta del proyecto los pierde**.
Para evitarlo, define una carpeta fuera del proyecto en el archivo `.env` (que no se sobrescribe al actualizar):

```bat
echo ETL_DATOS=C:\Users\TU_USUARIO\datos_riesgo> .env
```
Todo (capas, base de datos, modelos, backtesting y gráficas) se guarda entonces ahí. Para actualizar el código, descomprime el zip nuevo
encima de la carpeta del proyecto (sin borrarla).

## Estructura

| Archivo | Qué hace | Sección del documento |
|---|---|---|
| `config/config.yaml` | Activos, parámetros de limpieza, umbrales, rezagos | "Configuración externa" |
| `R/extraccion.R` | Clientes Yahoo (quantmod o API `chart`), Alpha Vantage y FRED, con reintentos y ocultamiento de claves | Extracción |
| `R/limpieza.R` | Reglas de la Tabla 2 y `alinear_macro` | Código 1 |
| `R/calidad.R` | Perfilado inicial y validaciones de la Tabla 3 | Perfilado, Control de calidad |
| `R/transformacion.R` | Retornos log, medias móviles, volatilidad, VaR y ES, z-scores | Transformación |
| `R/modelado.R` | Modelos de aprendizaje automático: pronóstico de alta volatilidad y caídas abruptas | Modelado predictivo |
| `R/backtesting.R` | Backtesting del VaR y de los modelos en las crisis de 2008 y 2020 | Evaluación de desempeño |
| `R/carga.R` | Carga idempotente a SQLite (PostgreSQL opcional) | Carga |
| `R/almacen.R` | Capas raw / clean / curated (Tabla 1) en CSV | Arquitectura por capas |
| `R/tareas.R`, `pipeline.R` | Etapas del flujo y línea de comandos | Código 2 |
| `R/visualizar.R`, `visualizar.R` | Gráficas (ggplot2) y exportación a CSV | Dashboard (primer paso) |
| `R/sintetico.R` | Reproduce la prueba de la Tabla 4 | Validación con datos sintéticos |
| `tests/testthat/` | 87 pruebas automáticas | — |

Las tablas de la base de datos son `precios_limpios`, `caracteristicas` (retornos, VaR, ES,
volatilidad y variables macro) y `reportes_calidad`. Se pueden abrir con *DB Browser for SQLite*,
o conectar Excel / Power BI a `data/riesgo.db` o a los CSV de `data/exportes`.

## Modelado predictivo (etapa `modelar`)

```bat
Rscript pipeline.R --etapas modelar cargar     :: entrena y valida los modelos (tarda unos minutos)
Rscript ver_modelos.R                          :: desempeño de cada modelo y señal de hoy
```

**Qué pronostica** (a 5 días): (1) *alta volatilidad*: la volatilidad realizada futura supera el percentil 90 de su
propio pasado; (2) *caída abrupta*: retorno acumulado ≤ −5 %. **Con qué**: regresión logística regularizada
(elastic net, `glmnet`) y bosque aleatorio (`ranger`), sobre retornos, medias móviles, volatilidad, VaR, Expected
Shortfall, caída desde máximos, volumen y las variables macro si están activas. No se incluyeron redes
recurrentes: en R dependen de TensorFlow/Python.

**Cómo se evita hacer trampa** (lo más importante): las etiquetas usan el futuro y nunca son variables de entrada;
la validación es *walk-forward* (se entrena con el pasado y se prueba en el bloque siguiente, reentrenando cada
año); se descartan 10 días de entrenamiento junto a cada bloque de prueba (*purga*) porque sus etiquetas se solapan;
la imputación, la estandarización, los hiperparámetros y el umbral de alerta se calculan solo con datos de
entrenamiento. Hay pruebas automáticas de cada una de estas garantías.

**Cómo se mide**: AUC, Brier y *habilidad de Brier* (1 − Brier/Brier de repetir la frecuencia histórica), siempre
fuera de muestra y contra una **línea base** sencilla (persistencia de la volatilidad; VaR pasado). Un modelo se
marca `recomendado` solo si su habilidad de Brier es positiva; si ninguno lo logra, ninguno se recomienda. El umbral
de alerta maximiza F1 pero nunca alerta más del 25 % de los días, y un modelo sin información no alerta.

**Lo que se sabe y lo que no (leer antes de afirmar algo en la exposición)**
- Con **series simuladas** se comprobó que el modelo detecta señal cuando existe (agrupamiento de volatilidad: AUC ≈ 0,70
  y habilidad de Brier positiva) y que **no inventa señal** en un paseo aleatorio (AUC ≈ 0,5, habilidad ≈ 0).
- En esa simulación la **línea base de persistencia (AUC 0,757) iguala al máximo teórico (0,756)**, por lo que ningún modelo
  podía superarla: la regresión logística (≈ 0,70) queda algo por debajo por el costo de estimar muchos parámetros, y el
  bosque rinde peor (con señales débiles los árboles ordenan casi al azar en la zona plana). Es un resultado esperable.
- **Con datos reales todavía no se han medido.** Las cifras reales las dará `ver_modelos.R`. Las caídas abruptas
  (≤ −5 % en 5 días) son eventos raros y difíciles de pronosticar: es normal que el AUC sea modesto y que a veces no haya
  ningún modelo recomendado. No se debe prometer "alta precisión" sin mirar esa tabla.
- Los hiperparámetros del bosque se eligen en el primer bloque y se reutilizan (por velocidad); la regresión logística
  reelige regularización en cada bloque.

## Backtesting (etapa `backtest`)

```bat
Rscript pipeline.R --etapas backtest cargar    :: requiere haber ejecutado antes la etapa modelar
Rscript ver_backtest.R                         :: tablas de resultados
```
Gráficas en `data\graficas\backtest_var_<ACTIVO>.png` y `backtest_crisis_<ACTIVO>.png`.

1. **VaR.** Se comprueba si el VaR 95 % y 99 % se supera con la frecuencia esperada (**Kupiec**), si las superaciones se
   agrupan en el tiempo (**Christoffersen**), y el **semáforo de Basilea** (verde 0–4, amarillo 5–9, rojo ≥ 10 violaciones en 250
   días para el VaR 99 %); en toda la muestra y dentro de cada crisis. El VaR de un día se compara con el retorno del día siguiente.
2. **Modelos en las crisis.** Para cada crisis (2008, COVID-19; configurables en `backtesting.crisis`) el modelo se entrena **solo con
   datos anteriores** (menos 10 días de purga), predice durante la crisis y se mide: AUC, habilidad de Brier contra la frecuencia
   aprendida antes de la crisis, precisión, exhaustividad, **fecha de la primera alerta** y días de anticipación respecto al peor
   día. Siempre se compara con la regla base.
3. **Incertidumbre.** Intervalos de confianza al 95 % por *bootstrap por bloques* del AUC, de su diferencia con la regla base y de la
   habilidad de Brier. Sirve para saber si una mejora es real o podría ser ruido: las etiquetas a 5 días se solapan, así que las
   observaciones no son independientes y los intervalos ingenuos serían demasiado optimistas.

**Cómo leer los resultados.** Es esperable que el VaR histórico *falle en las crisis* (se calcula con un pasado tranquilo y reacciona tarde):
mostrarlo es parte del resultado, no un error. Las pruebas en una ventana corta (p. ej. COVID: ~95 días) tienen poca potencia: mira siempre
la columna `dias`. Un modelo solo "supera a la base" si el intervalo de la diferencia de AUC queda por encima de cero.

## Decisiones metodológicas (idénticas en la versión Python)

- **Duplicados:** se conserva el último registro recibido (el orden de llegada se respeta).
- **Fechas:** con zona horaria se toma la fecha *local del mercado*, no la de UTC (evita correr un día a las bolsas asiáticas).
- **Atípicos:** z-score robusto (mediana y MAD) sobre retornos de `adj_close`, con ventana *solo pasada*. Un valor extremo se **corrige** como error solo si (1) se revierte al día siguiente, (2) el salto es desproporcionado (≥ ~50 %) y (3) no hay volumen disparado. Así el rebote real del 12–13 de marzo de 2020 se conserva como atípico. Esto se descubrió probando con datos reales del S&P 500, donde el criterio original del documento lo corregía por error.
- **Feriados:** solo se completan días de negociación esperados (calendario NYSE, con los cierres de duelo nacional de 2018 y 2025 añadidos a mano); los feriados reales no se rellenan, para no crear retornos artificiales de cero.
- **Macro:** un dato solo se usa desde su fecha de disponibilidad (observación + rezago, o fecha real de publicación en modo `vintage`); se conserva la primera versión publicada. Evita el *look-ahead bias*.
- **Oportunidad:** el documento programaba el flujo a las 22:00, lo que deja 6 h de retraso con cierre a las 16:00 ET y contradice el umbral de ≤ 2 h (Tabla 3). Se programa ~1 h 30 min después del cierre.

## Limitaciones y puntos que debes revisar

- **Las APIs reales no se probaron en vivo**: el entorno de desarrollo bloqueaba Yahoo, FRED y Alpha Vantage. Los clientes se probaron con respuestas simuladas siguiendo el formato documentado de cada API. Haz una primera corrida con `--sin-macro` y revisa `data/reportes/`.
- **Yahoo:** el motor por defecto usa el paquete `quantmod`, que sigue los cambios de Yahoo, pero Yahoo no ofrece una API oficial y puede limitar solicitudes (error 429). Si `quantmod` falla, cambia `fuentes.yahoo.motor` a `chart` en `config/config.yaml`.
- **Alpha Vantage** gratuito no ajusta por splits/dividendos (`adj_close = close`, queda registrado en el reporte) y su límite diario es bajo; verifica tu plan.
- **Los rezagos macro** (`rezago_dias`) son aproximaciones conservadoras. Para mayor exactitud usa `macro.modo: vintage`.
- **PostgreSQL** (paquete `RPostgres`) está implementado pero **no se probó**; solo SQLite se probó.
- **Calendarios:** NYSE/NASDAQ, Londres, Toronto y 24/7. El paquete `timeDate` puede desconocer cierres extraordinarios recientes; agrégalos en `calendarios.feriados_extra` del YAML.
- No incluidos: Bloomberg (licencia), Banco de la República (no hay un endpoint público verificado; FRED trae `DEXCOUS` para COP/USD), InfluxDB, datos alternativos (noticias/sentimiento), redes neuronales recurrentes, y el dashboard interactivo con alertas por umbrales (en desarrollo).
- Un error de captura de **dos o más días consecutivos** no se clasifica como error (solo se marca como atípico), porque no se "revierte" al día siguiente.
- Se probó con R 4.3.3 en Linux (con locale UTF-8 y con locale "C"). En Windows la primera ejecución real es la verificación pendiente.
- **YAML en R:** escribe las horas entre comillas (`"16:00"`) y no uses claves llamadas `y`, `n`, `yes`, `no` (se leen como verdadero/falso).
