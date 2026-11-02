"""Gráficas y exportación de los datos cargados en la base de datos.

    python -m etl.visualizar                      # panel de riesgo por activo + comparación
    python -m etl.visualizar --ticker SPY --abrir # solo SPY y abre la imagen
    python -m etl.visualizar --exportar           # además, CSV para Excel / Power BI
    python -m etl.visualizar --demo               # usa la base de datos de la demostración

Salida en data/graficas/ y data/exportes/. Cada gráfica tiene UN solo eje (sin doble eje),
líneas finas, cuadrícula discreta y etiquetas directas; los colores siguen una paleta
categórica de orden fijo validada contra daltonismo (el mismo activo conserva siempre su color).
"""

from __future__ import annotations

import argparse
import logging
import os
import sys
from pathlib import Path

import numpy as np
import pandas as pd
from sqlalchemy import text

from .carga import crear_motor
from .config import activos, cargar_config, directorio_datos

logger = logging.getLogger("etl.visualizar")

# Paleta categórica (modo claro) en orden fijo; superficie y tintas de texto.
SERIES = ["#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300", "#6250d6", "#e34948"]
SUPERFICIE, TINTA, TINTA_2, TINTA_3 = "#fcfcfb", "#0b0b0b", "#52514e", "#898880"
CUADRICULA, EJE, FRANJA = "#e9e8e4", "#c9c8c3", "#efeeea"

# Episodios de crisis del documento (entrenamiento y backtesting). Fechas aproximadas.
CRISIS = [("Crisis 2008", "2008-09-01", "2009-03-31"), ("COVID-19", "2020-02-19", "2020-04-07")]


def _pyplot():
    try:
        import matplotlib
        matplotlib.use("Agg")  # solo se guardan imágenes: no necesita ventana ni Tk
        import matplotlib.pyplot as plt
        return plt
    except ImportError:
        raise SystemExit("Falta matplotlib. Instálalo con:  pip install matplotlib")


# ------------------------------------------------------------------------------ datos
def leer_caracteristicas(cfg: dict, ticker: str) -> pd.DataFrame:
    from sqlalchemy import inspect

    motor = crear_motor(cfg)
    if not inspect(motor).has_table("caracteristicas"):
        raise SystemExit("La base de datos aún no tiene datos. Ejecuta primero el pipeline:  "
                         "python -m etl.pipeline --sin-macro   (o --demo para datos de prueba)")
    with motor.connect() as con:
        df = pd.read_sql(text("SELECT * FROM caracteristicas WHERE ticker = :t ORDER BY fecha"),
                         con, params={"t": ticker})
    if df.empty:
        raise SystemExit(f"No hay datos de {ticker} en la base de datos. Ejecuta antes el pipeline.")
    df["fecha"] = pd.to_datetime(df["fecha"])
    for c in ("es_atipico", "error_dato", "relleno"):
        if c in df:
            df[c] = df[c].astype(bool)
    return df.set_index("fecha")


def resumen_riesgo(dfs: dict[str, pd.DataFrame]) -> pd.DataFrame:
    """Última lectura de las métricas de riesgo de cada activo."""
    filas = {}
    for t, d in dfs.items():
        ultimo = d.dropna(subset=["var_hist_95"]).iloc[-1] if d["var_hist_95"].notna().any() else d.iloc[-1]
        filas[t] = {
            "fecha": ultimo.name.date(), "VaR 95%": ultimo.get("var_hist_95"),
            "ES 95%": ultimo.get("es_hist_95"), "VaR 99%": ultimo.get("var_hist_99"),
            "Volatilidad anual": ultimo.get("vol_hist_21"),
        }
    return pd.DataFrame(filas).T


# ---------------------------------------------------------------------------- estilo
def _estilo_eje(ax, pct_decimales=1):
    from matplotlib.ticker import PercentFormatter
    ax.set_facecolor(SUPERFICIE)
    for lado in ("top", "right"):
        ax.spines[lado].set_visible(False)
    for lado in ("left", "bottom"):
        ax.spines[lado].set_color(EJE)
    ax.grid(axis="y", color=CUADRICULA, linewidth=0.7)
    ax.set_axisbelow(True)
    ax.tick_params(colors=TINTA_2, labelsize=9, length=3, color=EJE)
    if pct_decimales is not None:
        ax.yaxis.set_major_formatter(PercentFormatter(1.0, decimals=pct_decimales))


def _titulo(ax, titulo, subtitulo=""):
    ax.annotate(titulo, xy=(0, 1), xycoords="axes fraction", xytext=(0, 22 if subtitulo else 8),
                textcoords="offset points", fontsize=11.5, fontweight="bold", color=TINTA, va="bottom")
    if subtitulo:
        ax.annotate(subtitulo, xy=(0, 1), xycoords="axes fraction", xytext=(0, 6),
                    textcoords="offset points", fontsize=8.5, color=TINTA_2, va="bottom")


def _franjas_crisis(ax, rotular=False):
    inicio, fin = ax.get_xlim()
    import matplotlib.dates as mdates
    for nombre, a, b in CRISIS:
        xa, xb = mdates.date2num(pd.Timestamp(a)), mdates.date2num(pd.Timestamp(b))
        if xb < inicio or xa > fin:
            continue
        ax.axvspan(xa, xb, color=FRANJA, zorder=0, linewidth=0)
        if rotular:
            ax.text((xa + xb) / 2, 0.97, nombre, transform=ax.get_xaxis_transform(), ha="center",
                    va="top", fontsize=8, color=TINTA_2,
                    bbox={"boxstyle": "round,pad=0.15", "fc": SUPERFICIE, "ec": "none", "alpha": 0.85})
    ax.set_xlim(inicio, fin)


def _fecha_num(x) -> float:
    import matplotlib.dates as mdates
    return mdates.date2num(pd.Timestamp(x))


def _etiquetas_finales(ax, etiquetas, separacion_pt=11):
    """Rotula el final de cada línea en tinta de texto, separando las que quedan encimadas."""
    ax.figure.canvas.draw()
    inv = ax.transData.inverted()
    puntos = [[t, x, y, ax.transData.transform((_fecha_num(x), y))[1]] for t, x, y in etiquetas if pd.notna(y)]
    puntos.sort(key=lambda p: p[3])
    minimo = separacion_pt * ax.figure.dpi / 72
    for i in range(1, len(puntos)):
        if puntos[i][3] - puntos[i - 1][3] < minimo:
            puntos[i][3] = puntos[i - 1][3] + minimo
    a, b = ax.get_xlim()
    for texto, x, _, ypix in puntos:
        y_texto = inv.transform((0, ypix))[1]
        ax.text(_fecha_num(x) + (b - a) * 0.008, y_texto, texto, fontsize=8.5, color=TINTA_2,
                va="center", ha="left", clip_on=False)


def _ampliar_derecha(ax, fraccion=0.10):
    a, b = ax.get_xlim()
    ax.set_xlim(a, b + (b - a) * fraccion)


def _linea(ax, serie, color, ancho=1.5, etiqueta=None, zorder=3):
    s = serie.dropna()
    if s.empty:
        return None
    ax.plot(s.index, s.values, color=color, linewidth=ancho, label=etiqueta, zorder=zorder,
            solid_capstyle="round")
    return s


def _leyenda(ax, **kw):
    return ax.legend(loc="lower right", bbox_to_anchor=(1.0, 1.0), frameon=False, fontsize=8.5,
                     ncol=kw.get("ncol", 4), labelcolor=TINTA_2, handlelength=1.6, borderaxespad=0.1,
                     columnspacing=1.2)


def _sin_ticks_fuera_de_datos(ax, ultimo):
    """Quita los años del margen derecho (reservado a las etiquetas) para no sugerir datos futuros."""
    limite = _fecha_num(ultimo)
    ax.set_xticks([t for t in ax.get_xticks() if t <= limite])


# ------------------------------------------------------------------- panel de riesgo
def grafico_riesgo(df: pd.DataFrame, ticker: str, ruta: str | os.PathLike) -> Path:
    """Panel de riesgo de un activo: precio, VaR/ES, volatilidad (y VIX si hay datos macro)."""
    plt = _pyplot()
    import matplotlib.dates as mdates

    paneles = ["precio", "var", "vol"] + (["vix"] if "macro_vix" in df and df["macro_vix"].notna().any() else [])
    fig, axes = plt.subplots(len(paneles), 1, figsize=(11, 2.9 * len(paneles) + 0.6), sharex=True,
                             facecolor=SUPERFICIE)
    axes = np.atleast_1d(axes)
    fig.subplots_adjust(left=0.07, right=0.93, top=0.915, bottom=0.05, hspace=0.62)
    fig.suptitle(f"{ticker}: panel de riesgo", x=0.07, y=0.985, ha="left", fontsize=14, fontweight="bold", color=TINTA)

    for ax, tipo in zip(axes, paneles):
        if tipo == "precio":
            _estilo_eje(ax, pct_decimales=None)
            _titulo(ax, "Precio ajustado y medias móviles", "Cierre ajustado por splits y dividendos")
            etq = []
            for col, color, nombre, w in (("adj_close", SERIES[0], "Precio", 1.4),
                                          ("ma_50", SERIES[1], "Media 50 días", 1.4),
                                          ("ma_200", SERIES[2], "Media 200 días", 1.6)):
                if col in df:
                    s = _linea(ax, df[col], color, w, nombre)
                    if s is not None:
                        etq.append((nombre, s.index[-1], s.iloc[-1]))
            if "es_atipico" in df and df["es_atipico"].any():
                at = df[df["es_atipico"]]
                ax.scatter(at.index, at["adj_close"], s=26, color=TINTA, edgecolor=SUPERFICIE,
                           linewidths=1.2, zorder=5, label="Retorno atípico")
            if "error_dato" in df and df["error_dato"].any():
                er = df[df["error_dato"]]
                ax.scatter(er.index, er["adj_close"], s=46, marker="D", facecolor=SUPERFICIE,
                           edgecolor=TINTA, linewidths=1.4, zorder=6, label="Error de dato (corregido)")
            _leyenda(ax, ncol=5) if ax.get_legend_handles_labels()[0] else None
            ax.set_ylabel("")
            ax._etq = etq
        elif tipo == "var":
            _estilo_eje(ax)
            _titulo(ax, "Valor en Riesgo (VaR) y Déficit Esperado (ES)",
                    "Pérdida diaria en % del valor; ES = pérdida media cuando se supera el VaR")
            etq = []
            for col, color, nombre, w in (("var_hist_95", SERIES[0], "VaR 95%", 1.5),
                                          ("es_hist_95", SERIES[1], "ES 95%", 1.5),
                                          ("var_hist_99", SERIES[2], "VaR 99%", 1.5)):
                if col in df:
                    s = _linea(ax, df[col], color, w, nombre)
                    if s is not None:
                        etq.append((nombre, s.index[-1], s.iloc[-1]))
            _leyenda(ax, ncol=3)
            ax._etq = etq
        elif tipo == "vol":
            _estilo_eje(ax, 0)
            _titulo(ax, "Volatilidad histórica anualizada", "Desviación estándar de retornos diarios (21 días), anualizada")
            etq = []
            col = next((c for c in df.columns if c.startswith("vol_hist_")), None)
            if col:
                s = _linea(ax, df[col], SERIES[0], 1.5)
            ax._etq = []
        elif tipo == "vix":
            _estilo_eje(ax, None)
            _titulo(ax, "VIX: volatilidad implícita del S&P 500", "Puntos del índice; con rezago de 1 día para no mirar el futuro")
            _linea(ax, df["macro_vix"], SERIES[0], 1.5)
            ax._etq = []
        ax.xaxis.set_major_locator(mdates.YearLocator(base=max(1, (df.index[-1].year - df.index[0].year) // 10)))
        ax.xaxis.set_major_formatter(mdates.DateFormatter("%Y"))
        ax.margins(x=0)

    # límites exactos (el localizador de años los redondea hacia atrás); ejes compartidos: basta uno
    axes[0].set_xlim(_fecha_num(df.index[0]), _fecha_num(df.index[-1]))
    _ampliar_derecha(axes[0], 0.09)
    for i, ax in enumerate(axes):
        _franjas_crisis(ax, rotular=(i == 0))
    _sin_ticks_fuera_de_datos(axes[-1], df.index[-1])
    fig.canvas.draw()
    for ax in axes:
        if getattr(ax, "_etq", None):
            _etiquetas_finales(ax, ax._etq)
    axes[-1].tick_params(labelbottom=True)

    ruta = Path(ruta)
    ruta.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(ruta, dpi=140, facecolor=SUPERFICIE)
    plt.close(fig)
    return ruta


# -------------------------------------------------------------------- comparación
def grafico_comparacion(dfs: dict[str, pd.DataFrame], ruta: str | os.PathLike) -> Path:
    """Evolución relativa (base 100) y VaR 95 % actual de todos los activos."""
    plt = _pyplot()
    import matplotlib.dates as mdates
    from matplotlib.ticker import PercentFormatter

    tickers = list(dfs)
    if len(tickers) > len(SERIES):
        logger.warning("Se grafican solo los primeros %s activos (límite de colores distinguibles).", len(SERIES))
        tickers = tickers[:len(SERIES)]
    color = {t: SERIES[i] for i, t in enumerate(tickers)}  # el color sigue al activo, no a su posición

    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(12, 5), facecolor=SUPERFICIE,
                                   gridspec_kw={"width_ratios": [2.3, 1]})
    fig.subplots_adjust(left=0.06, right=0.97, top=0.80, bottom=0.1, wspace=0.28)
    fig.suptitle("Comparación de activos", x=0.06, y=0.97, ha="left", fontsize=14, fontweight="bold", color=TINTA)

    # base 100 desde la primera fecha común: todos parten del mismo punto
    comun = max(d.index[0] for d in dfs.values())
    _estilo_eje(ax1, None)
    _titulo(ax1, "Evolución relativa (base 100)", f"Precio ajustado desde {comun.date()}")
    etq = []
    for t in tickers:
        s = dfs[t]["adj_close"].loc[comun:]
        s = s / s.iloc[0] * 100
        ax1.plot(s.index, s.values, color=color[t], linewidth=1.5, label=t, solid_capstyle="round")
        etq.append((t, s.index[-1], s.iloc[-1]))
    ax1.margins(x=0)
    ax1.xaxis.set_major_locator(mdates.YearLocator(base=max(1, (dfs[tickers[0]].index[-1].year - comun.year) // 8)))
    ax1.xaxis.set_major_formatter(mdates.DateFormatter("%Y"))
    ax1.set_xlim(_fecha_num(comun), _fecha_num(max(d.index[-1] for d in dfs.values())))
    _ampliar_derecha(ax1, 0.08)
    _franjas_crisis(ax1, rotular=True)
    _sin_ticks_fuera_de_datos(ax1, max(d.index[-1] for d in dfs.values()))
    _leyenda(ax1, ncol=len(tickers))
    fig.canvas.draw()
    _etiquetas_finales(ax1, etq)

    # barras horizontales ordenadas: una sola tonalidad, valor impreso (pocas barras)
    resumen = resumen_riesgo({t: dfs[t] for t in tickers})["VaR 95%"].astype(float).dropna().sort_values()
    _estilo_eje(ax2, 1)
    ax2.grid(axis="y", visible=False)
    ax2.grid(axis="x", color=CUADRICULA, linewidth=0.7)
    ax2.xaxis.set_major_formatter(PercentFormatter(1.0, decimals=1))
    _titulo(ax2, "VaR 95% actual", "Pérdida diaria no superada el 95% de los días")
    pos = np.arange(len(resumen))
    ax2.barh(pos, resumen.values, height=0.55, color=SERIES[0])
    ax2.set_yticks(pos, labels=list(resumen.index))
    ax2.tick_params(axis="y", length=0, labelsize=9.5, labelcolor=TINTA)
    for p, v in zip(pos, resumen.values):
        ax2.text(v, p, f" {v:.2%}", ha="left", va="center", fontsize=9, color=TINTA)
    ax2.set_xlim(0, resumen.max() * 1.25 if len(resumen) else 1)
    ax2.spines["left"].set_visible(False)

    ruta = Path(ruta)
    ruta.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(ruta, dpi=140, facecolor=SUPERFICIE)
    plt.close(fig)
    return ruta


# ------------------------------------------------------------------------------ CLI
def exportar_csv(df: pd.DataFrame, ticker: str, carpeta: Path) -> Path:
    carpeta.mkdir(parents=True, exist_ok=True)
    ruta = carpeta / f"{ticker}_caracteristicas.csv"
    df.to_csv(ruta, encoding="utf-8-sig")  # utf-8-sig: Excel abre bien los acentos
    return ruta


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description="Gráficas y exportación de los datos del ETL")
    ap.add_argument("--config")
    ap.add_argument("--ticker", help="un solo activo (por defecto todos los de la configuración)")
    ap.add_argument("--demo", action="store_true", help="usa la base de datos de data/demo")
    ap.add_argument("--exportar", action="store_true", help="exporta también un CSV por activo")
    ap.add_argument("--abrir", action="store_true", help="abre las imágenes al terminar (Windows)")
    a = ap.parse_args(argv)

    logging.basicConfig(level=logging.INFO, format="%(levelname)s: %(message)s")
    cfg = cargar_config(a.config)
    if a.demo:
        cfg["proyecto"]["directorio_datos"] = "data/demo"
        cfg["carga"]["base_datos_url"] = "sqlite:///data/demo/riesgo.db"
    base = directorio_datos(cfg)

    tickers = [a.ticker] if a.ticker else [x["ticker"] for x in activos(cfg)]
    dfs = {t: leer_caracteristicas(cfg, t) for t in tickers}
    generados = []
    for t, d in dfs.items():
        generados.append(grafico_riesgo(d, t, base / "graficas" / f"riesgo_{t}.png"))
        if a.exportar:
            generados.append(exportar_csv(d, t, base / "exportes"))
    if len(dfs) > 1:
        generados.append(grafico_comparacion(dfs, base / "graficas" / "comparacion.png"))

    print("\nÚltima lectura de riesgo (pérdidas diarias en fracción; 0.02 = 2 %):")
    print(resumen_riesgo(dfs).to_string())
    print("\nArchivos generados:")
    for g in generados:
        print("  ", g)
    if a.abrir and sys.platform == "win32":
        for g in generados:
            if str(g).endswith(".png"):
                os.startfile(g)  # noqa: S606
    return 0


if __name__ == "__main__":
    sys.exit(main())
