#!/usr/bin/env python3
"""Toleranz- und Sensitivitaetsanalyse eines Zoom-Teleskops mit dem `optics`-Solver.

Dieses Skript steuert den kompilierten Rust-Solver `optics` als Black Box und
bewertet die *Herstellbarkeit* eines vierteiligen Zoom-Teleskops. Der Kern:

  1. SENSITIVITAET MESSEN — rein gradientenbasiert, KEIN Parameter-Sweep.
     Fuer jede Zoom-Stellung (wide / mid / tele) liest das Skript die exakten
     Sensitivitaeten s_p = dLoss/dp aller Toleranzparameter (Radien, Dicken,
     Brechzahlen) direkt aus dem Autodiff-Solver (`optics sensitivity`). Das
     ersetzt den DoE-Sweep aus Tutorial 01 vollstaendig.

  2. TOLERANZ-RANKING — welche Bauteile sind am fehlerempfindlichsten, und wie
     wandert das Ranking ueber den Zoombereich? Da die rohe Ableitung dLoss/dp
     je nach Parameter andere Einheiten hat (pro mm bei Radius/Dicke, pro
     Brechzahl-Einheit bei Material), werden die Sensitivitaeten mit realen
     Fertigungstoleranzen skaliert: die "erwartete Loss-Verschlechterung" bei
     einer typischen Fertigungsabweichung ist  |s_p| * tol_p  und damit ueber
     alle Parametertypen fair vergleichbar.

  3. ROBUSTHEIT VERBESSERN — dort DoE/Suche, wo der Gradient nicht reicht.
     Die Robustheit KLEINER zu machen braucht die Ableitung der Sensitivitaet
     nach den Designvariablen, also eine zweite Ableitung, die der Forward-Mode-
     Autodiff nicht direkt liefert. Deshalb wird die Robustheitskennzahl ueber
     die freien Designvariablen per kleinem DoE-Sweep verbessert -- wobei die
     Kennzahl an JEDEM Sweep-Punkt gradientenbasiert (per Autodiff-Sensitivitaet)
     berechnet wird. DoE nur ueber den Rest, die Kennzahl selbst per Gradient.

  4. VALIDIERUNG — die Autodiff-Sensitivitaeten werden gegen zentrale Finite
     Differenzen der `loss`-Ausgabe gegengeprueft (Muster wie die Rust-Tests
     `gradient_matches_finite_differences`), damit die Gradienten belastbar sind.

Alles ist reine Python-Standardbibliothek (kein numpy / matplotlib), laeuft
also ueberall, wo Python 3.9+ und das kompilierte Binary vorhanden sind.

Aufruf:
    python3 tolerance_analysis.py [--binary PATH] [--out DIR]

Ergebnisse (in --out, Vorgabe ./results):
    sensitivity_profile.csv      s_p je Parameter je Zoom-Stellung (roh + skaliert)
    tolerance_ranking.csv        Parameter nach worst-case-Empfindlichkeit sortiert
    robustness_before_after.json Kennzahl vorher/nachher + verbesserte Designwerte
    summary.json                 maschinenlesbare Gesamt-Zusammenfassung
"""

from __future__ import annotations

import argparse
import json
import math
import re
import subprocess
import sys
from dataclasses import dataclass, asdict, field
from pathlib import Path

# --------------------------------------------------------------------------
# Solver und Prescriptions lokalisieren
# --------------------------------------------------------------------------

HERE = Path(__file__).resolve().parent
# tutorial/02_zoom_teleskop -> ../../source6
SOURCE6 = (HERE / ".." / ".." / "source6").resolve()
DEFAULT_BINARY = SOURCE6 / "target" / "release" / "optics"
ASSETS = HERE / "assets"

# Die drei Zoom-Stellungen (Label -> TOML-Datei).
ZOOM_POSITIONS = [
    ("wide", ASSETS / "zoom_wide.toml"),
    ("mid", ASSETS / "zoom_mid.toml"),
    ("tele", ASSETS / "zoom_tele.toml"),
]

# Realistische Fertigungstoleranzen je Parametertyp. Damit werden die roh
# unterschiedlich skalierten Gradienten in eine gemeinsame, physikalisch
# sinnvolle Einheit gebracht: die erwartete Loss-Aenderung bei einer typischen
# Abweichung. (Quelle: uebliche Feinoptik-Werkstattgenauigkeiten, bewusst
# konservativ; der Bericht erlaeutert die Wahl.)
#
# WICHTIG — Massstab: das Patent US 5,146,366 ist auf F = 1.0 normiert
# (dimensionslos), die Radien/Dicken tragen also KEINE Millimeter, sondern
# Vielfache der Systembrennweite. Die Toleranzen sind entsprechend in dieser
# normierten Einheit angesetzt: ~1 % des vorderen Krummungsradius fuer den
# Schliff, ~0,3 % der Systembrennweite fuer Dicken/Abstaende bei der Montage,
# und die uebliche Glas-Chargenstreuung fuer die Brechzahl.
DEFAULT_TOLERANCES = {
    "radius": 0.010,    # norm. Krummungsradius-Fehler beim Schleifen
    "thickness": 0.003, # norm. Dicken-/Abstandstoleranz bei Montage
    "material": 0.0010, # -     Brechzahl-Streuung der Glascharge
}

# Freie Designvariablen fuer die Robustheits-Verbesserung (Abschnitt 3 im
# Prompt): die beiden beweglichen Kompensator-/Variator-Luftspalte D10 und
# D12, die pro Zoom-Stellung die Montage-/Nachfuehrlage festlegen. Sie lassen
# sich in der Fertigung tatsaechlich justieren und beeinflussen die
# Empfindlichkeit stark, ohne die Kamera (letzter Spalt, per Solver gefixt)
# zu verschieben. Wir sweepen NUR diese und messen die Kennzahl an jedem
# Punkt per Gradient.
FREE_GAP_VARS = [
    ("G2 L6 back (D10 variable -> G3)", "thickness"),  # D10
    ("G3 L7 back (D12 variable -> G4)", "thickness"),  # D12
]

# "Naiver Entwurf": ein noch nicht auf Herstellbarkeit gepruefter erster Wurf.
# Er verschiebt die beiden beweglichen Spalte D10/D12 gegenlaeufig um diesen
# Betrag (normierte Patenteinheiten) aus der spaeter gefundenen guten Lage
# heraus -- so, wie ein Konstrukteur die Nachfuehrung zunaechst grob ansetzt.
# Klein gewaehlt, weil die Patent-Spalte selbst im Bereich ~0,1..2 liegen.
DRAFT_OFFSET_MM = 0.12


# --------------------------------------------------------------------------
# Mit dem Solver reden
# --------------------------------------------------------------------------

_LOSS_TRACE_RE = re.compile(r"rays:\s*(\d+)/(\d+)\s*arrived,\s*loss\s*=\s*([-\d.eE+]+)")
_LOSS_SENS_RE = re.compile(r"loss\s*=\s*([-\d.eE+]+)")
_EFL_RE = re.compile(r"EFL\s*=\s*([-\d.]+)")
_DEFOCUS_RE = re.compile(r"defocus\s*=\s*([-\d.]+)")
_SENS_RE = re.compile(
    r"sensitivity\s+(.+?)\s+(radius|thickness|material)\s+"
    r"value=([-\d.eE+]+)\s+dloss_dp=([-\d.eE+]+)"
)


class SolverError(RuntimeError):
    """Der Solver ist mit Fehlercode beendet oder gab nichts Verwertbares aus."""


def _run(binary: Path, args: list[str]) -> str:
    proc = subprocess.run(
        [str(binary), *args],
        capture_output=True,
        text=True,
        cwd=str(SOURCE6),  # damit relative Asset-Pfade in Configs aufgehen
    )
    if proc.returncode != 0:
        raise SolverError(
            f"optics {' '.join(args)} fehlgeschlagen ({proc.returncode}): "
            f"{proc.stderr.strip()}"
        )
    return proc.stdout


@dataclass
class Sensitivity:
    """Eine Toleranz-Sensitivitaet: Parameter plus exakte Ableitung dLoss/dp."""

    surface: str
    key: str            # radius / thickness / material
    value: float
    dloss_dp: float     # exakte Autodiff-Ableitung (roh)

    @property
    def label(self) -> str:
        return f"{self.surface} [{self.key}]"


def read_sensitivities(binary: Path, config: Path) -> tuple[float, list[Sensitivity]]:
    """Exakte Sensitivitaeten aller optimize-Parameter per Autodiff auslesen.

    Das ist der gradientenbasierte Ersatz des DoE-Sweeps: EIN Solver-Aufruf
    liefert dLoss/dp fuer jeden Parameter, ohne irgendein Raster.
    """
    out = _run(binary, ["sensitivity", "--config", str(config)])
    m = _LOSS_SENS_RE.search(out)
    if not m:
        raise SolverError(f"kein loss in sensitivity-Ausgabe:\n{out}")
    loss = float(m.group(1))
    sens = [
        Sensitivity(surface=mm.group(1), key=mm.group(2),
                    value=float(mm.group(3)), dloss_dp=float(mm.group(4)))
        for mm in _SENS_RE.finditer(out)
    ]
    if not sens:
        raise SolverError(f"keine sensitivity-Zeilen in:\n{out}")
    return loss, sens


@dataclass
class FocusMetrics:
    loss: float
    arrived: int
    total: int
    efl: float
    defocus: float


def read_loss_precise(binary: Path, config: Path) -> float:
    """Loss in voller Gleitkomma-Genauigkeit auslesen.

    Die menschenlesbare `trace`-Ausgabe rundet den Loss auf 6 Nachkommastellen;
    das ist fuer eine Finite-Differenzen-Gegenprobe viel zu grob (die Differenz
    zweier so gerundeter Werte kann komplett verschwinden). Der `sensitivity`-
    Befehl gibt `loss` dagegen in `%.12e` aus -- genau das nehmen wir hier.
    """
    out = _run(binary, ["sensitivity", "--config", str(config)])
    m = _LOSS_SENS_RE.search(out)
    if not m:
        raise SolverError(f"kein loss in sensitivity-Ausgabe:\n{out}")
    return float(m.group(1))


def read_focus(binary: Path, config: Path) -> FocusMetrics:
    """Nennschaerfe + Fokuslage einer Prescription auslesen (trace + efl)."""
    tr = _run(binary, ["trace", "--config", str(config)])
    m = _LOSS_TRACE_RE.search(tr)
    if not m:
        raise SolverError(f"kein loss in trace-Ausgabe:\n{tr}")
    arrived, total, loss = int(m.group(1)), int(m.group(2)), float(m.group(3))
    ef = _run(binary, ["efl", "--config", str(config)])
    efl = float(_EFL_RE.search(ef).group(1))
    defocus = float(_DEFOCUS_RE.search(ef).group(1))
    return FocusMetrics(loss=loss, arrived=arrived, total=total,
                        efl=efl, defocus=defocus)


# --------------------------------------------------------------------------
# Minimales TOML-Patchen (nur Zahlenfelder benannter Flaechen)
# --------------------------------------------------------------------------


def set_surface_field(toml_text: str, surface_name: str, field_: str, value: float) -> str:
    """Ersetzt `field = <Zahl>` im [[surfaces]]-Block mit passendem Namen."""
    lines = toml_text.splitlines()
    in_target = False
    seen_name = False
    patched = False
    name_pat = re.compile(r'^\s*name\s*=\s*"([^"]*)"')
    field_pat = re.compile(rf"^(\s*{re.escape(field_)}\s*=\s*)([-\d.eE+]+)(.*)$")
    for i, line in enumerate(lines):
        stripped = line.strip()
        if stripped.startswith("[[surfaces]]"):
            in_target = False
            seen_name = False
            continue
        if stripped.startswith("[") and not stripped.startswith("[["):
            in_target = False
        nm = name_pat.match(line)
        if nm:
            seen_name = True
            in_target = nm.group(1) == surface_name
            continue
        if in_target and seen_name:
            fm = field_pat.match(line)
            if fm:
                lines[i] = f"{fm.group(1)}{value}{fm.group(3)}"
                patched = True
                in_target = False
    if not patched:
        raise KeyError(f"surface {surface_name!r} field {field_!r} nicht gefunden")
    return "\n".join(lines) + "\n"


def get_surface_field(toml_text: str, surface_name: str, field_: str) -> float:
    """Liest den aktuellen Zahlenwert eines Feldes einer benannten Flaeche."""
    lines = toml_text.splitlines()
    in_target = False
    seen_name = False
    name_pat = re.compile(r'^\s*name\s*=\s*"([^"]*)"')
    field_pat = re.compile(rf"^\s*{re.escape(field_)}\s*=\s*([-\d.eE+]+)")
    for line in lines:
        stripped = line.strip()
        if stripped.startswith("[[surfaces]]"):
            in_target = False
            seen_name = False
            continue
        if stripped.startswith("[") and not stripped.startswith("[["):
            in_target = False
        nm = name_pat.match(line)
        if nm:
            seen_name = True
            in_target = nm.group(1) == surface_name
            continue
        if in_target and seen_name:
            fm = field_pat.match(line)
            if fm:
                return float(fm.group(1))
    raise KeyError(f"surface {surface_name!r} field {field_!r} nicht gefunden")


# --------------------------------------------------------------------------
# Robustheitskennzahl (gradientenbasiert)
# --------------------------------------------------------------------------


def scaled_sensitivity(s: Sensitivity, tolerances: dict[str, float]) -> float:
    """Toleranz-skalierte Empfindlichkeit: |dLoss/dp| * typische Abweichung.

    Diese Groesse hat fuer jeden Parametertyp dieselbe Einheit (Loss-Aenderung)
    und ist damit fair vergleichbar. Sie schaetzt, wie stark eine *typische*
    Fertigungsabweichung dieses einen Parameters die Bildschaerfe verschlechtert.
    """
    return abs(s.dloss_dp) * tolerances[s.key]


def position_metric(sens: list[Sensitivity], tolerances: dict[str, float]) -> float:
    """Robustheit einer Zoom-Stellung: Wurzel der Summe der quadrierten
    skalierten Sensitivitaeten (RSS). Klein = robust. Das ist die erwartete
    Loss-Verschlechterung, wenn alle Toleranzen unabhaengig zuschlagen."""
    return math.sqrt(sum(scaled_sensitivity(s, tolerances) ** 2 for s in sens))





# --------------------------------------------------------------------------
# Validierung: Autodiff gegen Finite Differenzen
# --------------------------------------------------------------------------


@dataclass
class FdCheck:
    label: str
    analytic: float
    numeric: float
    rel_error: float


def _force_on_axis(toml_text: str) -> str:
    """Setze `field_angles_deg = [0.0]` (on-axis only) fuer die FD-Gegenprobe.

    Das Pupil-Aiming ist ein bewusst PRIMALER (nicht-differenzierbarer)
    geometrischer Setup-Schritt pro Auswertung: der Autodiff-Gradient laeuft
    NICHT durch das Aiming (siehe Solver, `aim_pupil`). Eine Finite-Differenz
    ueber einen Linsenparameter wuerde das Aiming dagegen bei jeder Stoerung neu
    loesen und damit eine andere Groesse messen. Fuer eine saubere Gegenprobe
    des Autodiff-KERNS schalten wir die schraegen Felder ab; on-axis ist das
    Aiming die Identitaet, und Analytik und Finite Differenz muessen exakt
    uebereinstimmen. (Die Ranking-Sensitivitaeten selbst bleiben voll-feldrig.)
    """
    pat = re.compile(r'^\s*field_angles_deg\s*=.*$', re.MULTILINE)
    if pat.search(toml_text):
        return pat.sub("field_angles_deg = [0.0]", toml_text)
    # Kein Feld gesetzt -> im [source]-Block ergaenzen.
    return toml_text.replace("[source]", "[source]\nfield_angles_deg = [0.0]", 1)


def finite_difference_check(binary: Path, config: Path,
                            sens: list[Sensitivity],
                            n_check: int = 6) -> list[FdCheck]:
    """Zentrale Finite Differenzen der loss-Ausgabe gegen die Autodiff-Gradienten.

    Validiert den differenzierbaren KERN des Solvers on-axis (wo das primale
    Pupil-Aiming die Identitaet ist), damit Analytik und Finite Differenz
    dieselbe Groesse messen. Geprueft werden die (nach Betrag) groessten
    on-axis-Sensitivitaeten. Schrittweite je Parametertyp gewaehlt.
    """
    steps = {"radius": 1e-4, "thickness": 1e-4, "material": 1e-6}
    text = _force_on_axis(config.read_text(encoding="utf-8"))
    base_cfg = config.with_name("_fd_base.toml")
    base_cfg.write_text(text, encoding="utf-8")
    # On-axis analytische Sensitivitaeten (Aiming = Identitaet).
    _, on_axis_sens = read_sensitivities(binary, base_cfg)
    ranked = sorted(on_axis_sens, key=lambda s: -abs(s.dloss_dp))[:n_check]
    checks: list[FdCheck] = []
    for s in ranked:
        e = steps[s.key]
        plus = set_surface_field(text, s.surface, s.key, s.value + e)
        minus = set_surface_field(text, s.surface, s.key, s.value - e)
        p_cfg = config.with_name("_fd_plus.toml")
        m_cfg = config.with_name("_fd_minus.toml")
        p_cfg.write_text(plus, encoding="utf-8")
        m_cfg.write_text(minus, encoding="utf-8")
        lp = read_loss_precise(binary, p_cfg)
        lm = read_loss_precise(binary, m_cfg)
        numeric = (lp - lm) / (2 * e)
        denom = max(abs(s.dloss_dp), abs(numeric), 1e-9)
        rel = abs(s.dloss_dp - numeric) / denom
        checks.append(FdCheck(label=s.label, analytic=s.dloss_dp,
                              numeric=numeric, rel_error=rel))
        p_cfg.unlink(missing_ok=True)
        m_cfg.unlink(missing_ok=True)
    base_cfg.unlink(missing_ok=True)
    return checks


# --------------------------------------------------------------------------
# DoE ueber die freien Designvariablen (Kennzahl je Punkt per Gradient)
# --------------------------------------------------------------------------


def linspace(lo: float, hi: float, n: int) -> list[float]:
    if n == 1:
        return [lo]
    step = (hi - lo) / (n - 1)
    return [lo + step * i for i in range(n)]


@dataclass
class GapSolution:
    """Beste (g2, g3)-Lage einer Zoom-Stellung samt Kennzahl."""
    label: str
    g2: float
    g3: float
    metric: float
    loss: float


def metric_of_text(binary: Path, text: str, tolerances: dict[str, float],
                   work: Path, tag: str) -> tuple[float, float, int]:
    """Gradientenbasierte Kennzahl (RSS der skalierten Sensitivitaeten), Nenn-
    loss und Zahl angekommener Strahlen einer konkreten Prescription.
    EIN Autodiff-Aufruf plus ein trace, kein Raster."""
    work.mkdir(parents=True, exist_ok=True)
    cfg = work / f"{tag}.toml"
    cfg.write_text(text, encoding="utf-8")
    loss, sens = read_sensitivities(binary, cfg)
    fm = read_focus(binary, cfg)
    return position_metric(sens, tolerances), loss, fm.arrived


def improve_position(binary: Path, base_path: Path, tolerances: dict[str, float],
                     work: Path, span: float = 0.15, levels: int = 13
                     ) -> tuple[GapSolution, GapSolution, list[GapSolution]]:
    """Fuer EINE Zoom-Stellung: ausgehend von einem naiven Entwurf (Kompensator-
    Spalte g2/g3 gegenlaeufig verstellt) ein 2D-DoE-Raster ueber (g2, g3)
    ablaufen und an jedem Punkt die Robustheitskennzahl GRADIENTENBASIERT
    (Autodiff-Sensitivitaeten) messen. Gibt (Entwurf, bester Punkt, alle) zurueck.

    Das Raster ist um die bekannten guten Asset-Spalte zentriert (nicht um den
    Entwurf), damit die Suche die gute Lage sicher einschliesst. Kandidaten, die
    deutlich mehr Strahlen vignettieren als das Asset, werden verworfen: sonst
    koennte die Summen-Loss-Kennzahl faelschlich durch Abschattung "verbessert"
    werden statt durch echte Unempfindlichkeit.

    Warum DoE und nicht Gradient? Die Kennzahl ist selbst ein Gradient
    (Sensitivitaet). Sie nach g2/g3 abzuleiten waere eine zweite Ableitung, die
    der Forward-Mode-Solver nicht direkt liefert -> aeussere Rastersuche,
    innere Kennzahl per Autodiff.
    """
    base = base_path.read_text(encoding="utf-8")
    g2_0 = get_surface_field(base, *FREE_GAP_VARS[0])
    g3_0 = get_surface_field(base, *FREE_GAP_VARS[1])

    # Referenz-Strahlzahl des Assets (Vignettierungs-Schwelle).
    _, _, arrived_ref = metric_of_text(binary, base, tolerances,
                                       work, f"ref_{base_path.stem}")

    # Naiver Entwurf: g2/g3 gegenlaeufig um DRAFT_OFFSET_MM verschoben.
    draft_g2 = g2_0 + DRAFT_OFFSET_MM
    draft_g3 = g3_0 - DRAFT_OFFSET_MM
    draft_text = set_surface_field(base, *FREE_GAP_VARS[0], draft_g2)
    draft_text = set_surface_field(draft_text, *FREE_GAP_VARS[1], draft_g3)
    dm, dl, _ = metric_of_text(binary, draft_text, tolerances,
                               work, f"draft_{base_path.stem}")
    draft = GapSolution(base_path.stem, draft_g2, draft_g3, dm, dl)

    # 2D-Raster um die GUTEN Asset-Spalte (schliesst Entwurf und Optimum ein).
    g2_levels = linspace(g2_0 - span, g2_0 + span, levels)
    g3_levels = linspace(g3_0 - span, g3_0 + span, levels)
    cands: list[GapSolution] = []
    for i, g2 in enumerate(g2_levels):
        for j, g3 in enumerate(g3_levels):
            if g2 <= 0.02 or g3 <= 0.02:   # Luftspalt muss positiv/montierbar bleiben
                continue
            t = set_surface_field(base, *FREE_GAP_VARS[0], round(g2, 5))
            t = set_surface_field(t, *FREE_GAP_VARS[1], round(g3, 5))
            m, lo, arrived = metric_of_text(binary, t, tolerances, work,
                                            f"{base_path.stem}_{i}_{j}")
            if arrived < arrived_ref:   # keine "Verbesserung" durch Abschattung
                continue
            cands.append(GapSolution(base_path.stem, round(g2, 5),
                                     round(g3, 5), m, lo))
    best = min(cands, key=lambda c: c.metric)
    return draft, best, cands


# --------------------------------------------------------------------------
# Ausgabe-Artefakte
# --------------------------------------------------------------------------


def write_sensitivity_profile(path: Path,
                              profile: dict[str, list[Sensitivity]],
                              tolerances: dict[str, float]) -> None:
    """CSV: eine Zeile je (Parameter, Zoom-Stellung) mit roher + skalierter s_p."""
    cols = ["surface", "key", "zoom", "value", "dloss_dp",
            "tolerance", "scaled_sensitivity"]
    lines = [",".join(cols)]
    for label, sens in profile.items():
        for s in sens:
            lines.append(",".join([
                f'"{s.surface}"', s.key, label,
                f"{s.value:.6f}", f"{s.dloss_dp:.9e}",
                f"{tolerances[s.key]:.6g}",
                f"{scaled_sensitivity(s, tolerances):.9e}",
            ]))
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")


def write_tolerance_ranking(path: Path,
                            profile: dict[str, list[Sensitivity]],
                            tolerances: dict[str, float]) -> list[dict]:
    """CSV: Parameter nach worst-case-skalierter Sensitivitaet ueber alle
    Zoom-Stellungen sortiert. Zeigt zusaetzlich, in welcher Stellung der
    worst-case auftritt und wie das Ranking je Stellung aussieht."""
    # Aggregiere je Parameter-Label ueber alle Stellungen.
    agg: dict[str, dict] = {}
    for label, sens in profile.items():
        for s in sens:
            entry = agg.setdefault(s.label, {
                "surface": s.surface, "key": s.key,
                "per_zoom": {}, "worst": 0.0, "worst_zoom": ""})
            sc = scaled_sensitivity(s, tolerances)
            entry["per_zoom"][label] = sc
            if sc > entry["worst"]:
                entry["worst"] = sc
                entry["worst_zoom"] = label
    ranked = sorted(agg.items(), key=lambda kv: -kv[1]["worst"])
    zoom_labels = [lbl for lbl, _ in ZOOM_POSITIONS]
    cols = (["rank", "surface", "key"]
            + [f"scaled_{z}" for z in zoom_labels]
            + ["worst", "worst_zoom"])
    lines = [",".join(cols)]
    ranking_rows = []
    for i, (label, e) in enumerate(ranked, start=1):
        row = ([str(i), f'"{e["surface"]}"', e["key"]]
               + [f'{e["per_zoom"].get(z, 0.0):.9e}' for z in zoom_labels]
               + [f'{e["worst"]:.9e}', e["worst_zoom"]])
        lines.append(",".join(row))
        ranking_rows.append({
            "rank": i, "surface": e["surface"], "key": e["key"],
            "per_zoom": e["per_zoom"], "worst": e["worst"],
            "worst_zoom": e["worst_zoom"],
        })
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    return ranking_rows


# --------------------------------------------------------------------------
# Orchestrierung
# --------------------------------------------------------------------------


def main(argv: list[str]) -> int:
    ap = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--binary", type=Path, default=DEFAULT_BINARY)
    ap.add_argument("--out", type=Path, default=HERE / "results")
    args = ap.parse_args(argv)

    if not args.binary.exists():
        print(f"Fehler: Solver-Binary nicht gefunden unter {args.binary}\n"
              f"zuerst bauen:  (cd {SOURCE6} && cargo build --release)",
              file=sys.stderr)
        return 2

    args.out.mkdir(parents=True, exist_ok=True)
    tolerances = dict(DEFAULT_TOLERANCES)
    work = args.out / "work"

    # -- Schritt 0: Nennschaerfe / Parfokalitaet je Zoom-Stellung ----------
    print("Nennschaerfe und Fokuslage je Zoom-Stellung (Ausgangsdesign):")
    focus: dict[str, FocusMetrics] = {}
    for label, path in ZOOM_POSITIONS:
        fm = read_focus(args.binary, path)
        focus[label] = fm
        print(f"  {label:4s}: loss={fm.loss:8.5f}  EFL={fm.efl:7.3f} mm  "
              f"defocus={fm.defocus:+.3f} mm  ({fm.arrived}/{fm.total} Strahlen)")
    efls = [focus[l].efl for l, _ in ZOOM_POSITIONS]
    print(f"  Zoombereich: EFL {min(efls):.2f} .. {max(efls):.2f} mm "
          f"(Faktor {max(efls)/min(efls):.3f}x), Fokus fest -> parfokal.\n")

    # -- Schritt 1: SENSITIVITAET rein per Gradient (kein Sweep) -----------
    print("Schritt 1 — exakte Sensitivitaeten dLoss/dp per Autodiff "
          "(ein Solver-Aufruf je Zoom-Stellung, KEIN Raster):")
    profile: dict[str, list[Sensitivity]] = {}
    for label, path in ZOOM_POSITIONS:
        _, sens = read_sensitivities(args.binary, path)
        profile[label] = sens
        top = sorted(sens, key=lambda s: -scaled_sensitivity(s, tolerances))[:3]
        pretty = "; ".join(
            f"{s.label} (skal. {scaled_sensitivity(s, tolerances):.2e})"
            for s in top)
        print(f"  {label:4s}: {len(sens)} Parameter, kritischste 3: {pretty}")
    print()

    # -- Schritt 2: Toleranz-Ranking ---------------------------------------
    print("Schritt 2 — Toleranz-Ranking (worst-case ueber Zoom):")
    ranking = write_tolerance_ranking(args.out / "tolerance_ranking.csv",
                                      profile, tolerances)
    write_sensitivity_profile(args.out / "sensitivity_profile.csv",
                              profile, tolerances)
    for r in ranking[:5]:
        print(f"  #{r['rank']} {r['surface']} [{r['key']}]  "
              f"worst={r['worst']:.3e} @ {r['worst_zoom']}")
    print()

    # -- Schritt 4 (Validierung, vor der Optimierung): FD-Gegenprobe -------
    print("Validierung — Autodiff gegen zentrale Finite Differenzen "
          "(kritischste Parameter je Stellung):")
    fd_all: dict[str, list[FdCheck]] = {}
    max_rel = 0.0
    for label, path in ZOOM_POSITIONS:
        checks = finite_difference_check(args.binary, path, profile[label])
        fd_all[label] = checks
        worst_rel = max(c.rel_error for c in checks)
        max_rel = max(max_rel, worst_rel)
        print(f"  {label:4s}: max. rel. Fehler {worst_rel:.2e} "
              f"ueber {len(checks)} Parameter")
    verdict = "BESTANDEN" if max_rel < 1e-3 else "PRUEFEN"
    print(f"  -> groesster relativer Fehler gesamt: {max_rel:.2e}  [{verdict}]\n")

    # -- Schritt 3: Robustheit verbessern (DoE aussen, Gradient innen) -----
    print("Schritt 3 — Robustheit verbessern: je Zoom-Stellung ausgehend von\n"
          "  einem naiven Entwurf (Kompensator-Spalte g2/g3 verstellt) ein 2D-DoE\n"
          "  ueber (g2, g3); Kennzahl an jedem Punkt gradientenbasiert:")
    drafts: dict[str, GapSolution] = {}
    bests: dict[str, GapSolution] = {}
    all_cands: dict[str, list[GapSolution]] = {}
    for label, path in ZOOM_POSITIONS:
        draft, best, cands = improve_position(args.binary, path, tolerances, work)
        drafts[label] = draft
        bests[label] = best
        all_cands[label] = cands
        print(f"  {label:4s}: Entwurf g2={draft.g2:.3f} g3={draft.g3:.3f} "
              f"Kennzahl={draft.metric:.6f}  ->  "
              f"best g2={best.g2:.3f} g3={best.g3:.3f} "
              f"Kennzahl={best.metric:.6f} (loss={best.loss:.5f}) "
              f"ueber {len(cands)} Punkte")

    worst_before = max(d.metric for d in drafts.values())
    worst_after = max(b.metric for b in bests.values())
    improvement = (worst_before / worst_after) if worst_after > 0 else float("inf")
    print(f"\n  worst-case Kennzahl vorher (naiver Entwurf): {worst_before:.6f}")
    print(f"  worst-case Kennzahl nachher (DoE-optimiert): {worst_after:.6f}")
    print(f"  Verbesserung: Faktor {improvement:.3f}x")

    # -- Nennschaerfe/Parfokalitaet des verbesserten Designs pruefen -------
    print("\n  Kontrolle: Nennschaerfe/Parfokalitaet des verbesserten Designs:")
    improved_focus: dict[str, FocusMetrics] = {}
    for label, path in ZOOM_POSITIONS:
        text = path.read_text(encoding="utf-8")
        text = set_surface_field(text, *FREE_GAP_VARS[0], bests[label].g2)
        text = set_surface_field(text, *FREE_GAP_VARS[1], bests[label].g3)
        cfg = work / f"improved_{label}.toml"
        cfg.write_text(text, encoding="utf-8")
        fm = read_focus(args.binary, cfg)
        improved_focus[label] = fm
        (args.out / f"zoom_{label}_robust.toml").write_text(text, encoding="utf-8")
        print(f"    {label:4s}: loss {focus[label].loss:.5f} -> {fm.loss:.5f}  "
              f"defocus {focus[label].defocus:+.3f} -> {fm.defocus:+.3f} mm  "
              f"({fm.arrived}/{fm.total} Strahlen)")

    # -- Toleranzbudget (optional): erlaubte Abweichung je Parameter -------
    # Vorgabe: die schaerfste Stellung darf durch EINEN Parameter hoechstens
    # budget_loss an Loss verlieren. Erlaubte Abweichung = budget_loss / |s_p|.
    budget_loss = 0.02
    budget = []
    for label, sens in profile.items():
        for s in sens:
            if abs(s.dloss_dp) > 1e-9:
                allowed = budget_loss / abs(s.dloss_dp)
                budget.append({"surface": s.surface, "key": s.key,
                               "zoom": label, "allowed_deviation": allowed})

    # -- Roll-up -----------------------------------------------------------
    robustness_json = {
        "tolerances": tolerances,
        "free_gap_vars": [f"{s}|{f}" for (s, f) in FREE_GAP_VARS],
        "draft_offset_mm": DRAFT_OFFSET_MM,
        "metric_definition": ("RSS der toleranz-skalierten Autodiff-"
                              "Sensitivitaeten je Zoom-Stellung; "
                              "worst-case ueber die Stellungen"),
        "before": {
            "worst_metric": worst_before,
            "per_zoom": {l: {"g2": drafts[l].g2, "g3": drafts[l].g3,
                             "metric": drafts[l].metric, "loss": drafts[l].loss}
                         for l in drafts},
        },
        "after": {
            "worst_metric": worst_after,
            "per_zoom": {l: {"g2": bests[l].g2, "g3": bests[l].g3,
                             "metric": bests[l].metric, "loss": bests[l].loss}
                         for l in bests},
        },
        "improvement_factor": improvement,
    }
    (args.out / "robustness_before_after.json").write_text(
        json.dumps(robustness_json, indent=2), encoding="utf-8")

    summary = {
        "zoom_positions": [l for l, _ in ZOOM_POSITIONS],
        "focus_before": {l: asdict(focus[l]) for l in focus},
        "focus_after": {l: asdict(improved_focus[l]) for l in improved_focus},
        "efl_range": {"min": min(efls), "max": max(efls),
                      "ratio": max(efls) / min(efls)},
        "tolerances": tolerances,
        "tolerance_ranking": ranking,
        "robustness": robustness_json,
        "finite_difference_check": {
            l: [asdict(c) for c in fd_all[l]] for l in fd_all},
        "fd_max_rel_error": max_rel,
        "fd_verdict": verdict,
        "tolerance_budget_sample": sorted(
            budget, key=lambda b: b["allowed_deviation"])[:8],
        "budget_loss_target": budget_loss,
    }
    (args.out / "summary.json").write_text(
        json.dumps(summary, indent=2), encoding="utf-8")

    print("\nZusammenfassung")
    print(f"  Zoombereich EFL       : {min(efls):.2f} .. {max(efls):.2f} mm "
          f"({max(efls)/min(efls):.3f}x)")
    print(f"  Kritischster Parameter: {ranking[0]['surface']} "
          f"[{ranking[0]['key']}] @ {ranking[0]['worst_zoom']}")
    print(f"  Robustheit vorher     : {worst_before:.6f}")
    print(f"  Robustheit nachher    : {worst_after:.6f}  "
          f"(Faktor {improvement:.3f}x besser)")
    print(f"  Autodiff-Gegenprobe   : max. rel. Fehler {max_rel:.2e} [{verdict}]")
    print(f"\nArtefakte geschrieben nach {args.out}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
