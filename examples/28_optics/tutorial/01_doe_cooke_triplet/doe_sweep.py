#!/usr/bin/env python3
"""Design-of-Experiments sweep and analysis for the `optics` ray tracer.

This script treats the compiled Rust binary `optics` as a black-box solver
and drives a real optical-engineering task from Python:

  1. Take a bundled Cooke-triplet prescription that is *out of focus*
     (its image plane sits ~19 mm behind where the rays actually converge).
  2. Run a two-factor Design of Experiments (DoE) over the back air gap
     (back-focal distance) and the rear element curvature, tracing the
     polychromatic RMS spot loss at every grid cell.
  3. Analyse the response surface: locate the minimum, quantify how strongly
     each factor moves the loss (main effects), and pick the best design.
  4. Validate by handing the DoE-best design to the solver's own
     gradient-descent optimiser and confirming it converges into the same
     basin, then re-checking the focus.

Everything here is pure Python standard library (no numpy / matplotlib), so
the demo runs anywhere Python 3.9+ and the compiled binary are present.

Usage:
    python3 doe_sweep.py [--binary PATH] [--base-config PATH] [--out DIR]

Outputs (written into --out, default: ./results):
    doe_grid.csv            one row per DoE design with loss / EFL / defocus
    best_design.toml        the DoE-best prescription (fed to the optimiser)
    optimized.toml          the optimiser-refined prescription
    summary.json            machine-readable roll-up of the whole run
"""

from __future__ import annotations

import argparse
import json
import re
import subprocess
import sys
from dataclasses import dataclass, asdict
from pathlib import Path

# --------------------------------------------------------------------------
# Locating the solver and the baseline prescription
# --------------------------------------------------------------------------

HERE = Path(__file__).resolve().parent
# tutorial/01_doe_cooke_triplet -> ../../source6
SOURCE6 = (HERE / ".." / ".." / "source6").resolve()
DEFAULT_BINARY = SOURCE6 / "target" / "release" / "optics"
DEFAULT_BASE = SOURCE6 / "assets" / "cooke.toml"


# --------------------------------------------------------------------------
# Talking to the solver: run the CLI, parse its printed metrics
# --------------------------------------------------------------------------

# The `efl` subcommand prints three lines we care about:
#   EFL = 89.1073 mm
#   back focus z = 104.1349 mm (image plane 123.2000)
#   defocus = 19.0651 mm
_EFL_RE = re.compile(r"EFL\s*=\s*([-\d.]+)")
_DEFOCUS_RE = re.compile(r"defocus\s*=\s*([-\d.]+)")
_BACKFOCUS_RE = re.compile(r"back focus z\s*=\s*([-\d.]+)")

# The `trace` subcommand prints:  rays: 24/30 arrived, loss = 102.477846
_LOSS_RE = re.compile(r"rays:\s*(\d+)/(\d+)\s*arrived,\s*loss\s*=\s*([-\d.eE+]+)")

# The `optimize` subcommand prints: loss: 0.496775 -> 0.346787 (40 iters)
_OPT_RE = re.compile(r"loss:\s*([-\d.eE+]+)\s*->\s*([-\d.eE+]+)\s*\((\d+)\s*iters\)")


class SolverError(RuntimeError):
    """Raised when the optics binary exits non-zero or prints no metrics."""


def _run(binary: Path, args: list[str]) -> str:
    """Invoke the binary and return stdout, raising on failure."""
    proc = subprocess.run(
        [str(binary), *args],
        capture_output=True,
        text=True,
        cwd=str(SOURCE6),  # so relative asset paths in configs still resolve
    )
    if proc.returncode != 0:
        raise SolverError(
            f"optics {' '.join(args)} failed ({proc.returncode}): {proc.stderr.strip()}"
        )
    return proc.stdout


@dataclass
class TraceMetrics:
    """What one trace + efl evaluation tells us about a design."""

    loss: float
    arrived: int
    total: int
    efl: float
    back_focus_z: float
    defocus: float


def evaluate(binary: Path, config: Path) -> TraceMetrics:
    """Trace a prescription and read back its spot loss and focus metrics."""
    trace_out = _run(binary, ["trace", "--config", str(config)])
    m = _LOSS_RE.search(trace_out)
    if not m:
        raise SolverError(f"could not parse loss from:\n{trace_out}")
    arrived, total, loss = int(m.group(1)), int(m.group(2)), float(m.group(3))

    efl_out = _run(binary, ["efl", "--config", str(config)])
    efl = _first_float(_EFL_RE, efl_out, "EFL")
    back = _first_float(_BACKFOCUS_RE, efl_out, "back focus")
    defocus = _first_float(_DEFOCUS_RE, efl_out, "defocus")

    return TraceMetrics(
        loss=loss,
        arrived=arrived,
        total=total,
        efl=efl,
        back_focus_z=back,
        defocus=defocus,
    )


def _first_float(pattern: re.Pattern[str], text: str, label: str) -> float:
    m = pattern.search(text)
    if not m:
        raise SolverError(f"could not parse {label} from:\n{text}")
    return float(m.group(1))


# --------------------------------------------------------------------------
# Minimal TOML rewriting: we only need to patch numeric fields on named
# surfaces of an existing, valid prescription. This avoids depending on a
# TOML writer and keeps the human-authored file layout intact.
# --------------------------------------------------------------------------


def load_text(path: Path) -> str:
    return path.read_text(encoding="utf-8")


def set_surface_field(toml_text: str, surface_name: str, field: str, value: float) -> str:
    """Replace `field = <number>` inside the [[surfaces]] block whose name matches.

    Works on the tidy, one-key-per-line TOML the project uses. Raises if the
    surface or field is not found so silent no-ops can't corrupt a sweep.
    """
    lines = toml_text.splitlines()
    in_target = False
    seen_name = False
    patched = False
    name_pat = re.compile(r'^\s*name\s*=\s*"([^"]*)"')
    field_pat = re.compile(rf"^(\s*{re.escape(field)}\s*=\s*)([-\d.eE+]+)(.*)$")

    for i, line in enumerate(lines):
        stripped = line.strip()
        if stripped.startswith("[[surfaces]]"):
            in_target = False
            seen_name = False
            continue
        if stripped.startswith("[") and not stripped.startswith("[["):
            in_target = False  # left the surfaces array into another table
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
                in_target = False  # only the first match per surface

    if not patched:
        raise KeyError(f"surface {surface_name!r} field {field!r} not found")
    return "\n".join(lines) + "\n"


def add_optimize_directive(toml_text: str, surface_name: str, key: str,
                           learning_rate: float, iters: int) -> str:
    """Flag `key` as optimizable on a surface and append an [optimize] table."""
    lines = toml_text.splitlines()
    name_pat = re.compile(r'^\s*name\s*=\s*"([^"]*)"')
    out: list[str] = []
    target_idx = None
    for i, line in enumerate(lines):
        out.append(line)
        nm = name_pat.match(line)
        if nm and nm.group(1) == surface_name:
            target_idx = len(out)  # remember where this surface's body starts
    if target_idx is None:
        raise KeyError(f"surface {surface_name!r} not found")
    # Insert the optimize directive right after the target surface's name line.
    out.insert(target_idx, f'optimize = ["{key}"]')
    text = "\n".join(out) + "\n"
    # Drop any pre-existing [optimize] table, then append a fresh one.
    text = re.sub(r"\n\[optimize\][^\[]*", "\n", text)
    text += f"\n[optimize]\nlearning_rate = {learning_rate}\niters = {iters}\n"
    return text


# --------------------------------------------------------------------------
# The Design of Experiments itself
# --------------------------------------------------------------------------


def linspace(lo: float, hi: float, n: int) -> list[float]:
    """Evenly spaced points including both endpoints (stdlib float range)."""
    if n == 1:
        return [lo]
    step = (hi - lo) / (n - 1)
    return [lo + step * i for i in range(n)]


@dataclass
class Factor:
    """One DoE factor: which surface field it maps to, and its sweep levels."""

    name: str            # short label used in output columns
    surface: str         # [[surfaces]] name to patch
    field: str           # radius / thickness / material
    levels: list[float]


@dataclass
class DoeCell:
    """One evaluated design point in the grid."""

    factor_values: dict[str, float]
    metrics: TraceMetrics


def run_doe(binary: Path, base_text: str, factors: list[Factor],
            work_dir: Path) -> list[DoeCell]:
    """Full-factorial sweep: evaluate the solver at every combination of levels."""
    work_dir.mkdir(parents=True, exist_ok=True)
    grids = _cartesian(factors)
    cells: list[DoeCell] = []
    for idx, combo in enumerate(grids):
        text = base_text
        for factor, value in combo.items():
            f = next(f for f in factors if f.name == factor)
            text = set_surface_field(text, f.surface, f.field, value)
        cfg = work_dir / f"cell_{idx:03d}.toml"
        cfg.write_text(text, encoding="utf-8")
        metrics = evaluate(binary, cfg)
        cells.append(DoeCell(factor_values=combo, metrics=metrics))
        print(
            f"  cell {idx:3d}: "
            + ", ".join(f"{k}={v:.3f}" for k, v in combo.items())
            + f"  -> loss={metrics.loss:10.4f}  defocus={metrics.defocus:+.3f} mm"
        )
    return cells


def _cartesian(factors: list[Factor]) -> list[dict[str, float]]:
    """Full-factorial expansion of factor levels into dicts."""
    combos: list[dict[str, float]] = [{}]
    for f in factors:
        combos = [dict(c, **{f.name: lvl}) for c in combos for lvl in f.levels]
    return combos


# --------------------------------------------------------------------------
# Analysis
# --------------------------------------------------------------------------


def best_cell(cells: list[DoeCell]) -> DoeCell:
    """The design with the lowest spot loss."""
    return min(cells, key=lambda c: c.metrics.loss)


def main_effects(cells: list[DoeCell], factors: list[Factor]) -> dict[str, float]:
    """Range of the mean loss across each factor's levels (bigger = stronger).

    For each factor we group cells by that factor's level, average the loss in
    each group, and report the spread (max mean - min mean). This is the
    classic DoE 'main effect' magnitude: how much moving that one knob shifts
    the average outcome.
    """
    effects: dict[str, float] = {}
    for f in factors:
        by_level: dict[float, list[float]] = {}
        for c in cells:
            by_level.setdefault(c.factor_values[f.name], []).append(c.metrics.loss)
        means = [sum(v) / len(v) for v in by_level.values()]
        effects[f.name] = max(means) - min(means)
    return effects


def ascii_heatmap(cells: list[DoeCell], x: Factor, y: Factor) -> str:
    """Render the 2-factor loss surface as an ASCII heatmap (log-scaled shades)."""
    import math

    losses = [c.metrics.loss for c in cells]
    lo, hi = min(losses), max(losses)
    ramp = " .:-=+*#%@"  # low loss -> high loss

    def shade(v: float) -> str:
        if hi <= lo:
            return ramp[0]
        # log scale so the deep minimum stays visible against huge maxima
        t = (math.log10(v + 1e-9) - math.log10(lo + 1e-9)) / (
            math.log10(hi + 1e-9) - math.log10(lo + 1e-9)
        )
        return ramp[min(len(ramp) - 1, max(0, int(t * (len(ramp) - 1))))]

    lookup = {(c.factor_values[x.name], c.factor_values[y.name]): c.metrics.loss
              for c in cells}
    rows = []
    header = "        " + "".join(f"{xv:6.1f}" for xv in x.levels)
    rows.append(header)
    for yv in y.levels:
        cells_row = "".join(f"   {shade(lookup[(xv, yv)])}  " for xv in x.levels)
        rows.append(f"{yv:7.1f} {cells_row}")
    rows.append("")
    rows.append(f"x = {x.name} ({x.field} of {x.surface})")
    rows.append(f"y = {y.name} ({y.field} of {y.surface})")
    rows.append(f"shades ' .:-=+*#%@' map log-loss from {lo:.3f} to {hi:.1f}")
    return "\n".join(rows)


# --------------------------------------------------------------------------
# Orchestration
# --------------------------------------------------------------------------


def write_csv(path: Path, cells: list[DoeCell], factors: list[Factor]) -> None:
    cols = [f.name for f in factors] + [
        "loss", "arrived", "total", "efl", "back_focus_z", "defocus"
    ]
    lines = [",".join(cols)]
    for c in cells:
        row = [f"{c.factor_values[f.name]:.6g}" for f in factors] + [
            f"{c.metrics.loss:.6f}",
            str(c.metrics.arrived),
            str(c.metrics.total),
            f"{c.metrics.efl:.4f}",
            f"{c.metrics.back_focus_z:.4f}",
            f"{c.metrics.defocus:.4f}",
        ]
        lines.append(",".join(row))
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")


def parse_optimize(binary: Path, config: Path) -> dict:
    """Run the solver's gradient-descent optimiser, return its reported deltas."""
    out = _run(binary, ["optimize", "--config", str(config),
                         "--write-back", str(config.with_name("optimized.toml"))])
    m = _OPT_RE.search(out)
    if not m:
        raise SolverError(f"could not parse optimize output:\n{out}")
    return {
        "loss_start": float(m.group(1)),
        "loss_end": float(m.group(2)),
        "iters": int(m.group(3)),
        "raw": out.strip(),
    }


def main(argv: list[str]) -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--binary", type=Path, default=DEFAULT_BINARY)
    ap.add_argument("--base-config", type=Path, default=DEFAULT_BASE)
    ap.add_argument("--out", type=Path, default=HERE / "results")
    args = ap.parse_args(argv)

    if not args.binary.exists():
        print(f"error: solver binary not found at {args.binary}\n"
              f"build it first:  (cd {SOURCE6} && cargo build --release)",
              file=sys.stderr)
        return 2

    base_text = load_text(args.base_config)
    args.out.mkdir(parents=True, exist_ok=True)

    # -- Baseline: the prescription as shipped -----------------------------
    print("Baseline (prescription as shipped):")
    baseline = evaluate(args.binary, args.base_config)
    print(f"  loss = {baseline.loss:.4f}   EFL = {baseline.efl:.3f} mm   "
          f"defocus = {baseline.defocus:+.3f} mm\n")

    # -- Factors -----------------------------------------------------------
    # The Cooke triplet's obvious defect is a mis-set back-focal distance
    # (the last air gap). We also sweep the rear element's curvature, which
    # trades focus against spherical aberration.
    factors = [
        Factor(name="back_gap", surface="L3 Back", field="thickness",
               levels=linspace(60.0, 90.0, 7)),
        Factor(name="rear_R", surface="L3 Back", field="radius",
               levels=linspace(-60.0, -45.0, 5)),
    ]

    print(f"DoE: full factorial, "
          f"{'x'.join(str(len(f.levels)) for f in factors)} = "
          f"{len(_cartesian(factors))} designs\n")
    cells = run_doe(args.binary, base_text, factors, args.out / "cells")

    # -- Analysis ----------------------------------------------------------
    best = best_cell(cells)
    effects = main_effects(cells, factors)
    heat = ascii_heatmap(cells, factors[0], factors[1])

    print("\nResponse surface (loss):")
    print(heat)
    print("\nMain effects (spread of mean loss across each factor):")
    for name, eff in sorted(effects.items(), key=lambda kv: -kv[1]):
        print(f"  {name:10s} {eff:12.4f}")

    print("\nDoE-best design:")
    for k, v in best.factor_values.items():
        print(f"  {k} = {v:.4f}")
    print(f"  loss = {best.metrics.loss:.4f}   defocus = {best.metrics.defocus:+.3f} mm")

    # Persist the best design as a standalone prescription.
    best_text = base_text
    for f in factors:
        best_text = set_surface_field(best_text, f.surface, f.field,
                                      best.factor_values[f.name])
    best_path = args.out / "best_design.toml"
    best_path.write_text(best_text, encoding="utf-8")

    # -- Validation: hand the DoE-best to the gradient optimiser -----------
    print("\nValidation: refining the DoE-best design with exact-gradient descent...")
    opt_input = args.out / "best_design_opt.toml"
    opt_text = add_optimize_directive(best_text, "L3 Back", "thickness",
                                      learning_rate=0.01, iters=60)
    opt_input.write_text(opt_text, encoding="utf-8")
    opt = parse_optimize(args.binary, opt_input)
    optimized_metrics = evaluate(args.binary, args.out / "optimized.toml")
    print(f"  optimiser: loss {opt['loss_start']:.4f} -> {opt['loss_end']:.4f} "
          f"in {opt['iters']} iters")
    print(f"  refined design: loss = {optimized_metrics.loss:.4f}   "
          f"defocus = {optimized_metrics.defocus:+.3f} mm")

    # -- Roll-up -----------------------------------------------------------
    write_csv(args.out / "doe_grid.csv", cells, factors)
    summary = {
        "baseline": asdict(baseline),
        "factors": [
            {"name": f.name, "surface": f.surface, "field": f.field,
             "levels": f.levels}
            for f in factors
        ],
        "n_designs": len(cells),
        "main_effects": effects,
        "doe_best": {
            "factor_values": best.factor_values,
            "metrics": asdict(best.metrics),
        },
        "optimizer": {
            "loss_start": opt["loss_start"],
            "loss_end": opt["loss_end"],
            "iters": opt["iters"],
            "refined_metrics": asdict(optimized_metrics),
        },
        "improvement": {
            "loss_baseline": baseline.loss,
            "loss_doe_best": best.metrics.loss,
            "loss_optimized": optimized_metrics.loss,
            "factor_vs_baseline": baseline.loss / optimized_metrics.loss,
        },
    }
    (args.out / "summary.json").write_text(json.dumps(summary, indent=2),
                                           encoding="utf-8")

    print("\nSummary")
    print(f"  baseline loss    : {baseline.loss:12.4f}")
    print(f"  DoE-best loss    : {best.metrics.loss:12.4f}")
    print(f"  optimized loss   : {optimized_metrics.loss:12.4f}")
    print(f"  total improvement: {baseline.loss / optimized_metrics.loss:11.1f}x")
    print(f"\nArtifacts written to {args.out}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
