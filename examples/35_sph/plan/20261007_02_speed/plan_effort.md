# Aufwand: GPU-Performance-Optimierung (2026-10-07)

## Token-Verbrauch

Die Runtime legt keine Token-Zähler offen, daher ehrlich:
**nicht instrumentiert** (keine erfundene Zahl).

| Kennzahl | Wert (ca., aus Sitzungsprotokoll gezählt) |
|---|---|
| Input / Output / Total Tokens | nicht verfügbar (kein Zähler in Runtime) |
| Turns (Aktionsblöcke) | ≈ 35 |
| Tool-Calls gesamt | ≈ 75 (davon ≈ 30 Shell-Kommandos) |
| Subagents | 0 (alles inline, keine Delegation) |
| DeepWiki-Anfragen | 0 (cuda-oxide-Quellen lagen im Container vor und sind aktueller) |
| `cargo test`-Läufe | ≈ 6 |
| `cargo oxide`-Builds/Läufe | ≈ 12 (Baseline-Sweep, 3 Phasen-Tests, Final-Sweep, GUI-Smoke) |
| GUI-Smoke-Tests (X11) | 1 (120 Frames, PASS) |
| Commits | 5 (1 docs + 3 perf + 1 docs-Abschluss, alle grün) |

## Zeit

- Wall-Clock: ≈ 60 Minuten.
- Größte Kostenblöcke: Baseline-Sweep über 4 Partikelgrößen inkl.
  N=262k-Lauf (≈ 10 Min), P1-Ping-Pong-Implementierung mit Dual-Modul-Split
  (≈ 15 Min), Final-Sweep + Bericht (≈ 15 Min). Keine Sackgassen: Dual-Modul,
  Shared-Memory-Scan und Event-Timing liefen jeweils beim ersten GPU-Build.

## Bemerkung

Turn-/Call-Zahlen sind aus den Tool-Resultaten gezählt, keine
Hochrechnung. Auffällig: Der parallele Scan brachte 1,33× statt der
erwarteten ~1,05× — der serielle Scan war nach P1 mit ~25 % der größte
Einzelposten geworden (im Walkthrough belegt).
