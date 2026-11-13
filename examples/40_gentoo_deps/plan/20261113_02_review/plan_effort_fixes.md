# plan_effort_fixes.md — Aufwand Review-Fixes (`plan/20261113_02_review/fix_plan.md`)

Stand: 2026-11-13. Session mit hohem Token-Verbrauch (komplexer
Multi-Datei-Umbau über ~12 Turns, inkl. Skill-Loads, vollständiger
Test-/Gate-Läufe und Doku). Exakte Zahl: siehe Session-Abrechnung
(~22–23 Mio. Tokens zum Abschluss).

## Wo der Aufwand anfiel

- Code-Lektüre: 15 Source-Dateien + 2 Test-Suiten + Review/Fix-Plan
  (mehrfach, inkl. Re-Reads nach jedem Reminder).
- 6 Implementierungs-Blöcke: PerfStats/Instrumentierung, Engine-Parallelisierung
  (`rayon`, Runtime-only, Manifest-Cache), TUI-Entkopplung/Scrolling/Choice,
  Delta-Paketliste, Portage-Fixes, Verify-Korrektur.
- Verifikation: `cargo check`/`clippy`/`fmt`/`test`/`perf_system` je mehrfach
  (3 echte Debug-Runden: deterministischer Big-Levers-Vergleich, Scroll-Tests,
  Choice-Baseline-Semantik).
- Doku: `gentoo-dep-tui/README.md` (neu), Walkthrough-Teil-2, dieser Eintrag.

## Ergebnis

Alle 6 Phasen umgesetzt, alle Gates grün (53 lib- + 4 hil- + 9
host-Integration- + 5 perf-Tests, `fmt --check`, `clippy -D warnings`).
Bewusste Reste (siehe Walkthrough Teil 2): Datei-Aufteilung >600 Zeilen,
Thread-Worker statt Idle-Recompute, HIL-Zahlen auf echtem Snapshot.
