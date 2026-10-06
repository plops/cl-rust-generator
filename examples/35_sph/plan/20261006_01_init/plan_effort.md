# Aufwand: 35_sph-Session (2026-10-06)

## Token-Verbrauch

Die Runtime legt keine Token-Zähler offen (`host.budget` existiert nur für
Workflow-Kindläufe; es liefen keine). Daher ehrlich: **nicht instrumentiert**.

| Kennzahl | Wert |
|---|---|
| Input / Output / Cached / Total Tokens | nicht verfügbar (kein Zähler in Runtime) |
| Turns (Aktionsblöcke) | ≈ 65 |
| Tool-Calls gesamt | ≈ 175 (davon ≈ 105 Shell-Kommandos) |
| Subagents | 0 (alles inline, keine Delegation) |
| DeepWiki-Anfragen | 3 (Kernel-Autorenschaft, Device-Features) |
| `cargo test`-Läufe | ≈ 15 |
| `cargo oxide`-Builds/Läufe | ≈ 20 |
| GUI-Smoke-Tests (X11) | 3 (30/120/120 Frames) |
| Commits | 8 (Conventional Commits, alle grün) |

## Zeit

- Wall-Clock: ≈ 55 Minuten (20:03–20:58 UTC).
- Größte Kostenblöcke: Nightly-/Clang-Installation + Erstkompilierung der
  Oxide-Abhängigkeiten (≈ 10 Min), ρᵢ-Diagnose mit k-Sweeps (≈ 10 Min),
  Feature-Gate-Umstellung inkl. Clippy-Nacharbeit (≈ 10 Min).

## Bemerkung zur Schätzung

Turn-/Call-Zahlen sind aus dem Sitzungsprotokoll gezählt (Chunk-IDs der
Tool-Resultate), keine Token-Hochrechnung — eine erfundene Token-Zahl wäre
irreführend. Für künftige Sitzungen: Token-Zähler am Session-Ende aus dem
Harness exportieren (falls verfügbar) statt schätzen.
