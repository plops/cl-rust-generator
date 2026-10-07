# Aufwand: h konfigurierbar + 60-FPS-Maximum (2026-10-07)

## Token-Verbrauch

Nicht instrumentiert (kein Runtime-Zähler) — keine erfundene Zahl.

| Kennzahl | Wert (ca., aus Sitzungsprotokoll gezählt) |
|---|---|
| Turns (Aktionsblöcke) | ≈ 30 |
| Tool-Calls gesamt | ≈ 50 (davon ≈ 25 Shell-Kommandos) |
| Subagents | 0 (alles inline) |
| DeepWiki-Anfragen | 0 |
| `cargo test`-Läufe | ≈ 5 |
| `cargo oxide`-Builds/Läufe | ≈ 35 (h-Sweep 19, Block 6, GUI 12, Verifikation) |
| GUI-Smoke-Tests (X11) | 12 (60–2000 Frames) |
| Commits | 4 (2 docs + 1 feat + 1 perf) |

## Zeit

- Wall-Clock: ≈ 75 Minuten.
- Größte Blöcke: h×N-Sweep mit Stabilitätsgate (≈ 25 Min),
  GUI-Vermessung inkl. Draw/Present-Diagnose (≈ 20 Min), Interactive
  Screenshots (ergebnislos, schwarze Bilder, ≈ 10 Min).

## Bemerkung

Der teuerste Irrweg war der Screenshot-Vergleich (h=0,02 vs. 0,04):
X-Capture liefert bei indirektem GL nur Schwarz — früher abbrechen,
quantitative Gates (PASS + ρ-Statistik) genügen. Ertragreichster Fund:
Present kostet ~9 ms Sockel über X11 — das allein erklärt die GUI-Grenze
und war in 10 Minuten Split-Timing isoliert.
