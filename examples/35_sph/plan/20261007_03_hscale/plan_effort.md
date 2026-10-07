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


  Berichte: plan/20261007_03_hscale/walkthrough.md (Sweep-Tabellen, h(N)-Empfehlung, Frame-Diagramm),
  plan/20261007_03_hscale/plan_effort.md.

◆ Worked for 10m 51s · 6:02 AM

┌────────────────────────────────────────────────────────┐
│  Session usage                                         │
│                                                        │
│    Input      19,221,814                               │
│    Cached     18,979,350                               │
│    Output         89,149                               │
│    Total      19,310,963                               │
│                                                        │
│    Turns              119                              │
│    Subagents         none                              │
│                                                        │
│  Subscription · Muse Code Everyday Usage               │
│    Current        7% used · Resets at 10:13 AM         │
│    Weekly         30% used · Resets Oct 12 at 12:00 AM │
└────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────────────────────────┐
│  MUSE CODE 1.4.3 / morning-dione                                          COMPLETED │
│                                                                                     │
│  MODEL          muse-spark-1.3-contributor · max                                    │
│                 meta · native-basic                                                 │
│                                                                                     │
│  WORKSPACE      /workspace/src/cl-rust-generator/examples/35_sph                    │
│                 trusted · not found                                                 │
│  ACCESS         Unrestricted                                                        │
│                 sandbox disabled (--yolo)                                           │
│                 Meta account                                                        │
│  ACCOUNT        Wol Pumba (wolpumba@gmail.com)                                      │
│                                                                                     │
│  USAGE          19,310,963 tokens · 119 turns · 0 subagents                         │
│  CONTEXT        77% left · 236K used / 1008K · normal                               │
│                 Usage: cumulative session tokens; context: current prompt estimate. │
│                                                                                     │
│  SESSION        01a114c8-dd7c-7c82-8a03-4cb8db173469                                │
│  ACTIVITY       no tasks                                                            │
│                 0 terminals · inbox clear                                           │
│                                                                                     │
│  BILLING        Subscription · Muse Code Everyday Usage                             │
└─────────────────────────────────────────────────────────────────────────────────────┘

───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
❯
───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
  muse-spark-1.3-contributor · max · /workspace/src/cl-rust-generator/examples/35_sph · YOLO
