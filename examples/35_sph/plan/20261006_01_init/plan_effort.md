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


◆ Fertig: Die GPU-SPH-Fluidsimulation ist implementiert, verifiziert und in 8 Conventional Commits gesichert. Alle Gates sind grün, der Arbeitsbaum ist sauber.

  Ergebnis: Sechs SPH-Kernel in nativem Rust (cuda-oxide → PTX) mit Voll-GPU-Uniform-Grid, dazu CPU-Referenzbackend, Headless-Validierung/Benchmark und interaktive macroquad-GUI. 500 GPU-Schritte: PASS (kein NaN/Inf, kein Tunneln), 22 Mio
  Partikel/s bei N=16 384 (47× schneller als CPU), GUI-Smoke-Test mit 120 Frames bei ~60 FPS auf dem Host-X.

  Wichtigste Diagnose unterwegs: Der Damm kollabierte zunächst, weil im Druckterm der Faktor ρᵢ fehlte (Abstoßung ~1000× zu schwach) — per k-Sweep als Formel- statt Konstantenfehler identifiziert und in GPU-Kernel und CPU-Spiegel identisch
  gefixt.

  Struktur: Cargo.toml, src/01_types.rs … src/09_headless.rs (nummeriert, main.rs/lib.rs nur Verdrahtung), tests/ (9 Integrationstests), benches/, deps.md; Plan/Todo/Walkthrough/Aufwand in plan/20261006_01_init/.

  Drei ehrliche Einschränkungen (Details im Walkthrough): Erstens rendert die GUI mit LIBGL_ALWAYS_SOFTWARE=1, weil Container und Host-X kein IPC für MIT-SHM teilen (Physik läuft trotzdem auf der GPU; echte Lösung: --ipc=host). Zweitens
  skaliert N=262 144 nur mit 2 Mio/s, da bei festem h die Nachbarzahl ∝ N wächst (Physik, kein Bug). Drittens ist eine Commit-Message (af3c72b) unpräzise — die genannten Overrides landeten bereits in Vorgänger-Commits; ohne
  Amend-Autorisierung habe ich die Historie nicht umgeschrieben.

  Start: cargo oxide run --features gpu (GUI, DISPLAY=:0) bzw. cargo oxide run --features gpu -- --headless --steps 500; Gates: cargo fmt --check, cargo clippy --all-targets [--features gpu] -- -D warnings, cargo test (25 Tests).

◆ Worked for 33m 29s · 8:57 PM

┌────────────────────────────────────────────────────────┐
│  Session usage                                         │
│                                                        │
│    Input      22,874,782                               │
│    Cached     22,238,051                               │
│    Output        138,439                               │
│    Total      23,013,221                               │
│                                                        │
│    Turns              150                              │
│    Subagents         none                              │
│                                                        │
│  Subscription · Muse Code Everyday Usage               │
│    Current        11% used · Resets at 1:23 AM         │
│    Weekly         26% used · Resets Oct 12 at 12:00 AM │
└────────────────────────────────────────────────────────┘

┌──────────────────────────────────────────────────────────────────┐
│  MUSE CODE 1.4.2 / rust-gibbous                        COMPLETED │
│                                                                  │
│  MODEL          muse-spark-1.3-contributor · max                 │
│                 meta · native-basic                              │
│                                                                  │
│  WORKSPACE      /workspace/src/cl-rust-generator/examples/35_sph │
│                 trusted · not found                              │
│  ACCESS         Unrestricted                                     │
│                 sandbox disabled (--yolo)                        │
│                 Meta account                                     │
│  ACCOUNT        Wol Pumba (wolpumba@gmail.com)                   │
│                                                                  │
│  USAGE          23,013,221 tokens · 150 turns · 0 subagents      │
│  CONTEXT        75% left · 256K used / 1008K · normal            │
│                                                                  │
│  SESSION        01a112e2-880f-7aa2-bd0a-a6a92a069093             │
│  ACTIVITY       no tasks                                         │
│                 0 terminals · inbox clear                        │
│                                                                  │
│  BILLING        Subscription · Muse Code Everyday Usage          │
└──────────────────────────────────────────────────────────────────┘
