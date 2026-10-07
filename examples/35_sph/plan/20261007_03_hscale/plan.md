# Plan: h konfigurierbar + 60-FPS-Maximum suchen (03_hscale)

Datum: 2026-10-07. Basis: Stand nach 02_speed (0,276 ms/Schritt bei
N=16.384, h=0,04).

## 1. Ziel

- `h` (Glättungslänge = Zellgröße) per CLI konfigurierbar (`--h`).
- Maximale Partikelzahl N finden, die mit **60 FPS** (≤ 16,67 ms/Frame,
  Default: 3 Sub-Steps/Frame) stabil simulierbar ist.
- Weitere Effizienz-Parameter prüfen und ggf. optimieren: Blockgröße,
  (`dt`/Steifigkeit nur als Stabilitätsgrenze, keine Verhaltensänderung).

## 2. Analyse: Warum h der Schlüssel ist

- Nachbarkosten ∝ (h/s)², mit Anfangsabstand s = √(0,612/N).
  Bei festem h=0,04 wächst die Nachbarzahl mit N (N=16k: ~130, N=262k:
  ~2.000) → superlineare Kosten. Konstantes h/s-Verhältnis (h ∝ 1/√N)
  hält die Nachbarzahl konstant → lineare Skalierung.
- Faustformel für ~130 Nachbarn: h*(N) = 0,04·√(16384/N).
  N=32k→0,028, 65k→0,020, 98k→0,016, 131k→0,014, 262k→0,010.
- Gegenkräfte bei kleinem h: CFL-Bedingung (dt < h/c, c≈√k≈45 m/s),
  rauschere Dichte (Validierungsband (0, 5ρ₀]), mehr Zellen (h=0,01 →
  160×100 = 16k Zellen; Chunk-Scan skaliert, prüfen).
- 60-FPS-Budget: 3 × ms/Schritt + Download + Render + Present ≤ 16,67 ms.
  Physik per Headless (präzise), GUI-Anteil per In-App-Timing + Wanduhr
  (X11-Umgebung beachten, ehrlich trennen).

## 3. Umsetzung

### 3.1 Code

1. `--h` in `02_params.rs` (`Cli.h: Option<f32>`, `sim_config` übernimmt,
   Fehler bei nicht-positiv/nicht-finit, Hilfe + Unit-Test).
   Default bleibt 0,04 (keine Verhaltensänderung ohne Flag).
2. In-App-Timing in `08_app.rs`: kumulierte Physikzeit + Wanduhr, beim
   Smoke-Exit (`--frames`) Ø ms/Frame, Ø Physik-ms/Frame, effektive FPS.
3. Blockgrößen-Experiment (temporär per sed): 128/256/512 bei N=65k,
   Winner bleibt (launch_bounds + Kontrakte konsistent halten).

### 3.2 Sweep-Matrix (alle mit Headless-PASS als Stabilitätsgate)

- N ∈ {16k, 32k, 65k, 98k, 131k, 262k}, je h ∈ {h*·{0,75; 1,0; 1,5},
  0,04 als Anker}, 300 Schritte; Finalisten mit 500 Schritten bestätigen.
- Falls h* instabil: dt ∈ {0,0008; 0,0004} als zweite Achse (nur dort).
- GUI-FPS für Top-Kandidaten: In-App-Ø + Wanduhr-Kontrolle.

### 3.3 Entscheidung

- Max-N bei 60 FPS definieren als: größtes N mit stabilem (h, dt) und
  gemessenem Ø-Frame ≤ 16,67 ms (GUI) bzw. Physik-Prognose + Renderanteil.
- Falls eine robuste Auto-Regel (z. B. h = 0,04·√(16384/N), geklemmt)
  überall stabil und schneller ist: als Opt-in (`--auto-h`) einbauen,
  sonst nur dokumentieren.

## 4. Risiken

| Risiko | Maßnahme |
|---|---|
| Kleines h instabil (CFL/Dichteband) | dt-Achse, Stufen-Sweep, ehrlicher Negativbefund |
| Render dominiert GUI-FPS (X11/llvmpipe) | Physik/GUI trennen, beide Zahlen berichten |
| Blockgröße ≠ 256 bricht launch_bounds | Konsistent ändern, nach Experiment reverten/fixieren |
| Tests mit h-Annahme | Nur `02_params`-Defaults betroffen (bleiben), Sweep ist manuell |

## 5. Validierung

Gates wie bisher (fmt, beide clippy, 25+ Tests), dazu: `--h`-Parse-Test,
Headless-PASS jedes Sweep-Punkts, GUI-Smoke der Finalisten, Sweep-Tabellen
im Walkthrough.
