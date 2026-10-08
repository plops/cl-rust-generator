# Plan: Robuste Zustandsgleichung + Wasser-Visualisierung (01_better_water)

Datum: 2026-10-08. Basis: Stand nach 03_hscale (Default h=0,04, dt=0,0008,
k=2000, μ=0,1, Wanddämpfung 0,5, 3 Sub-Steps, 26 Tests grün).

Auftrag aus `prompt.txt`: `review.md` (Zustandsgleichung robuster) und
`beauty.md` (Visualisierung optimieren) umsetzen. Die Perf-Boilerplate aus
`prompt.txt` (Pufferlayout, Kernel-Ablauf) passt nicht zu dieser Aufgabe
und wird adaptiert: keine Layoutänderung, dafür Kraftbilanz- und
Render-Diagramme im Walkthrough.

## 1. Diagnose (Baseline-Messung, 500 Schritte, GPU)

| N | ms/Schritt | MIUPS | ρ̄ | ρmin | \|v\|max | Status |
|---|---|---|---|---|---|---|
| 2.048 | 0,0589 | 34,76 | 606,8 | 237,8 | 12,00 | PASS |
| 16.384 | 0,2848 | 57,52 | 630,3 | 29,7 | 11,90 | PASS |
| 65.536 | 1,7313 | 37,85 | 814,4 | 7,4 | 12,00 | PASS |
| 262.144 | 21,2181 | 12,35 | 815,1 | 2,4 | 11,87 | PASS |

Befund, konsistent mit `review.md`:

- `|v|max` klebt überall am 12-m/s-Cap → explosive Druckstöße (CFL:
  dt=0,0008 ist ~5× über dt_CFL≈0,00017 bei k=2000, h=0,04).
- ρ̄ weit unter ρ₀=1000, ρmin bis 2,4 → Fragmentierung; Unterdichte wird
  auf P=0 geklemmt, keine Kohäsion, Rand-Katapult (review.md §1, §4).
- Punkte statt Fläche (review.md §5): harte `draw_rectangle`-Sprites,
  technische Farbpalette.

## 2. Physik: linearer EOS mit begrenztem Unterdruck

### 2.1 Entscheidung (Optionen aus review.md §B)

- **Gewählt: linearer EOS + Tension-Clamp.**
  `P = k·(ρ−ρ₀)`, unten geklemmt auf `−TENSION_RATIO·k·ρ₀` mit
  `TENSION_RATIO = 0,1` (Startwert, Sensitivität 0,05/0,2 wird vermessen).
  Unterdruck wirkt über den bestehenden symmetrischen Druckterm
  `−ρᵢ·m·(Pᵢ/ρᵢ²+Pⱼ/ρⱼ²)·∇W` als Anziehung → Kohäsion ohne neuen Kernel,
  CPU/GPU automatisch konsistent (eine Funktion in `03_sph_math.rs`).
- **Verworfen: Tait P=B·((ρ/ρ₀)⁷−1).** Bei gleicher Schallgeschwindigkeit
  ist Tait-7 unter Kompression deutlich steifer (ρ=1,5ρ₀: ~4,6× des
  linearen Drucks) → verschärft die CFL-Verletzung statt sie zu heilen.
  Linear hält c=√k konstant und berechenbar.
- **Zurückgestellt: separater Akinci-2013-Kohäsionskernel**
  (DeepWiki-verifiziert: `C(r)=32/(πh⁹)·(h−r)³r³` mit repulsivem
  Innenast `−h⁶/64`, Kraft `−γ·m²·C(r)·q̂`). Er brächte einen neuen
  Abstimmparameter γ und Stabilitätsrisiko ohne visuelle Verifikation im Container
  (Screenshots schwarz, vgl. 03_hscale). Als dokumentierte Folgearbeit
  im Walkthrough; der Tension-Clamp liefert die Kohäsion mit einer
  Konstante statt neuem Kraftpfad.
- **NaN-Robustheit:** `pressure(NaN)` gab bisher 0 (f32::max-Semantik),
  gäbe neu −cap (Phantom-Anziehung). Explizite `is_nan`-Wache → 0.
  GPU-Codegen-Risiko klein (fcmp uno via LLVM); verifiziert durch
  `cargo oxide run`. Fallback: `density != density` mit allow(eq_op).

### 2.2 Stabile Defaults (02_params.rs, nur Werte + Kommentare)

| Parameter | alt | neu | Begründung |
|---|---|---|---|
| dt | 0,0008 | **0,0004** | Halbierung heilte bereits 262k/h=0,01 (03_hscale); Rest-CFL über Tension-Dämpfung + Cap |
| substeps (SimConfig + Cli) | 3 | **6** | gleiche Sim-Geschwindigkeit 0,0024 s/Frame |
| wall_damping | 0,5 | **0,2** | review.md B3: Zusammenlaufen statt Flummi |
| h / k / μ | 0,04/2000/0,1 | unverändert | h(N)-Tabelle aus 03_hscale bleibt gültig |

Erwartung: gleiche ms/Schritt (identische Kernelschleifen + ein max),
bessere Qualität: ρ̄↑, ρmin↑, \|v\|max vom Cap weg. Falls ein
Sweep-Punkt FAIL: dt-Achse (0,0002) nur dort, kein globaler Rollback.

### 2.3 Tests (langlebig, Stil wie bisher)

- Updaten: `druck_klemmt_unterdichte_auf_null` (neu: Tension-Verhalten),
  `druck_waechst_linear_mit_ueberdichte` (Schranke ≥ −cap statt ≥ 0),
  `defaults_entsprechen_feature_set_1` (neue Defaults), Renderer-Farbtest.
- Neu: Kohäsions-Regressionstest (2 Partikel, g=0, Abstand schrumpft;
  **fällt auf altem Code**, da P=0 keine Kraft liefert) in
  `tests/cpu_stability.rs`.
- Neu: Unit-Test NaN→0, Tension-Monotonie; Palette-/Foam-Gültigkeit.

## 3. Visualisierung (beauty.md, Stufen 1+2+§3, ohne Shader-Pipeline)

Begründet ohne Metaball-Render-Target: im Container nicht visuell
verifizierbar (indirektes GL, schwarze Captures); Design mit
DeepWiki-verifiziertem macroquad-Muster (`render_target` + `Camera2D` +
`load_material`/`_ScreenTexture` + Threshold-Shader) als Folgearbeit im
Walkthrough. Umgesetzt:

1. **Wasserpalette** statt Wärmebild: tief → türkis → Gischtweiß über
   |v|/6; Dichtemodus aquatisch (tief → cyan → weiß über ρ/ρ₀).
2. **Weiche Kreise** statt harter Rechtecke: Radius aus echtem
   Partikelabstand (`0,75·s·scale`, geklemmt 1,5–8 px), Alpha ~0,86 →
   Überlappung verschmilzt optisch (Pseudo-Metaball ohne Shader).
3. **Gischt:** ρ<0,7ρ₀ oder vy>2,5 → klein, weiß, deckend.
4. **Trails:** halbtransparentes Fade-Rechteck statt hartem Clear
   (Taste T toggelt, Default an), kurze Schweife, HUD bleibt lesbar
   (wird jeden Frame frisch darüber gezeichnet).
5. **Becken-Styling:** massiver Doppelrahmen, Hindernis-Schatten +
   Glanzpunkt.

Moduldisziplin: Farben/Stil nach `07a_water_style.rs` (Präzedenz 05a),
`07_renderer.rs` bleibt Kompositor; beide < 300 Zeilen. `08_app.rs`:
`particle_r`-Verdrahtung + T-Taste (~10 Zeilen).

## 4. Risiken

| Risiko | Maßnahme |
|---|---|
| Tension → Klumpen-Instabilität | Cap 10 %, Sweep-Gate PASS + ρ-Statistik; Sensitivität 0,05/0,2 |
| `is_nan` im Device-Code | Oxide-Build als Gate; Fallback-Vergleich bereit |
| Trails schmieren (Swap-Puffer) | Toggle T; Default-Entscheid im Smoke-Test begründet |
| Kreise verteuern Present bei 262k | Messen (Draw/Present-Split existiert); 60-FPS-Urteil nur bis ~24k relevant |
| Datei-Längen | Split 07/07a; 02_params (369, vorbestehend) nicht weiter aufblähen |

## 5. Validierung

Gates: `cargo fmt --check`, beide clippy (`-D warnings`), `cargo test`
(26+ neu). Headless-PASS 500 Schritte (Default-N). Vorher/Nachher-Sweep
N∈{2048, 16384, 65536, 262144} (ms/Schritt, MIUPS, ρ̄/ρmin/vmax).
GUI-Smoke `--frames 120` auf DISPLAY=:0 (flüssig, Trails-Toggle ok).
Umgebung: GPU-Clippy/Build braucht extrahiertes libclang
(`/tmp/clangroot` + `LIBCLANG_PATH`/`BINDGEN_EXTRA_CLANG_ARGS`, siehe
`/tmp/gpu_env.sh`), da die Platte für apt voll ist.
