# Plan Effort & Aufwandsanalyse (`20261029_02_insar`)

## 1. Geschätzter vs. Tatsächlicher Aufwand pro Phase

| Phase | Beschreibung | Geschätzter Aufwand | Tatsächlicher Aufwand | Status | Wichtigstes Ergebnis |
| :--- | :--- | :--- | :--- | :--- | :--- |
| **Phase 0: Daten-Inspektion & DeepWiki** | Entpacken der beiden Süd-Taiwan `S1C_IW_RAW`-Archive in `data/insar/`, Python-Orbit-/Burst-Spike, DeepWiki-Recherche zu TOPS-InSAR | ~10 Min. / 4–6 Turns | ~12 Min. / 5 Turns | Abgeschlossen | 1:1-Burst-Synchronisation ($\Delta t \approx 0{,}78\text{ ms}$), 36 Bursts über IW1–IW3, kompakte 3D-Basislinie ($\sim 17\text{ m}$) bestätigt |
| **Phase 1: Architektur & Planung** | Erstellung von `plan/20261029_02_insar/plan.md` auf Deutsch mit Mermaid-Pipeline-Diagramm und mathematischer Herleitung | ~10 Min. / 2–3 Turns | ~10 Min. / 3 Turns | Abgeschlossen | Vollständige Spezifikation von Baseband-TOPS-Fokussierung, 3D-WGS84-Geolokation, ESD, Goldstein-Filter und Phase Unwrapping |
| **Phase 2: GPU/CPU InSAR-Implementierung** | Implementierung von `11_insar.rs`, Erweiterung von `01_types.rs`, `03_ephem.rs`, `06_rda.rs`, `09_gpu.rs` (`#[cuda_oxide::kernel]`) und `main.rs` | ~25 Min. / 8–10 Turns | ~30 Min. / 9 Turns | Abgeschlossen | Funktionsfähige CPU- und GPU-Kernels (`tops_insar_phase_comp_inplace`, `insar_resample_and_ifg_kernel`) für `sm_86` |
| **Phase 3: Koregistrierungs-Debugging & 12-Burst-GPU-Lauf** | Diagnose und Behebung der Zero-Doppler-Orthogonalisierung (`-55 px`), des Right-Looking-Vorzeichens (`+6 px`) und des Baseband-Resamplings; Voll-Lauf über 72 SLC-Bursts | ~20 Min. / 6–8 Turns | ~45 Min. / 8 Haupt-Turns (+ 1 Sub-Agent) | Abgeschlossen | $B_{3\text{D}} = 17{,}00\text{ m}$, $B_\perp = -5{,}68\text{ m}$, P90-Kohärenz $0{,}7763$ (gefiltert), Gesamtlaufzeit $129{,}39\text{ s}$ auf RTX A4000 |
| **Phase 4: Tests, Panel-Downsampling & Qualität** | 4 neue InSAR-Integrationstests in `tests/integration.rs`, Downsampling des 2×2-Panels auf $< 10\text{ MB}$, `cargo fmt`, `cargo clippy`, `cargo oxide test` | ~15 Min. / 4–5 Turns | ~18 Min. / 3 Haupt-Turns (+ 1 Sub-Agent) | Abgeschlossen | 10/10 Integrationstests grün, 0 Clippy-Warnungen, 0 Format-Diffs, visuelle Prüfung von `taiwan_insar_insar_panel.png` |
| **Phase 5: Dokumentation & Abschluss** | Erstellung von `walkthrough.md` und `plan_effort.md` auf Deutsch, Kopieren der Artefakte, Bereinigung temporärer Dateien | ~10 Min. / 3–4 Turns | ~10 Min. / 4 Turns | Abgeschlossen | Vollständige deutsche Dokumentation mit eingebetteten Ergebnisbildern und sauberem Git-Arbeitsbaum |

---

## 2. Token-/Turn-Einsatz & Kontext-Kompaktierung durch Sub-Agenten

Um das Hauptkontextfenster trotz umfangreicher mathematischer Analysen, Compiler-Durchläufe (`cargo oxide build/test -- -j 1 --release`) und 72-Burst-GPU-Läufe kompakt und reaktionsschnell zu halten, wurde ab Phase 3 konsequent eine **Sub-Agenten-Delegationsstrategie (`task` mit `general-purpose`)** eingesetzt:

1. **Sub-Agent 1 (`insar_coreg_and_run` in Phase 3)**:
   - Übernahm isoliert in einem eigenen Kontextfenster die Feinanalyse des Vorzeichenfehlers im Cross-Track-Vektor ($\hat{\mathbf{u}}_v \times \hat{\mathbf{u}}_r$ für Right-Looking SAR), die Polynom-Orbitschätzung 4. Grades (`eval_smooth_orbit_poly`), die Entfernung des quadratischen Post-IFFT-Re-Rampings vor der Slave-Interpolation sowie den vollständigen 12-Burst-GPU-Lauf (`129,39 s`).
   - Dadurch blieben sämtliche Zwischen-Logs, Compiler-Ausgaben und iterativen Kohärenz-Messungen außerhalb des Hauptkontexts; zurückgegeben wurde nur die kompakte Zusammenfassung mit den finalen Kennzahlen.
2. **Sub-Agent 2 (`insar_tests_and_panel` in Phase 4)**:
   - Implementierte die 4 neuen synthetischen und realen InSAR-Integrationstests in `source2/tests/integration.rs`, skalierte `taiwan_insar_insar_panel.png` für die direkte Bildinspektion auf `2932 × 2526 px` (`7,45 MiB`) herunter und führte `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings` sowie `cargo oxide test -- -j 1 --release` aus.
3. **Sub-Agent 3 (Phase 5 Dokumentation & Cleanup)**:
   - Erstellte `walkthrough.md` und `plan_effort.md`, kopierte die Ergebnis-PNGs und `taiwan_insar_insar_summary.json` nach `plan/20261029_02_insar/` und bereinigte temporäre Test-Verzeichnisse.

---

## 3. Wichtigste Kostentreiber & Lessons Learned

### Wichtigste Kostentreiber
1. **72-Burst TOPS-Fokussierung mit $8\times$ Azimut-Zero-Padding ($N_{\text{fft}} = 16\,384$)**:
   - Für ein 3-Swath-Interferogramm müssen insgesamt $2 \times 36 = 72$ Bursts à $\sim 1\,450 \times 24\,000$ Samples fokussiert werden. Auf der GPU (NVIDIA RTX A4000) benötigt der gesamte End-to-End-Lauf inkl. Level-0-FDBAQ-Dekodierung, Sub-Pixel-Feinsuche, Goldstein-Filterung und 2D-Phase-Unwrapping rund **$129{,}4\text{ s}$** (~$1{,}8\text{ s}$ pro Burst).
   - Für schnelle Entwicklungs- und Debugging-Zyklen erwies sich der CLI-Parameter `--bursts 5,7` (2 Bursts statt 12, Laufzeit $\sim 18\text{ s}$) als entscheidender Hebel zur Reduzierung der Turn-Latenz.
2. **Sequentielles PTX-Linking (`cargo oxide ... -- -j 1`)**:
   - Da `rustc_codegen_cuda` beim parallelen Linken mehrerer PTX-Artefakte deterministisch `-j 1` benötigt, dauert jeder Re-Compile im Release-Modus ~15–25 Sekunden. Durch Bündelung von Code-Änderungen vor dem Build konnten unnötige Kompilierzyklen vermieden werden.

### Technisch-Physikalische Lessons Learned
1. **Exakte Orthogonalität der Zero-Doppler-Ebene im ECEF-System**:
   - Schon die kleine Abweichung ($\sim 0{,}1^\circ$) zwischen geozentrischem Radiusvektor $\mathbf{P}$ und der Senkrechten auf den Geschwindigkeitsvektor $\mathbf{V}$ infolge der Erdabplattung und Bahnexzentrizität erzeugt in 850 km Entfernung einen Along-Track-Projektionsfehler von $\sim 800\text{ m}$ ($\approx 55\text{ Azimut-Pixel}$). Erst die explizite Orthogonalisierung $\mathbf{r}_\perp = \mathbf{P} - (\mathbf{P}\cdot\hat{\mathbf{u}}_v)\hat{\mathbf{u}}_v$ macht die 3D-WGS84-Zero-Doppler-Geolokation sub-pixelgenau.
2. **Rechtsschauende (Right-Looking) Kreuzprodukt-Konvention**:
   - Ein Vorzeichenfehler im Cross-Track-Vektor ($\hat{\mathbf{u}}_r \times \hat{\mathbf{u}}_v$ statt $\hat{\mathbf{u}}_v \times \hat{\mathbf{u}}_r$) spiegelt das Ziel auf die linke Schwadseite und kehrt das Vorzeichen des Slant-Range-Offsets ($\Delta r \approx -6\text{ px}$ statt $+6\text{ px}$) um. Ein expliziter Geometrie-Unit-Test (`vel.cross(pos).dot(los) > 0.0`) verhindert solche Vorzeichenfehler dauerhaft.
3. **TOPS-Interferometrie erfordert Baseband-Resampling**:
   - Anders als bei reinen Intensitätsbildern (`|SLC|^2`) darf bei TOPS-InSAR vor der Sub-Pixel-Interpolation des Slave-SLCs kein quadratisches Phasen-Re-Ramping erfolgen; Master und Slave müssen mit identischer Steering-Rate $k_t$ ins Zero-Doppler-Basisband fokussiert und dort interpoliert werden.
