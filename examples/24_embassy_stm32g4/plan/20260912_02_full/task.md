# task.md — 20260912_02_full: seriell abarbeitbare Schritte
Jeder Schritt endet mit Gates. Erst bei gruenen Gates committen (s. `plan.md` Kap. 11)
und zum naechsten Schritt. Code: `examples/24_embassy_stm32g4/fw/`.
Dateiregeln aus dem Prompt gelten ab der ersten angefassten Datei
(`NN_name.rs`, ≤~300 Zeilen, `main.rs`/`lib.rs` nur Verdrahtung, vorher/nachher gruen).

## F0 — Pin-/Clock-Fixierung + File-Split (Basis)
- PCB-Dok vs. Datenblatt: konkrete Pins je Modus festlegen (E: COMP2-IN + FT-Zaehler-Pin;
  A: ADC-Pins; B/C: DAC1/OPAMP1-Pins; D: ≥3 TDM-Pins; belegt: PA11/PA12 USB, PA9/PA10 UART,
  PC13 KEY). 170-MHz-PLL-Entscheidung (erst HSI, PLL spaeter mit MCO-Check).
- `main.rs` → `01_clock.rs`, `02_usb_cdc.rs`, `03_transport.rs`, `04_control.rs`
  (reiner Split, kein Verhalten); `common` → `01_types.rs`-Auszug nur falls angefasst.
- `doc/deps.md` um `plan.md` Kap. 7 ergaenzen.
- Gates: `cargo fmt --check`, `cargo clippy -p g474-common -p g474-host-smoke --all-targets -- -D warnings`,
  `cargo clippy --release -D warnings` (fw), `cargo test -p g474-common` (17/17),
  `host-smoke` auf USB+UART gruen, `probe-rs run` bootet (RTT).
- Commit: `chore(fw): split transport control modules`.

## F1 — Protokoll v2 in `common` (04_modes.rs + 05_blocks.rs)
- `HostCmd`/`DeviceResp` nach `plan.md` Kap. 3 erweitern, `PROTO_VER=2`,
  `err::{MODE_BUSY,NO_DATA,BAD_ARG}`; Text-Aliase (`MODE …`, `GET FREQ`).
- FIX 2026-09-12 (PROTO_VER=3): Binaer-Frames brauchen einen fuehrenden `0x00`-Marker
  (`encode_cmd`/`encode_resp`), der Router oeffnet Binaer-Modus nur auf leerer Zeile.
  Grund: COBS schliesst nur `0x00` aus — `0x0A`/`0x0D` im Payload (z.B. AwgStart(1000)
  = `[04,0A,E8,07,00]`) wurden als Text-\n gefressen (HIL-belegt, Router-Regressionstests).
- Unit-Tests: Roundtrip je neue Variante (1-Byte-Feed), COBS-Invariante,
  `Block`-Groessenbeweis (Frame ≤128 B), Router-Matrix mit Modus-Frames, PROTO_MISMATCH-Pfad.
- Gates: `cargo test -p g474-common` gruen (neu ≥10 Tests), `fmt`, `clippy`.
  Kein FW-Flash noetig (reine Host-Tests).
- Commit: `feat(proto): mode commands with proto_ver 2`.

## F2 — Control-Task exklusiv (04_control.rs + Signal/Channel)
- `ModeReq`-Channel, genau ein Mess-Task (Handle-Drop = Cancellation + De-Init),
  Antworten `ModeOk/ModeBusy/ModeIdle`, `ModeStop`, Block-Abbruch bei Wechsel.
- Host-Test des State-Modells (Mock-Task oder Logik-Test ohne HW).
- Gates: `fmt`, `clippy`, `cargo test -p g474-common` gruen, `probe-rs run` + smoke
  (alte Befehle) gruen.
- Commit: `feat(fw): exclusive mode control task`.

## E1 — Modus E implementieren (05_mode_freq.rs)
- COMP2 + DAC3-Level (0–3300 mV clamp, `BAD_ARG` sonst) + Hysterese, TIM2-Zaehler mit
  Digitalfilter, TIM6-Torzeit; Ergebnis `Freq{hz,counts,gate_ms}`; Schutz-Doku (§4).
- Unit-Tests: Arg-Clamping, Gate-Rechnung (counts/gate→Hz), Filter-/Hyst-Mapping.
- Gates: `fmt`, `clippy`, Host-Tests gruen.
- Commit: `feat(mode-e): comp tim2 frequency counter`.

## E2 — Modus E HIL + smoke
- `host-smoke`: `FreqStart/FreqRead` (Testton, z.B. AWG-Loopback oder Generator),
  Plausibilitaet ±Toleranz; danach `PING` (Recovery).
- Stand 2026-09-12 (partiell): smoke gruen USB+UART (SelfTest, FreqStart mit 200-ms-
  und 3-s-Gates, FreqRead, ModeStop, Text GET FREQ/MODE STOP); schwebendes PA7 zaehlt
  stabil 0 (Null-Nachweis, COMP+DAC-Init via SelfTest-Bit, Gate-Timing exakt).
  Kanten-Nachweis (counts>0) braucht ein Taktsignal auf PA7 (1 kOhm, max 3,3 V) und
  wird nach C2 per AWG-Loopback (PA5→PA7-Jumper) geschlossen.
- Gates: smoke auf Gerate-HW gruen (USB), RTT-Log ohne Panic, Messwert im Task-Kommentar.
- Commit: `test(host): smoke freq start read`.

## C1/C2 — Modus C AWG (06_awg.rs + LUT) + HIL-Loopback
- SRAM1-LUT → DMA → DAC1 → OPAMP1; `AwgLoad{off,len}` + `AwgStart{freq_hz}`;
  Software-Limit 0–3,3 V. HIL: Loopback auf Scope-Pin (oder DMM), Sinus/Rechteck erkannt.
- Stand 2026-09-12 (Stufe 1 gruen): TIM2-PWM-Rechteck auf PA5, `AwgStart{1000}` →
  `ModeOk(C)`, `AwgStart{0}` → `BAD_ARG`, `ModeStop` stoppt; smoke gruen USB+UART.
  LUT/DMA-Playback (Stufe 2) wartet auf Timer-getriggertes DAC-DMA (embassy 0.6.0
  kennt nur SOFTWARE-Trigger); `AwgLoad` → `NOT_IMPL`.
- Offen (1 Jumper): PA5→PA7 → `FreqStart` muss Tonfrequenz zaehlen (E-Kantennachweis);
  alternativ DMM/Scope an PA5 (0–3,3 V, 1 kHz).
- Gates analog E1/E2. Commit: `feat(mode-c): dma dac awg from sram1`.

## A1/A2 — Modus A Scope RTS (06_scope.rs) + HIL
- ADC1..4 interleaved → DMA circular → SRAM1 → `ScopeStart/ScopeRead/Block/BlockEnd`;
  HIL: C→A-Loopback, Samplezahl + Amplitude plausibel.
- Gates analog. Commit: `feat(mode-a): interleaved adc scope snapshots`.

## A3 — Modus A ETS (HRTIM-Delay, separater Schritt)
- COMP1-Trigger + HRTIM-Delay + Burst-DMA, erst nach stabilem RTS.
- HIL: periodisches Signal, effektive Aufloesung plausibel. Commit: `feat(mode-a): ets hrtim delay`.

## B1/B2 — Modus B VNA (06_vna.rs) + HIL
- HRTIM→DAC1/OPAMP1-Sinus, phasenversetzte ADC-Trigger, Daempfung/Phase;
  HIL: Through (0 dB/0°) + RC-Glied. Commit: `feat(mode-b): hrtim vna sweep`.

## D1/D2 — Modus D Kapazitaet (06_cap.rs) + HIL
- Charge→analog, COMP + TIM2-Input-Capture, TDM ≥3 Pins; HIL: 100 pF vs. 1 nF
  (Zeitverhaeltnis ~10×). Commit: `feat(mode-d): comp tim2 capacitance tdm`.

## T1 — TUI-Crate (ratatui)
- Neues `tui`-Crate (ratatui/crossterm/clap/toml + `common`), Screens je Modus,
  Block-Download, `PROTO_MISMATCH`-Hinweis, host-seitige TOML-Profile.
- Gates: `fmt`, `clippy`, `cargo test -p g474-tui` (Parser-/Block-Logik), Smoke-Paritaet.
- Commit: `feat(tui): ratatui screens per mode`.

## T2 — Dauerlauf + Haertung
- 60-s-Wechsel je Modus, Stecker-Reconnect, `ModeStop`-waehrend-Transfer,
  Text+Binär im Wechsel; Grenzen dokumentieren.
- Gates: alle Gates F0–T1 erneut gruen + Dauerlauf-Log. Commit: `fix(fw): mode switch hardening`.

## T3 — Upgrade + Deps + Abschluss
- `cargo upgrade` (nur neue/host-Deps; heapless 0.7-Pinning beachten), `doc/deps.md` final,
  `fmt`/`clippy`/`test` final gruen, `git log` sammeln.
- `plan/20260912_02_full/walkthrough.md` schreiben (implementiert vs. Plan, Abweichungen,
  Mess-Logs, Learnings, Erweiterungen, Docker-Pakete).
- Commit: `docs(plan): walkthrough for modes a-e and tui`.
