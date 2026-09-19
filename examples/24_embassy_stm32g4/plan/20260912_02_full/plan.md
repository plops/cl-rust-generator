# Implementierungsplan — 20260912_02_full
Rest-Spec (§3 Modi A–E, §2 Exklusiv-Modell + RAM-Sharing, §1+§5 PC-TUI)

Auftrag: Wol Pumba (wolpumba@gmail.com) · Board: WeAct STM32G474CEU6 · Framework: Embassy (`no_std`, async)
Spec: `plan/20260912_00_what/spec.md` · Vorgaenger: `plan/20260912_01_init/{plan,task,walkthrough}.md`
Code: `examples/24_embassy_stm32g4/fw/` (Workspace: `firmware`, `common`, `host-smoke`)
Dieser Prompt: `plan/20260912_02_full/prompt.txt` · Tasks: `plan/20260912_02_full/task.md`

Stand (validiert, nicht neu schreiben): USB-CDC + UART (USART1 PA9/PA10, 115200 8N1),
First-Byte-Dispatch (`\n`/`\r` → Text, `0x00` → postcard+COBS), `common` mit 17 Unit-Tests,
`host-smoke`-Vertragstest gruen auf beiden Transporten (s. 01_init `walkthrough.md`).

## 1. Was ist gefragt (Checkliste aus dem Prompt)

1. Modi A–E aus Spec §3 als exklusive Embassy-Tasks (Cancellation per Drop, HW-De-Init, SRAM1-Puffer freigeben).
2. Reihenfolge: zuerst Modus E (Frequenzzaehler, kleiner RAM, gut testbar), dann A/C/B/D, zuletzt TUI.
3. Steuerung vom bestehenden Control-Task ueber bestehendes Protokoll: `HostCmd`/`DeviceResp` in
   `common` um Modus-Befehle + Block-Transfers erweitern, `PROTO_VER` bei inkompatiblen Aenderungen bumpen.
4. Snapshot-Transfer (kein Live-Streaming), `common`-Crate bleibt `no_std`-faehig.
5. Datei-Aufteilung: >~300 Zeilen oder >1 Zustaendigkeit → splitten; neue Dateien `NN_name.rs`
   in Initialisierungs-/Datenfluss-Reihenfolge; `mod.rs`/`lib.rs`/`main.rs` nur Verdrahtung.
6. Tooling: `cargo fmt`, `cargo clippy -D warnings`, neueste Deps (`cargo upgrade`), neue Deps in `doc/deps.md`.
7. DeepWiki-Nutzung (Repo-Angaben in `doc/deps.md`-Notation) + Usage-Examples vorab in diesem Plan.
8. Pro Modus HIL-Nachweis auf echter Hardware (host-smoke-Erweiterung o. Modus-Check mit plausiblen Werten).
9. Neue Unit-/Integration-Tests wie erforderlich, ausfuehren, gruen.
10. Deliverables: dieser Plan, `task.md` (serielle Tasks mit Test-Gates), am Ende
    `plan/20260912_02_full/walkthrough.md` + Docker-Paketliste.

## 2. Architektur (Zielbild)

```
firmware/ (thumbv7em, no_std)              common/ (no_std, host+fw)          host/ (std)
01_clock.rs (HSI48+CRS, spaeter PLL)       01_types.rs (HostCmd/DeviceResp)   host-smoke/ (Vertragstest, bleibt)
02_usb_cdc.rs (Builder, Klassen-Task)      02_frame.rs (postcard+COBS)        tui/ (NEU, ratatui, siehe Kap. 7)
03_transport.rs (USB+UART serve, Router)    03_router.rs (Dispatch, besteht)   └── 01_app.rs, 02_widgets.rs, …
04_control.rs (Modus-Manager, exklusiv)    04_modes.rs (NEU: Modus-Typen)
05_mode_*.rs (je ein Task A–E)             05_blocks.rs (NEU: Block-Transfer)
06_acquire.rs (DMA/ADC-Helfer)             PROTO_VER 1 → 2 (inkompatibel!)
main.rs (nur init, spawn, join)
```

- Exklusivitaet: `04_control.rs` haelt genau einen Mess-Task (Embassy-`Spawner` + `JoinHandle`:
  neuen Modus starten = alten Handle droppen → Destructor laeuft → HW-De-Init + Pufferfreigabe).
  DeepWiki-Bestaetigung: `embassy-executor` Tasks sind per Drop des Handles abbrechbar, Koordination
  ueber `embassy-sync::{Signal,Channel}` (Details Kap. 8).
- RAM: CCM (Code/Stack/Executor/IRQs, vom Linker via `memory-x`), SRAM1 als *ein* statischer
  80-KB-Pool (`StaticCell`), den der aktive Modus exklusiv als DMA-/Sample-Puffer nutzt
  (bis ~40.000 u16-Samples). Kein zweiter Modus allokiert je gleichzeitig.
- Transport unveraendert: CDC-Blocktransfer in 64-B-Paketen, `MAX_FRAME` bleibt 128 B auf dem Geraet;
  grosse Snapshots werden in `05_blocks.rs`-Chunks (Sequenznummer + CRC) zerlegt (Kap. 5).

## 3. Protokoll-Erweiterung (`common`, PROTO_VER 1 → 2)

Bestehendes bleibt (Ping/GetVer/Echo, Text HELP/PING/GET VER/GET UID). Neu:

```rust
pub enum HostCmd {
    Ping, GetVer, Echo(heapless::Vec<u8, 64>),   // Bestand
    ModeStop,                                     // Mess-Task stoppen → Idle
    FreqStart { level_mv: u16, hyst: u8, filter: u8, gate_ms: u32 },
    FreqRead,                                     // letztes Ergebnis abholen
    ScopeStart { rate: u8, level_mv: u16 }, ScopeRead { off: u32, len: u16 },
    AwgLoad { off: u32, len: u16 }, AwgStart { freq_hz: u32 },
    CapStart { pin: u8 }, CapRead,
    VnaStart { f0_hz: u32, f1_hz: u32, points: u16 }, VnaRead { off: u32, len: u16 },
    BlockAck { seq: u16 },                        // Flusskontrolle Download
}
pub enum DeviceResp {
    Pong, Ver { proto: u8, fw: heapless::String<16> }, Echo(..), Err { code: u8 }, // Bestand
    ModeOk { mode: u8 }, ModeBusy, ModeIdle,
    Freq { hz: u32, counts: u32, gate_ms: u32 },
    Block { seq: u16, total: u16, data: heapless::Vec<u8, 96> }, // 96 damit Frame ≤128 B
    BlockEnd { total: u16, crc: u16 },
}
```

- Text-Seite: `MODE E START …`, `MODE STOP`, `GET FREQ` als duenne Aliase (TUI nutzt binaer).
- `err::` neu: `MODE_BUSY=5`, `NO_DATA=6`, `BAD_ARG=7`; `PROTO_MISMATCH=4` (besteht) wird bei
  `proto != PROTO_VER` aktiv geantwortet.
- Kompatibilitaet: Bump auf 2, weil alte Hosts die neuen Varianten nicht decodieren.
  `host-smoke` prueft `Ver{proto:2}` + alte Befehle weiter (Rueckwaerts-Test).

## 4. Modi-Reihenfolge und Hardware-Mapping (mit HIL-Idee je Modus)

- **E Frequenzzaehler (zuerst):** COMP2 (Signal, z.B. PA7) + DAC3-Trigger-Level (0–3,3 V) +
  Hysterese; COMP-Ausgang → TIM2-Zaehleingang mit Digitalfilter; TIM6 = Torzeit.
  HIL: Funktionsgenerator (oder HRTIM-Testton) auf FT-Pin → host-smoke `FreqStart/FreqRead`,
  plausibel ±Quarztoleranz. Kleinster RAM, kein DMA.
- **C AWG (zweitens, Quelle fuer B/E-Tests):** LUT in SRAM1 → DMA zyklisch → DAC1 → OPAMP1
  (High-Speed, 45 V/µs). HIL: DAC-Pin mit Scope/ADC ruecklesen (Loopback auf Scope-Pin).
- **A Oszilloskop (drittens):** ADC1..4 interleaved (4×4 MSPS=16 MSPS) → DMA circular → SRAM1
  → Block-Transfer; ETS spaeter: COMP1-Trigger + HRTIM-Delay (184-ps-Schritte) + Burst-DMA.
  HIL: AWG-Loopback (C→A) Rechteck/Sinus, Samplezahl + Amplitude pruefen.
- **B VNA (viertens):** HRTIM-taktet DAC1/OPAMP1-Sinus → DUT; phasenversetzte ADC-Trigger
  (0–360°), Daempfung/Phase aus Amplitudendifferenz + Maximum-Lage. HIL: Kurzschluss/Durchgang
  als DUT (0 dB/0°), dann RC-Glied (plausible Kurve).
- **D Kapazitaet (fuenftens):** Pin Push-Pull-High → analog, Entladung via Pull-Down, COMP-Schwelle,
  TIM2-Input-Capture misst Zeit; TDM ueber ≥3 Pins. HIL: bekannte Kondensatoren (z.B. 100 pF/1 nF),
  Zeitverhaeltnis ~ Kapazitaetsverhaeltnis.
- **TUI (zuletzt):** neues `tui`-Crate (ratatui + serialport + postcard, `common`-Vertrag),
  Screens pro Modus + Block-Download-Anzeige; kein Logik-Duplikat (nutzt `common`).

Pin-Regel (Spec §4, Pflicht in jedem Modus-Task dokumentieren): analoge Eingaenge ≤3,3 V
(TT_a bis 3,6 V, abs. max. 4,0 V — 1 kΩ Serienwiderstand), Zaehler-Eingang auf 5-V-toleranten
FT-Pin. Konkrete Pins erst nach Abgleich PCB-Dok vs. CubeMX/Datenblatt fixieren (Task F0).

## 5. Block-Transfer (Snapshot-Prinzip, USB-FS-tauglich)

12 Mbps FS + 64-B-Pakete ⇒ kein Streaming. Protokoll: `ScopeRead/VnaRead{off,len}` →
`Block{seq,total,data[≤96 B]}` … → `BlockEnd{total,crc}`. Host quittiert optional per
`BlockAck{seq}` (Stop-and-Wait fuer langsame Links; Default: Fenster ohne Ack, Retry via
erneutem `Read`). 96-B-Nutzdaten halten den COBS-Frame unter `MAX_FRAME=128`.

## 6. Fehlende Requirements — Rueckfragen + Vorschlaege

Prompt §„beruecksichtige dabei" nennt 5 Punkte; dazu 7 eigene Vorschlaege (Status: Vorschlag,
bedarf Bestaetigung von Wol Pumba):

Geforderte (vom Plan abgedeckt, Entscheidung notiert):
1. **Messgenauigkeit/Kalibrierung:** VDDA-Referenz (intern messen + per TUI anzeigen),
   Quarztoleranz (HSI48+CRS ist USB-genau; bei spaeterem PLL/HSE: Ist-Frequenz per MCO messen).
   Kalibrierwerte fluechtig (RAM), kein Flash-Schreiben in dieser Phase.
2. **Schutzbeschaltung/Limits:** Software-Limits (DAC-Level ≤3300 mV clampen + `BAD_ARG`),
   Doku-Warnhinweis pro Modus; keine aktive Schutzschaltung (Barebone per Spec §4).
3. **TUI-Protokoll-Versionierung:** `PROTO_VER`-Handshake (`PROTO_MISMATCH`), TUI zeigt
   Update-Hinweis statt abzustuerzen.
4. **Modus-Wechsel waehrend Messung:** neuer Befehl bricht alten Task ab (Drop), Antwort
   `ModeOk`; laufender Block-Transfer wird mit `ModeBusy`-Abbruch + neuem `ModeOk` beendet.
5. **Persistente Konfiguration:** Vorschlag: **kein Flash-Speicher in dieser Phase**
   (Schreibzyklen, Dual-Bank-Komplexitaet); TUI speichert Profile host-seitig (TOML).

Eigene Vorschlaege (noch nicht bestaetigt):
6. TUI-Framework: **ratatui** (rein Rust, passt zu serialport/postcard) — Alternative `egui` nur bei GUI-Wunsch.
7. Fehler-/Panic-Policy: `panic-probe` + Reset beibehalten, Watchdog (IWDG) erst mit TUI-Phase (Task T5).
8. Self-Test `SELFTEST\n` (Text) + `HostCmd::SelfTest`: meldet erkannte Peripherie/Clock ohne Messaufbau.
9. Sampling-Default: RTS zuerst, ETS als separater Task danach (Risiko-Entkopplung HRTIM-Burst-DMA).
10. AWG-Spannungsgrenze in Software (0–3,3 V) + Doku, keine negativen/ueber-VDDA-Werte.
11. VNA braucht DUT-Definition + Kalibrierlauf (Open/Short/Through) — sonst keine absoluten dB.
12. Host-Seite: `host-smoke` bleibt Vertragstest (Exit-Code), TUI nutzt es als Bibliothek, kein Fork.

## 7. Abhaengigkeiten (neu, in `doc/deps.md` nachtragen, jeweils neueste nehmen)

| Crate | Org/Projekt | Zweck |
|---|---|---|
| ratatui | ratatui/ratatui | TUI-Screens (host) |
| crossterm | crossterm-rs/crossterm | TUI-Backend (host) |
| serialport | serialport/serialport | besteht (host-smoke + TUI) |
| clap | clap-rs/clap | TUI-/Smoke-CLI-Args (host) |
| toml | toml-rs/toml | TUI-Profile (host) |
| micromath / libm | tac0turtle/micromath | Sinus-LUT ohne std/float (fw) |
| stm32-hrtim | stm32-rs/stm32-hrtim | HRTIM-Treiber via embassy (fw, transitives Dep-Pruefung) |

Embassy-Module (keine neuen Crates, nur Features/Pruefung): `comp`, `dac`, `opamp`, `adc`
(DMA), `hrtim`, `timer` (Input-Capture/Zaehler). Bestand bleibt: embassy-* 0.6/0.10/0.5/0.8/0.1,
postcard 1.1.3 + heapless **0.7** (nicht upgraden — postcard pinnt 0.7, s. 01-Walkthrough),
serde, static_cell, defmt.

## 8. Usage-Examples (DeepWiki-verifiziert + lokal bestaetigt)

COMP (G474, lokales Beispiel `examples/stm32g474/src/bin/comp.rs` — kanonisch, nicht die
DeepWiki-Variante mit veralteter API):

```rust
use embassy_stm32::comp::{Comp, Config, InvertingInput};
bind_interrupts!(struct Irqs { COMP1_2_3 => comp::InterruptHandler<peripherals::COMP2>; });
let mut cfg = Config::default();
cfg.inverting_input = InvertingInput::Vref;   // spaeter: DAC-Ausgang als Schwelle
let mut comp2 = Comp::new(p.COMP2, p.PA7, Irqs, cfg);
comp2.enable();
comp2.wait_for_rising_edge().await;           // async Flanke (Modus D/E-Trigger)
```

ADC+DMA ( Embassy-Dok, fuer Modus A/B):

```rust
use embassy_stm32::adc::{Adc, AdcChannel, SampleTime};
let mut adc = Adc::new(p.ADC1);
adc.read(p.DMA1_CH2.reborrow(), Irqs,
    [(&mut *pin, SampleTime::CYCLES160_5)].into_iter(), &mut buf).await;
```

DAC (fuer Modus B/C; `DacChannel::new_blocking`, Sinus via LUT):

```rust
use embassy_stm32::dac::DacChannel;
let mut dac = DacChannel::new_blocking(p.DAC1, p.PA4);
dac.set(value);                               // AWG: per DMA zyklisch aus SRAM1-LUT
```

HRTIM (G474, lokales `examples/stm32g474/src/bin/hrtim.rs`; PLL→120 MHz→×32):

```rust
let Parts { control, tima, .. } = p.HRTIM1.hr_control();
let (control, ..) = control.wait_for_calibration();
let mut control = control.constrain();
let parts = tima.pwm_advanced(pin1, pin2).prescaler(Pscl4).period(0xFFFF); // Delay-Basis ETS/VNA
```

Exklusiv-Tasks (DeepWiki: `embassy-sync::Signal/Channel`, Drop bricht Task ab):

```rust
static CMD: Channel<CriticalSectionRawMutex, ModeReq, 1> = Channel::new();
// control-Task: let h = spawner.spawn(mode_e_task(ctx)); … drop(h) bei Modus-Wechsel
```

Referenzen: `embassy-rs/embassy` (embassy-stm32 `-comp/-dac/-adc/-hrtim/-timer`,
embassy-usb, embassy-sync), `jamesmunns/postcard` (bereits in `doc/deps.md`).

## 9. Risiken

- HRTIM-Burst-DMA (ETS) ist die fummeligste Stelle → als letzter Scope-Schritt, RTS zuerst.
- ADC-Interleaving 4×4 MSPS braucht exakte Trigger-Versatz-Konfiguration → Loopback-Test deckt es.
- `MAX_FRAME=128` begrenzt `Block.data` auf ~96 B → viele Chunks bei 40k Samples (~420 Bloecke);
  Stop-and-Wait nur bei Bedarf, sonst Fenster-Download.
- heapless/postcard-Versionen nicht „einfach upgraden" (0.7-Pinning); `cargo upgrade` nur fuer
  neue/host-seitige Deps.
- `probe-rs download` haelt Core an → immer `probe-rs run`/Reset (s. 01-Walkthrough Kap. 5).

## 10. Kontext fuer einen unabhaengigen Agenten (Pflichtlektuere)

1. `plan/20260912_00_what/spec.md` — Systemkonzept, Modi, RAM-Modell, Protokollvertrag.
2. `plan/20260912_01_init/walkthrough.md` — Stand, Learnings (heapless-Pinning, run-vs-download, mknod).
3. `plan/20260912_01_init/plan.md` Kap. 3–5 — Dispatch-Regel, Clock-Annahmen, Deps-Tabelle.
4. `plan/20260912_02_full/prompt.txt` — dieser Auftrag (Reihenfolge, HIL-Pflicht, Dateiregeln).
5. `plan/20260912_02_full/plan.md` (diese Datei) + `task.md` — was/wie zu tun ist.
6. `fw/common/src/{lib,router,frame,text}.rs` — Protokoll (nicht neu schreiben, erweitern).
7. `fw/firmware/src/main.rs` — Transport/Control (Basis fuer 01–06-Split).
8. `fw/host-smoke/src/main.rs` — Vertragstest (Muster fuer Modus-Checks).
9. `doc/stm32g474ceu6_pcb.md` — Pins (PA11/PA12 USB, PA9/PA10 UART, KEY PC13).
10. `doc/deps.md` — Dependency-Registry (hier Kap. 7 nachtragen).
11. `/workspace/src/embassy/examples/stm32g474/src/bin/{comp,hrtim,hrtim_master,pwm_input_async}.rs` — G474-Muster.
12. `/workspace/src/embassy/examples/stm32g4/src/bin/{adc_dma,dac_dma_circular,input_capture,usb_serial}.rs` — Peripherie-Muster.
13. `doc/stm32g474_datasheet.md` + `doc/stm32g474_programming_manual.md` — nur bei Pin-/Register-Zweifeln.
14. Live-Hardware: `probe-rs list`, `lsusb` (`c0de:cafe`), `mknod`-Regel aus 01-Walkthrough.

## 11. Commit-Konvention (fuer alle Tasks)

Format: Conventional Commits, ein logischer Schritt pro Commit:
`feat|fix|test|docs|chore|refactor(<scope>): <kurz, imperativ, ≤72 Zeichen>`.
Body: was + warum, betroffene Hardware/Peripherie, Validierung
(`cargo fmt --check`, `cargo clippy`, `cargo test`, `probe-rs run`-Ergebnis).
Footer: `Refs: plan/20260912_02_full/task.md <ID>`.
Beispiele: `feat(proto): mode commands with proto_ver 2`,
`feat(mode-e): comp+tim2 frequency counter`, `test(host): smoke freq start/read`,
`docs(plan): walkthrough for modes a-e and tui`.
Nie committen ohne gruene Gates des Tasks; keine fremden/untracked Dateien anfassen;
keine generierten Artefakte ausser `Cargo.lock` (Binaries: ja).
