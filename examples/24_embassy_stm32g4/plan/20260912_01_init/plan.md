# Implementierungsplan — 20260912_01_init
Dual-Mode-Parser (Text + Binär) mit USB-CDC-Bring-up auf STM32G474CEU6

Auftrag: Wol Pumba (wolpumba@gmail.com) · Board: WeAct STM32G474CEU6 · Framework: Embassy (`no_std`, async)
Spec: `plan/20260912_00_what/spec.md` · Prompt dieses Schritts: `plan/20260912_01_init/prompt.txt`

## 1. Scope dieses Inits (bewusst klein)

Dieses Init implementiert **nur die Kommunikations-Grundlage**, keine Messmodi (Scope/VNA/AWG/Cap/Freq kommen später):

1. USB-CDC (virtueller COM-Port) Bring-up auf dem G474 (PA11/PA12, FS 12 Mbps).
2. First-Byte-Dispatch-Parser: Textkommandos (führender ASCII-Buchstabe, `\n`/`\r`-terminiert)
   plus Binär-Frames (nicht-Buchstabe als Start-Byte, Länge + CRC).
3. Geteiltes `common`-Crate (Host + Firmware) mit Nachrichten-Typen, postcard + COBS.
4. Host-seitiger Smoke-Test (Python/pyserial oder `picocom`), kein volles TUI.
5. `task.md` in diesem Ordner als seriell abarbeitbare Schrittfolge mit Test-Gates.
6. Validierung auf echter Hardware via STLink + `probe-rs` (im Container verifiziert vorhanden:
   `probe-rs 0.32.0`, STLink V2-1 an USB, `rustc 1.98.1`).

UART kommt später (nice-to-have, erst USB). Das volle TUI kommt später.

## 2. Architektur-Entscheidung

```
firmware/ (thumbv7em-none-eabi, no_std)     common/ (no_std-kompatibel, host+fw)     host-smoke/ (std, später TUI)
├── main.rs (Executor, USB-Task, LED)       ├── lib.rs (HostCmd/DeviceResp Enums)    └── kleine CLI / Python-Skript
├── usb_cdc.rs (Builder, Klassen-Task)      ├── text.rs (HELP/GET/PING Parser)       (Text senden, Binär senden,
├── proto_dispatch.rs (First-Byte-Router)   └── frame.rs (postcard+COBS helpers)      Antworten prüfen)
└── mode_stub.rs (Platzhalter Control-Task
    für spätere exklusive Modi, vgl. spec §2)
```

Neuer Ordner unterhalb von `examples/24_embassy_stm32g4/`: `fw/` als Cargo-Workspace
mit Membern `firmware`, `common`, (optional `host-smoke` als reines std-Bin).
Alternative (falls einfacher): ein Crate mit `src/bin/usb_proto.rs` nach Embassy-Beispielmuster.
Empfehlung: Workspace mit `common` von Anfang an — die Spec (§5) verlangt es, und es kostet kaum mehr.

Executor-Layout (Embassy): `usb.run()`-Task + `dispatch`-Task via `join` bzw. zwei
gespawte Tasks; später ersetzt `dispatch` den Mess-Task per Cancellation (spec §2, RAM-Sharing).

## 3. Kritische Design-Frage (offen, vor T3 entscheiden)

Der Prompt schlägt `[0x02][LEN]...[CRC][0x03]` vor, die Spec (§5) verlangt **postcard + COBS
mit `0x00`-Delimiter**. Beides gleichzeitig geht, aber der First-Byte-Dispatch braucht eine
präzise Regel, weil COBS-codierte Bytes **auch** ASCII-Buchstaben sein können:

- Empfehlung: Kanonisch ist **postcard + COBS** (`to_slice_cobs`, `CobsAccumulator`, `0x00` als Delimiter).
  Dispatch-Regel: Bytes akkumulieren; kommt `\n`/`\r` ohne vorheriges `0x00` → Textpfad;
  kommt `0x00` → COBS-Frame an `CobsAccumulator::feed` geben. البديل `0x02 … 0x03`-Wrapper nur als
  Legacy-Alias, nicht als zweites Format.
- Minimaler Textbefehlssatz für dieses Init: `H`/`HELP`, `PING`, `G`/`GET <key>` (z. B. `GET VER`,
  `GET UID`), Antwort immer `... \n`-terminiert. Unbekannt → `ERR UNKNOWN\n`.
- Binär: `HostCmd`-Enum (z. B. `Ping`, `GetVer`, `Echo{len,n}`), Antwort `DeviceResp`-Enum.
  Max. Frame 64 B (FS-Paketgröße), COBS-Puffer 128 B.

## 4. Clock-/Hardware-Annahmen (am Gerät zu validieren, T2)

- Referenz: Embassy `stm32g4/src/bin/usb_serial.rs` (G4-Familie, direkt übertragbar):
  `USB_LP => usb::InterruptHandler<peripherals::USB>`, `Driver::new(p.USB, p.PA12, p.PA11, Irqs)`,
  `CdcAcmClass::new(&mut builder, &mut state, 64)`, Deskriptor-Puffer 256/256/64.
- RCC: `hsi48 = Some(Hsi48Config { sync_from_usb: true })` (CRS trimmt HSI48 über USB-SOF),
  `mux.clk48sel = Clk48sel::Hsi48`, Systemclock per PLL (Boost-Mode). Das Beispiel nimmt
  8 MHz HSE an — **am WeAct-Board verifizieren** (Quarzbestückung), Fallback rein HSI48 wenn kein HSE.
- Chip-Feature: `embassy-stm32` mit `stm32g474ce` (nicht `re` — `re` war nur der probe-rs-Runner-Alias
  aus dem Prompt; `probe-rs run --chip STM32G474RETx` funktioniert für CE trotzdem, ggf. auf
  `STM32G474CEUx` umstellen). `memory-x`, `time-driver-any`, `tick-hz-32_768`, `exti`, `defmt`.
- USB-Identität vorerst: `VID 0xC0DE / PID 0xCAFE` (Embassy-Beispiel), später eigene IDs + Seriennummer.
- SB8/SB9-Lötbrücken und USB-C-Kabel (Daten, nicht nur Power) prüfen, wenn Enumeration fehlschlägt.

## 5. Abhängigkeiten (in `doc/deps.md` nachtragen, neueste Version nehmen)

| Crate | Org/Projekt | Version (Stand Sep 2026) | Zweck |
|---|---|---|---|
| embassy-stm32 | embassy-rs/embassy | 0.6.0 (path-Override auf `/workspace/src/embassy`) | HAL, USB-Driver, RCC |
| embassy-usb | embassy-rs/embassy | 0.6.0 | USB-Stack, CDC-ACM-Klasse |
| postcard | jamesmunns/postcard | 1.1.3 (+`use-std` nur hostseitig) | Binär-Serialisierung + COBS (`to_slice_cobs`, `CobsAccumulator`) |
| serde | serde-rs/serde | 1.0.229 (`derive`, `default-features=false`) | `Serialize/Deserialize` für `common` |
| heapless | rust-embedded/heapless | 0.9.3 (`default-features=false`) | `Vec`/Puffer ohne Alloc |
| static_cell | rust-embedded/static_cell | 2.0.0 | `'static`-Buffers für USB-Builder |
| defmt / defmt-rtt / panic-probe | knurling-rs/defmt u.a. | 1.0.1 / 1.0.0 / 1.0.0 | RTT-Logging (nicht über USB loggen!) |
| embassy-executor/time/futures/sync | embassy-rs/embassy | passend zu 0.6.0 | Executor, Timer, Join |

Host (`host-smoke`, std): `postcard` (+`use-std`), `serialport` o. `tokio-serial` — erst in T6 wählen.
Tooling im Container: `cargo-edit` (`cargo upgrade`), `usbutils` (`lsusb`), `picocom` o. Python+`pyserial`.

Usage-Beispiele (DeepWiki-verifiziert, Details in T-Tasks):

```rust
// postcard + COBS, no_std-tauglich
let frame: &[u8] = postcard::to_slice_cobs(&cmd, &mut tx_buf)?;
let mut acc: postcard::accumulator::CobsAccumulator<128> = postcard::accumulator::CobsAccumulator::new();
// pro empfangenem Chunk:
match acc.feed::<HostCmd>(&chunk) {
    postcard::accumulator::FeedResult::Success { data, .. } => handle(data),
    postcard::accumulator::FeedResult::Consumed => {}
    _ => acc = postcard::accumulator::CobsAccumulator::new(), // OverFull/DeserError → Reset + ERR
}
```

```rust
// embassy-usb CDC-ACM (G4-Muster, aus examples/stm32g4 usb_serial.rs)
bind_interrupts!(struct Irqs { USB_LP => usb::InterruptHandler<peripherals::USB>; });
let driver = Driver::new(p.USB, p.PA12, p.PA11, Irqs);
let mut state = State::new();
let mut class = CdcAcmClass::new(&mut builder, &mut state, 64);
class.wait_connection().await;
let n = class.read_packet(&mut buf).await?;
class.write_packet(&resp[..]).await?;
```

## 6. Fehlende Requirements (Vorschläge, noch nicht vom Auftraggeber bestätigt)

1. VID/PID-Strategie (Test-IDs vs. eigene USB-IDs, Seriennummer/UID).
2. Kanonisches Binärformat: postcard+COBS (`0x00`) vs. Prompt-`0x02…0x03` — Entscheidung Kap. 3 übernehmen.
3. Vollständige Text-Befehlstabelle + Fehlermeldungen (`ERR …\n`) und max. Zeilenlänge.
4. USB-Reconnect/DTR-Verhalten, Puffergrößen, was passiert bei Überlauf (drop vs. ERR).
5. Logging-Policy: defmt-RTT vs. USB — nie Anwendungsdaten über defmt mischen.
6. Panic-Verhalten (panic-probe + Reset) und Watchdog für spätere Modi.
7. Host-Protokollclient als Vertragspartner des `common`-Crates (Versionierung `PROTO_VER`).
8. RAM-Budget: USB/COBS-Puffer aus SRAM1 oder CCM — festlegen, bevor Messpuffer (40k Samples) dazukommen.
9. UART-Pins/Parameter für den späteren Zweitpfad reservieren (kein Pin-Konflikt mit USB/LED/KEY).
10. HIL-Teststrategie: `probe-rs run` + RTT-Erwartungen + Host-Smoke als Gate pro Task.

## 7. Commit-Konvention (für alle Folge-Tasks)

- Format: Conventional Commits, ein logischer Schritt pro Commit:
  `feat|fix|test|docs|chore|refactor(<scope>): <kurz, imperativ, ≤72 Zeichen>`
- Body: was + warum, betroffene Hardware/Peripherie, Validierung (`cargo fmt --check`,
  `cargo clippy`, `cargo test`, `probe-rs run`-Ergebnis). Footer: `Refs: plan/20260912_01_init/task.md <ID>`.
- Beispiele: `feat(fw): usb-cdc echo on g474ce with hsi48`, `feat(proto): first-byte text/binary dispatch`,
  `test(host): smoke ping over cdc`, `docs(plan): …`.
- Nicht committen ohne grüne Gates des jeweiligen Tasks (s. `task.md`); keine fremden/untracked
  Dateien anderer Nutzer anfassen; keine generierten Locks/Artefakte außer `Cargo.lock` (Binaries: ja).

## 8. Kontext für einen unabhängigen Agenten (Pflichtlektüre)

1. `plan/20260912_00_what/spec.md` — Systemkonzept, Modi, postcard+COBS-Vertrag (§5), RAM-Modell (§2).
2. `plan/20260912_01_init/prompt.txt` — dieser Auftrag (Dual-Parser, USB-zuerst, Tooling, Deliverables).
3. `doc/stm32g474ceu6_pcb.md` — Board/Pins (USB PA11/PA12, SWD, KEY PC13, LED).
4. `doc/hal.md` — Embassy- vs. stm32-rs-Ökosystem, Async-Modell, Time-Treiber-Hinweis.
5. `doc/deps.md` — Dependency-Registry (Kap. 5 hier nachtragen).
6. `/workspace/src/embassy/examples/stm32g4/src/bin/usb_serial.rs` — G4-USB-Referenz (kopieren/anpassen).
7. `/workspace/src/embassy/examples/stm32g474/src/bin/comp.rs` — G474-Crate-Muster (Features, Init, defmt).
8. `/workspace/src/embassy/examples/stm32g474/Cargo.toml` + `/workspace/src/embassy/embassy-usb/Cargo.toml` — Versionsanker.
9. `doc/stm32g474_datasheet.md` + `doc/stm32g474_programming_manual.md` — nur bei Clock/USB-Zweifeln.
10. Live-Hardware: `probe-rs list` (STLink V2-1), `lsusb`, `dmesg` nach Flashen/Enumeration.

## 9. Risiken

- HSE-Bestückung des WeAct-Boards ≠ 8 MHz → Clock-Config anpassen, sonst enumeriert USB nicht.
- COBS-Erstbyte kann Buchstabe sein → naive Dispatch-Regel bricht; Kap.-3-Regel (`\n` vs. `0x00`) verwenden.
- `reader`-Überläufe bei 64-B-Paketen → `CdcAcmClass` paketweise lesen, in 128-B-Accumulator kopieren.
- defmt-RTT blockiert ohne Debugger-Session nie, aber USB-Logs ohne Host hängen → `wait_connection` beachten.
