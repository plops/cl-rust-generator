# Walkthrough — 20260912_01_init: Dual-Mode-Parser + USB-CDC-Bring-up

Stand: 2026-09-12. Code: `examples/24_embassy_stm32g4/fw/` (Workspace: `firmware`, `common`, `host-smoke`).
Plan: `plan.md`, Schritte: `task.md` (T1–T8).

## 1. Was implementiert wurde (vs. Plan)

- **T1 Scaffold:** `fw/`-Workspace mit `g474-common` (`HostCmd`/`DeviceResp`, `PROTO_VER=1`,
  Module `text`/`frame`/`router`), `g474-firmware` (Bin `usb_proto`, `.cargo/config.toml` mit
  Runner `probe-rs run --chip STM32G474RETx`, Target `thumbv7em-none-eabi`), `g474-host-smoke`
  (Vertragstest über USB-CDC). Die Target-Config liegt bewusst unter `firmware/.cargo/`,
  damit Host-Tests (`common`, `host-smoke`) nativ bauen.
- **T2 USB-CDC:** `CdcAcmClass` (64 B), `Driver::new(p.USB, Irqs, p.PA12, p.PA11)`,
  HSI48+CRS (`sync_from_usb`), `clk48sel=HSI48`, **kein HSE** (Quarzbestückung unverifiziert),
  VID/PID `C0DE/CAFE`. RTT-Log auf echter Hardware: `usb_proto starting`, `hsi48: 48 MHz`,
  CDC-Endpoints allokiert, kein Panic (s. Kap. 4).
- **T3 Text:** `HELP|PING|GET VER|GET UID` + `ERR UNKNOWN` / `ERR TOOLONG`, `\n`/`\r`/`\r\n`,
  `GET UID` antwortet mit echter Chip-UID (`embassy_stm32::uid::uid_hex`).
- **T4 Binär:** postcard+COBS (`to_slice_cobs`, `CobsAccumulator<128>`, `0x00`-Delimiter),
  `Ping|GetVer|Echo` ↔ `Pong|Ver|Echo|Err`, COBS-Invariante (`0x00` nur als Delimiter) getestet.
- **T5 Dispatch:** Kap.-3-Regel aus `plan.md` (`\n`/`\r`→Text, `0x00`→Binär). Der Router lebt in
  `common::router` und wird **exakt derselbe Code** in der Firmware verwendet — die T5-Matrix
  (Text/Binär gemischt, paketweise gesplittet, korrupt→Recovery, TOOLONG→Resync) läuft als
  Host-Unit-Test statt nur auf dem Gerät.
- **T6 host-smoke: GRÜN auf beiden Transporten.** `g474-host-smoke /dev/ttyACM1` (USB) und
  `/dev/ttyACM0` (UART via STLink-VCP): jeweils `all smoke checks passed` — Text-Matrix
  (PING/HELP/GET VER/GET UID mit echter UID `4C002B000250314B56373820`/FOO/PING-CRLF),
  Binär-Matrix (Ping→Pong, GetVer→Ver), korruptes Frame → `Err`, danach weiter `PONG`.
- **UART-Zweitpfad (nachträglich, User-Wunsch):** `USART1` auf `PA9` (TX)/`PA10` (RX),
  115200 8N1, DMA (`DMA1_CH1` TX, `DMA1_CH2` RX), `read_until_idle`-Chunks durch denselben
  `common::router` (`dispatch()`-Helper teilen sich USB- und UART-Pfad). `GET UID`-Logik als
  `reply_text()` nach `common` gezogen + Unit-Test (`reply_text_uid_branch`).
- **T7 Stabilisierung:** `wait_connection`-Reconnect-Loop, Accumulator-Reset nach jedem
  Fehler, `PROTO_VER` in `Ver`-Antwort, Limits dokumentiert (Zeile 64, Frame 128).
  Mini-Dauerlauf: **6/6 Smoke-Durchläufe grün** (3× USB + 3× UART im Wechsel, je frischem
  Port-Open).

## 2. Abweichungen vom Plan (begründet)

1. **`common::router` statt Firmware-lokalem Staging:** Die Dispatch-Logik wäre auf dem Gerät
   ohne USB-Link untestbar gewesen — jetzt teilen sich Host-Test und Firmware denselben Code.
2. **heapless 0.7 statt 0.9:** `postcard 1.1.3` pinnt intern heapless 0.7; 0.8/0.9 legen einen
   zweiten heapless-Typ in den Dependency-Graph und brechen alle serde-Derives (E0277).
   `doc/deps.md` korrigiert.
3. **crates.io-API statt lokalem Embassy-Checkout:** `Driver::new(p.USB, Irqs, p.PA12, p.PA11)`
   (crates.io 0.6.0) statt Beispiel-Reihenfolge `(p.USB, p.PA12, p.PA11, Irqs)` (neuerer Checkout);
   `Clk48sel::HSI48` (Großschreibung); `dual-bank`-Feature für `stm32g474ce` Pflicht.
   Embassy-Versionsanker: stm32/usb 0.6.0, executor 0.10, time 0.5.1, sync 0.8, futures 0.1.2.
4. **Kein LED-Heartbeat (T2):** Der User-LED-Pin steht nicht in `doc/stm32g474ceu6_pcb.md`
   (nur KEY=PC13 dokumentiert) — Heartbeat läuft über defmt/RTT statt LED. Pin nachtragen,
   dann nachrüsten.
5. **Kein `cargo upgrade`-Lauf:** Alle Dependencies wurden am 2026-09-12 frisch gegen
   crates.io aufgelöst (= neueste semver-kompatible); `cargo-edit` bringt hier keinen Mehrwert.

## 3. Test-Evidenz (alle Gates grün außer Geräte-HIL)

- `cargo test -p g474-common`: **17/17 ok** (10× text/frame + 6× router: CRLF, Split-Frames,
  Mixed-Mode, Corrupt-Recovery, TooLong-Resync, Lone-NUL + `reply_text_uid_branch`).
- `cargo fmt --check`: sauber (alle drei Crates).
- `cargo clippy -p g474-common -p g474-host-smoke --all-targets -- -D warnings`: sauber.
- `cargo clippy --release -- -D warnings` (Firmware, thumbv7em): sauber.
- `cargo build --release` (Firmware): ok. `probe-rs download`: ok (2.4 s).
- RTT-Boot auf Hardware (`probe-rs run`, STLink V2-1 `0668FF373841423043111225`): s. Kap. 4.

## 4. RTT-Boot-Log (Auszug, echte Hardware)

```
[DEBUG] rcc: Clocks { …, hsi: 16000000, hsi48: 48000000, hse: 0, sys: 16000000 … }
[INFO ] usb_proto starting (usb_proto firmware/src/main.rs:41)
[TRACE] allocating type=Interrupt mps=8 … / type=Bulk mps=64 … dir=Out / … dir=In
[TRACE] USB: config_descriptor used: 70 / bos_descriptor used: 12 / control_buf size: 64
```

Belegt: Clock-Setup ohne HSE, USB-Stack inkl. CDC-Endpoints initialisiert, kein Panic bis zum
Warten auf den Host. `USB connected` kann hier nicht erscheinen (s. Kap. 5).

## 5. HIL-Blocker (aufgelöst) + Learnings zum Flashen

- **Ursprünglicher Blocker:** Board-USB hing nicht am Host (`lsusb` nur STLink). Nach Anstecken
  (User): `c0de:cafe … G474 dual-mode proto` → `ttyACM1`. Im Container fehlte der Device-Node
  (kein udev-Hotplug) — behoben via `mknod /dev/ttyACM1 c 166 1` (Nummer aus
  `/sys/class/tty/ttyACM1/dev`).
- **`probe-rs download` lässt den Core angehalten zurück:** Nach `download` enumeriert das Gerät
  nicht — erst `probe-rs run` (oder Reset/Power-Cycle) startet die Firmware. Falsche Fährte
  („UART-Umbau hat USB kaputtgemacht") wurde per RTT-Log widerlegt: USB-Enumeration
  (`SETUP`, `SET_ADDRESS`) lief parallel zu `UART ready/serving` normal weiter.
- **UART-Verdrahtung (User-seitig, verifiziert):** STLink-VCP ↔ `PA9`/`PA10` gekreuzt am
  unteren Header (Pos. 6). Durch den grünen UART-Smoke ist die Richtung bewiesen
  (falsch herum käme kein einziges `ok`).
- RTT-Beleg UART-Pfad: `USART: … desired baudrate: 115200, actual baudrate: 115107`,
  `UART ready on PA9/PA10 @115200`, `UART serving on USART1`, danach USB-Reset/Setup —
  kein Panic, kein HardFault.

## 6. Learnings

- postcard-Version diktiert die heapless-Version — bei `no_std`-Serde immer zuerst `cargo tree`
  prüfen statt die neueste heapless zu nehmen.
- Embassy-Beispiele im lokalen Checkout können neuer sein als crates.io — bei API-Diffs
  (`Driver::new`-Reihenfolge) gilt die Registry-Quelle aus `Cargo.lock`.
- Workspace-`.cargo/config.toml` mit festem ARM-Target bricht alle Host-Tests — Target-Runner
  gehört in das Firmware-Crate, nicht in den Workspace.
- `to_ascii_uppercase` auf `&str` braucht alloc — in `no_std` über `heapless::String` +
  `make_ascii_uppercase` gehen.
- USB-HIL braucht zwei physische Links (SWD + Geräte-USB); STLink-VCP ≠ Geräte-CDC —
  vor jedem HIL `lsusb` mit VID:PID-Erwartung prüfen.

## 7. Erweiterungen (Reihenfolge-Vorschlag)

1. USB-C anstecken → T6/T7-HIL schließen (host-smoke grün, Dauerlauf, Reconnect-Test).
2. LED-Pin recherchieren (Schaltplan) → Heartbeat-LED + KEY PC13 als User-Input.
3. UART-Zweitpfad (Pins aus Pinout reservieren, selber Router wiederverwenden).
4. Messmodi aus Spec (Scope/VNA/AWG/Cap/Freq) als exklusive Tasks + `PROTO_MISMATCH`-Handshake.
5. TUI-Client gegen `common`-Vertrag.

## 8. Neue Docker-Pakete (bereits installiert / noch aufzunehmen)

- **Installiert:** `libudev-dev`, `pkg-config` (Pflicht für `serialport`/`host-smoke`-Build),
  ARM-Target `thumbv7em-none-eabi` (`rustup target add`).
- **Noch aufnehmen:** `usbutils` (`lsusb`), `picocom` (manuelle Text-HIL), `python3-serial`
  (Ad-hoc-Smoke), `cargo-edit` (`cargo upgrade`-Workflow aus dem Auftrag).
