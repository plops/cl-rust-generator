# task.md — 20260912_01_init: seriell abarbeitbare Schritte
Jeder Schritt endet mit Gates. Erst bei grünen Gates committen (Conventional Commits,
s. `plan.md` Kap. 7) und zum nächsten Schritt gehen. Ordner für Code:
`examples/24_embassy_stm32g4/fw/` (Workspace: `firmware`, `common`, optional `host-smoke`).

## T1 — Scaffold + Tooling
- `fw/`-Workspace + `common`-Crate-Skelett (`HostCmd`/`DeviceResp` mit `PROTO_VER: u8 = 1`)
  anlegen; `firmware`-Crate nach Embassy-`stm32g474`-Muster (`.cargo/config.toml`: Runner
  `probe-rs run --chip STM32G474RETx`, Target `thumbv7em-none-eabi`, `DEFMT_LOG=trace`).
- `cargo install cargo-edit`, `apt install usbutils picocom` (falls fehlend); `doc/deps.md`
  um Kap.-5-Zeilen aus `plan.md` ergänzen.
- Gates: `cargo fmt --check`, `cargo clippy -- -D warnings` (firmware für Host-Target prüfen
  nur `common`), `cargo metadata` ok.
- Commit: `chore(fw): scaffold workspace with common stub`.

## T2 — USB-CDC Bring-up (Echo)
- `usb_serial.rs`-Muster von `examples/stm32g4` auf G474 portieren: `stm32g474ce`-Feature,
  `USB_LP`-Interrupt, `Driver::new(p.USB, p.PA12, p.PA11, Irqs)`, HSI48+CRS, `Clk48sel::Hsi48`,
  VID/PID `C0DE/CAFE`, Echo-Loop mit `read_packet`/`write_packet` (64 B).
- HSE-Annahme (8 MHz) am Board verifizieren; bei Abweichung Clock-Config korrigieren.
- Gates: `cargo fmt --check`, `cargo clippy`, `cargo run --release --bin usb_proto`
  flasht per `probe-rs`; `lsusb`/`dmesg` zeigt CDC-Gerät; `picocom`/Python-Echo kommt zurück;
  LED blinkt bei Connect (sichtbarer Heartbeat).
- Commit: `feat(fw): usb-cdc echo on g474ce with hsi48`.

## T3 — Text-Protokoll
- `HELP|PING|GET VER|GET UID` + `ERR UNKNOWN\n` implementieren (`\n`/`\r`-terminiert,
  max. 64 Zeichen/Zeile, Überlauf → `ERR TOOLONG\n` + Resync). Unit-Tests auf Host
  (`common::text`, Fälle: `\r`, `\n`, `\r\n`, leer, zu lang, unbekannt).
- Gates: `cargo test -p common` grün, `cargo fmt --check`, `cargo clippy`,
  HIL: jede Zeile per `picocom` von Hand tippen und Antwort prüfen.
- Commit: `feat(proto): text commands help/ping/get`.

## T4 — Binär-Protokoll (postcard + COBS)
- `HostCmd::{Ping,Echo{…},GetVer}` / `DeviceResp::{Pong,Echo{…},Ver{…},Err{…}}` in `common`
  (serde, `no_std`); FW nutzt `to_slice_cobs` + `CobsAccumulator<128>`; Fehler → Accumulator-Reset.
- Host-Unit-Tests: Roundtrip pro Variante, Chunk-Splits (1-Byte-Feed), `OverFull`/`DeserError`-Reset,
  Nachweis: `0x00` nur als Delimiter.
- Gates: `cargo test -p common` grün, `fmt`, `clippy`, danach erst FW-Verdrahtung.
- Commit: `feat(proto): postcard-cobs binary frames in common`.

## T5 — First-Byte-Dispatch am Gerät
- Router nach `plan.md` Kap. 3: `\n`/`\r` ohne `0x00` → Text, `0x00` → COBS-Feed.
  Busy-Loop vermeiden (async `read_packet`, 128-B-Accumulator, 64-B-Line-Buffer).
- HIL-Matrix: `PING\n`→`PONG\n`, `HELP\n`, Binär-`Ping`→Binär-`Pong`, gemischt alternierend,
  korruptes Frame → danach weiter `PONG\n` (kein Deadlock).
- Gates: `fmt`, `clippy`, volle HIL-Matrix protokolliert (RTT-Log + Host-Mitschnitt).
- Commit: `feat(fw): first-byte text/binary dispatch`.

## T6 — Host-Smoke (Vertragstest, std)
- Kleines `host-smoke` (oder Python+pyserial): sendet Text- und Binär-Vektor aus T5,
  assertet Antworten, Exit-Code 0/≠0. Keine TUI-Ambition.
- Gates: `cargo test`/`cargo run -p host-smoke` grün gegen echte Hardware; `fmt`, `clippy`.
- Commit: `test(host): smoke text+binary vectors over cdc`.

## T7 — Stabilisierung
- `wait_connection`/Reconnect (USB-Stecker ziehen → wieder `PONG`), DTR-unabhängig,
  60-s-Dauerlauf (Text+Binär im Wechsel, Zähler auf Drift/Stillstand prüfen),
  `PROTO_VER`-Mismatch → `ERR`/Binär-`Err`, Doku der Grenzen (max. Frame, Zeilenlänge).
- Gates: alle Gates aus T2–T6 erneut grün + Dauerlauf-Log im Task-Kommentar.
- Commit: `fix(fw): reconnect and limits hardening` (ggf. mehrere `fix|test`-Commits).

## T8 — Abschluss-Doku (walkthrough.md)
- Erst nach T7: `plan/20260912_01_init/walkthrough.md` schreiben — was implementiert vs. Plan,
  abweichende Entscheidungen (Kap.-3-Frage!), Mess-Logs (RTT, `lsusb`, Smoke-Output),
  Learnings, Erweiterungen (UART-Pfad, Modi aus Spec, TUI), **neu benötigte Docker-Pakete**
  (`cargo-edit`, `usbutils`, `picocom`, `python3-serial` o.ä. mit Begründung).
- Gates: `cargo fmt --check`, `cargo clippy`, `cargo test -p common` final grün;
  Dateiliste der Commits (`git log --oneline`) im Walkthrough zitieren.
- Commit: `docs(plan): walkthrough for dual-mode usb bring-up`.
