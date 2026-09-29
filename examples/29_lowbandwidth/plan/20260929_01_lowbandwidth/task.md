# task.md — serielle Tasks (jeder Schritt getestet und validiert)

Konventionen: Arbeitsverzeichnis `examples/29_lowbandwidth/source6`.
Nach **jedem** Task: `cargo fmt --all`, `cargo clippy --workspace
--all-targets -- -D warnings`, `cargo test --workspace`, dann Commit
(Conventional Commit, siehe `plan.md` §7).

Die Vorlage des Prompts nennt „Modus / Host-Tests / HIL-Nachweis / TUI“.
Übertragen auf dieses Projekt: *Modus* = Teilsystem, *Host-Tests* =
`cargo test`, *HIL-Nachweis* = End-to-End unter Xvfb mit echten Modellen
und gedrosselter Leitung, *TUI* = Client-Bedienoberfläche.

## T0 Spike (Risiko zuerst)
- [x] T0.1 Workspace `common/ server/ client/ throttle/` anlegen, Rust 2024.
- [x] T0.2 `server/09_av1.rs` (rav1e still) + `client/02_av1.rs` (rav1d).
- [x] T0.3 Test `server/tests/av1_roundtrip.rs`: 640², 128×48, 16², 200×90 dekodieren, Farben ±24.
  Validierung: `cargo test -p lbw-server --test av1_roundtrip`.

## T1 Protokoll (`lbw-common`)
- [x] T1.1 `01_types.rs` Rect, TextItem, Input, Konstanten.
- [x] T1.2 `02_codec.rs` Encode/Decode aller Nachrichten; Tests: Roundtrip jeder Variante, abgeschnittene/ungültige Frames → `Err`.
- [x] T1.3 `03_frame.rs` Framing über `Read`/`Write` inkl. Teil-Reads/Timeouts; Test mit Cursor und 1-Byte-Reader.
- [x] T1.4 `04_keys.rs` Keysyms/Modifier, `char_to_keysym`; `06_rate.rs` Token-Bucket (Test mit künstlicher Zeit).

## T2 Server-Analyse (Modus „Analyse“)
- [x] T2.1 Implementierung: `03_image`, `04_ocr_detect`, `05_ocr_recognize`, `06_gui_detect`, `07_layout`, `08_dirty`, `10_text_diff`, neu `11_analyze` (Modelle bündeln, Erkennungs-Cache).
- [x] T2.2 Host-Tests: Box-Klassifikation, Farbsampling, Maske, Dirty-Rects (Merge, Grenzen, gerade Kanten), Text-Diff (IDs stabil, remove/add).
- [x] T2.3 Nachweis mit echten Modellen (`server/tests/models.rs`, `--ignored`): HN-Screenshot aus `26_onnx/source8` → 37 Zeilen, 3 Icons, AV1 37,6 kB → 2,0 kB durch Maskierung.

## T3 Server-Transport (Modus „Versand“)
- [x] T3.1 `12_scheduler.rs`: Priorität, Chunking, Token-Bucket, Ack-Fenster, Stats.
- [x] T3.2 Host-Tests: Text überholt Bild, Rate eingehalten (±10 %), Fenster blockiert ohne Acks.
- [x] T3.3 `14_session.rs`: Accept, Handshake, Ablösung alter Verbindung, Heartbeat, Resume.
- [x] T3.4 `13_input.rs`: XTEST (Maus, Tasten, fehlende Keysyms binden).
- [x] T3.5 `15_pipeline.rs` mit `FrameSource`-Trait (X11 / synthetisch), adaptive Bildrate.
- [x] T3.6 `server/tests/loopback.rs`: synthetische Quelle → Session → Test-Client (lbw-client-Lib): Text + Kacheln kommen an, Resume ohne Refresh, Reconnect mit Refresh.

## T4 Drossel-Proxy (`lbw-throttle`)
- [x] T4.1 Rate je Richtung, Latenz, Blackout (N s nichts weiterleiten), Abriss nach N s.
- [x] T4.2 Test: 6 kB/s gemessen ±10 %; Blackout 60 s überlebt die Verbindung (E2E, T6).

## T5 Client (Modus „Client“, danach TUI)
- [x] T5.1 `03_net.rs` Reconnect-Schleife, Ack, Ping, Dekodierung im Netz-Thread.
- [x] T5.2 `04_scene.rs` Kachel-Zusammenbau, Textmap; Host-Tests.
- [x] T5.3 `05_input.rs` KeyCode→Keysym, Maus-Throttle; Host-Tests.
- [x] T5.4 TUI: `07_render.rs` (Canvas, Unifont-Fit, HUD), `06_select.rs` (Auswahl→Clipboard, Paste), `08_app.rs`.

## T6 HIL-Nachweis (End-to-End)
- [x] T6.1 `scripts/smoke_xvfb.sh`: Xvfb :99 (Server, xterm), Xvfb :98 (Client), Drossel 6 kB/s. Nachweise: Text erscheint im Client-Log, Tippen im Client landet im xterm (OCR sieht es), Screenshots, Bytes/s ≤ 6000.
- [x] T6.2 Blackout 60 s + Abriss/Reconnect im Smoke.
- [x] T6.3 `scripts/ssh_tunnel.sh`: lokaler `sshd`, `ssh -L`, Client über Tunnel.
- [x] T6.4 Messwerte (Latenz Text, Bytes pro Kachel, CPU-Zeiten) in `source6/README.md`.

## T7 Abschluss
- [x] T7.1 `cargo upgrade --incompatible`, fmt, clippy, Tests, Binärgröße Client (`--profile min`).
- [x] T7.2 `walkthrough.md` (Deutsch, Mermaid, Regeln in `plan.md` §8), Commit.

## Status

Alle Tasks erledigt (Commits `44b7454` … siehe `git log -- examples/29_lowbandwidth`).
Abweichungen vom Plan (Details im Walkthrough): `11_analyze.rs` eingeschoben
(Nummern 12–15 verschoben); Icon-Erkennung braucht zusätzlich einen
Inhaltstest; Maske 2 px gepolstert; Unifont fest 16 px; rav1d ohne `asm`.
