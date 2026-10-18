# Tasks: `source11_compact` — minimale, verschlüsselte Neuauflage

Stand 2026-10-18 · Plan: [`plan.md`](plan.md) · **Reihenfolge ist verbindlich**; jeder Task endet grün (`cargo test --workspace`, `cargo fmt --check`, `cargo clippy --workspace --all-targets -- -D warnings`, weniger Dateien/keine >600 Zeilen).

Arbeitsmodell (Vorgabe aus `prompt.txt`): Umsetzung in Schritten mit Subagenten, kleiner Kontext; **Wonder-Modell plant/architektonisch (`plan.md` lesen), `muse-spark-1.3` setzt Details/Compiler-Interaktion um**. Jeder Agent bekommt nur: sein Task-Feld ändern → Tests → kurz berichten, was fehlt. Kein Agent liest „alles".

## Phase 0 — Grundlagen / Gate

- [ ] S0 – Basislinie source11 anlegen (`Cargo.toml`-Workspace `common/server/client`, edition 2024, README mit Symlink-Kette) → `cargo test --workspace` grün **ohne** X11/Modelle.
- [ ] D0 – Tunnel-Spike in isolation (`spike/`): `russh`-Client baut die echte Kette nach (Plan §2.1): Session zu Jump-sshd → `direct-tcpip` auf dessen `127.0.0.1:2332`-Äquivalent → genestete Session zum Ziel-sshd → `direct-tcpip` auf LBW-Testport; OpenSSH-Ed25519-Keys pro Hop (`tinyeu_key.pem`- + `id_lenovo_to_hpz6o`-Muster); TOFU-Fingerprint (`SHA256:…`) pro Hop; Negativtests (falscher Key, falscher Ziel-Hostkey → Abbruch vor Auth, Jump sieht keinen Klartext) → Gate: Spike läuft oder Plan wird revidiert.
- [ ] W0 – Dissector-Schmier: `sample.pcap` (alle v4-Varianten inkl. `MoveText`, `Challenge`, `Auth`) per `gen_sample.py` erzeugen; Bauweg aus Plan §2.6 gegen die lokale Wireshark-Version verifizieren (Plugin lädt, `tshark -r sample.pcap` zeigt alle Varianten) → C pur, kein Helper, kein Keylog.
- [ ] A0 – Android-Prototyp: `cargo ndk`-Build des Tunnel-Spikes (`russh` + Tokio) als `.so`, `hello from jni`, APK läuft im Emulator/Host-Minimum → Ed25519-Auth per E2E (nicht ECDSA-Fallback).

## Phase 1 — Kern-Runtime (Desktop)

- [ ] T0 – `tunnel/`-Crate (von beiden Clients genutzt): `russh`-Forward + `--jump`-Kette + TOFU-Pinning pro Hop + Watchdog/Reconnect; Unit-/Loopback-Tests gegen Wegwerf-sshd (Direkt + 1 Jump, Ed25519-Key).
- [ ] T1 – `common` v4: `Challenge`/`Auth`/`Hello`/`ClearText`/`NewText`/`MoveText`/`RemoveText`/`Tile`/`Input` + Bincode + `[u32LE]`-Framing, `MAX_MSG` 8 MB; Token-HMAC (`hmac`+`sha2`, konstantzeitiger Vergleich) als `common`-Helfer mit Tests; Protokoll-Tests ≥ source10-`common`.
- [ ] T2 – `server` Grundgerüst aus source10 minus Log: Capture/Av1/OCR/Stubs, Config, Bind nur localhost; **ohne** `lbw-log`; `server`-Tests vorhanden.
- [ ] T3a – `09_stabilize.rs` Anti-Flacker (Plan §2.2a): Normalisieren (Text/Farben/Rechteck quantisieren), Ähnlichkeits-Matching (Levenshtein + Position, Multimenge), Konfidenz-Hysterese, Debounce (neu nach 2 Frames, weg nach 5), Move-Totband (≥6 px); Unit-Tests mit synthetischen Frame-Folgen (Jitter → null Nachrichten, Tippen → Update nach 2 Frames, Scroll → nur `MoveText`, Fehlen 2 vs. 6 Frames).
- [ ] T3b – Delta + Latenzpfad: inhaltsverankerte IDs auf der stabilisierten Folge (`NewText`/`MoveText`/`RemoveText`), Maske aus gesendeten Rechtecken (alt+neu in Dirty-Box); `08_pace.rs`-Pacer (`--budget`, Default 6000, 1 s-Cap), `TCP_NODELAY`, 100-ms-GAP minus Laufzeit, 1 coalescierte Box, überholte Tiles verwerfen; begrenzte Input-`mpsc`, Stale-Flush; Loopback-Tests (Scroll-Soak: identischer Text wird **nicht** erneut übertragen; Priorität: Text springt vor `Tile`, Paced-Tiles kommen ganz an).
- [ ] T4 – Neuer Client (macroquad): `03_net`/`04_scene`/`05_app`; Decay 50→10 ms Poll, Metriken (B/s, Input→Text/Tile-Latenzstempel) sichtbar/testbar; Client-Loopback-Tests.
- [ ] T5 – Fonts: `06_font.rs` Glyphen-Fallback (Plan §2.5): Font-Stapel (Noto CJK → Noto → Unifont → Default) gleichzeitig laden, `ttf-parser`-Cmap-Abdeckung + `char→Font`-Cache, Text in Font-Läufe zerlegen und kumulativ zeichnen; Unit-Tests (Lauf-Zerlegung mit Fake-Abdeckung, gemischter String 中/日/한/Latin) + `render_check.sh` (Xvfb) inkl. Fall „kein Font installiert" (Default greift, kein Absturz).
- [ ] T6 – Server-Deployment: sshd-Pflicht dokumentieren (Ed25519-Hostkey, `AllowTcpForwarding yes`, Key-only), LBW bindet nur `127.0.0.1:7878`; CGNAT-Variante: `hpz6o` hält Reverse-Tunnel zu `tinyeu` (`ssh -R 2332:127.0.0.1:22`, autossh/systemd-Unit + Healthcheck) — README-Abschnitt + Beispiel-Units, kein Code im Server-Binary.

## Phase 2 — Android

- [ ] A1 – `android/rust-core`: Kein JSch in Android; Tunnel-Layer = gemeinsame `tunnel/`-Crate (`russh`); JNI `CoreBridge`-Muster aus source6 (`jni-sys`, Direct-ByteBuffer, `minify=false`).
- [ ] A2 – Kotlin-UI anpassen (Connect-Form mit **konfigurierbaren IPs/Ports pro Hop** — Host/IP, Port, User, Key pro Hop, Jump-Zwischenport, Ziel-LBW-Port; Profile in SharedPreferences speichern/laden/löschen; TOFU-Pinning pro Hop; moderner OpenSSH-Key-Pfad getestet) → Tests: JVM-Host + Emulator-E2E **mit** Ed25519-Client-Key **und** Jump-Host (nicht ECDSA-Fallback) **und** geänderten Test-Ports (Beweis, dass nichts hartcodiert ist); `ci_local` angepasst.
Und zum Schluss: Task-Spicker „ohne Java-Crypto" + Dep-Liste (`russh`, `ssh-key`, rav1d …) in `android/deps.md` schreiben.

## Phase 3 — Wireshark + Docker

- [ ] W1 – `wireshark/`: C-Dissector LBW-v4/7878 (inkl. `NewText`/`MoveText`/`RemoveText`, `Challenge`, `Auth` — Token nie im Klartext-Feld), `check.sh`, `sample.pcap`, README (Bauweg aus Plan §2.6, „kein Lua", ABI-Hinweis, Mitschnitt auf `lo`/Server-Host bzw. Client-`--tap`); Negativtest: SSH-Strecke ohne Tap zeigt keinen LBW-Inhalt.
- [ ] W2 – Client-`--tap 127.0.0.1:<port>` (Debug-Spiegel des Klartext-Stroms für Wireshark, Default aus, Warnung beim Aktivieren).
- [ ] D1 – `docker/`: `Dockerfile`, `netem.sh` (tc-Setup, netem auf 6 kB/s, Delay/Loss-Profile), sshd-Paar (Ziel + Jump-Host für CGNAT-Nachstellung), `smoke_xvfb.sh` (Xvfb `:99`, Server+Client **durch den Jump-Tunnel**, synthetische Eingaben + Scroll-Sequenz), Metrik-Extraktion (Input→Text/Tile p50, Scroll-Dedup-Quote, `queue_peak` via Dissector/tshark-Export) gegen Ziele aus Plan §2.2–2.3 → Skript läuft im Container aus `cl-cl-generator/example/05_dockerfile_meta/source01/examples/03_ai_env $ setup02_run.sh --gpu --host-kmsg --usb --source-isolation --docker-sock --x11 --graphics --host-network`.

## Phase 4 — CI, Release, Abschluss

- [ ] C1 – GitHub-Actions unter `../../.github/workflows/release-29-source11.yml` (+ `android-lbw11.yml` als Abkömmel): Jobs Linux/Android/Dissector; `workflow_dispatch` = Dry-Run; `lbw11-v*`-Tag → Release mit Server+Modellen, SSH-Client (Größe nur messen/berichten, **kein** MB-Gate), Direkt-Client ohne Tunnel als separates schlankes Artefakt, APK, Dissector, `SHA256SUMS`.
- [ ] C2 – `scripts/fetch_models.sh` (PP-OCRv6 det/rec/`inference.yml` von HuggingFace `PaddlePaddle`, SHA256; `gpa_640_int8.onnx` optional lokal/`--gui none`); Server-Release enthält Gewichte (keine Blobs im Git).
- [ ] C3 – Abschluss: LOC-Ziel (~3,5–4 k) messen, README + Walkthrough + Smokes grün, Plan-Aspekte (inkl. gestrichene ≥5 Features) als Tabelle belegen; Commits pro Task (Conventional Commits), unverdientes Ops-Signing nach Plan §2.1 → `go`.
