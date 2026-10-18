# Tasks: `source11_compact` — minimale, verschlüsselte Neuauflage

Stand 2026-10-18 · Plan: [`plan.md`](plan.md) · **Reihenfolge ist verbindlich**; jeder Task endet grün (`cargo test --workspace`, `cargo fmt --check`, `cargo clippy --workspace --all-targets -- -D warnings`, weniger Dateien/keine >600 Zeilen).

Arbeitsmodell (Vorgabe aus `prompt.txt`): Umsetzung in Schritten mit Subagenten, kleiner Kontext; **Wonder-Modell plant/architektonisch (`plan.md` lesen), `muse-spark-1.3` setzt Details/Compiler-Interaktion um**. Jeder Agent bekommt nur: sein Task-Feld ändern → Tests → kurz berichten, was fehlt. Kein Agent liest „alles".

## Phase 0 — Grundlagen / Gate

- [x] S0 – Basislinie source11 anlegen (`Cargo.toml`-Workspace `common/server/client`, edition 2024, README mit Symlink-Kette) → `cargo test --workspace` grün **ohne** X11/Modelle.
- [x] D0 – Tunnel-Spike in `tunnel/` (statt `spike/`, direkt wiederverwendbar): `russh` 0.64 baut die echte Kette (Jump-sshd → `direct-tcpip` → genestete Ziel-Session → Echo statt LBW-Port); OpenSSH-Ed25519-Keys pro Hop; TOFU-Fingerprint (`SHA256:…`) pro Hop; Negativtests (falscher Key, falscher Hostkey → Abbruch vor Auth). `cargo test -p lbw-tunnel`: 3 Ketten-Tests grün gegen echte sshd (2222–2226).
- [x] W0 – Dissector-Schmier: `sample.pcap` (alle v4-Varianten inkl. `MoveText`, `Challenge`, `Auth`) per `gen_sample.py` erzeugt (Vektoren per `vecdump` aus `encode_msg` verifiziert); Bauweg aus Plan §2.6 gegen Wireshark 4.6.4 verifiziert (`./check.sh`: alle 14 Varianten + Felder, Baum-Pass sauber) → C pur, kein Helper, kein Keylog.
- [x] A0 – Android-Prototyp: `lbw-android-core` (`cdylib`, JNI-`version()`) linkt `russh` + Tokio für arm64 + x86_64 (NDK r27d, minSdk 26; direkt per NDK-Clang, da cargo-ndk 4.1.2 das Layout nicht kennt — s. `android/README.md`); JNI-Symbol verifiziert, Workspace-Tests grün. APK + Emulator-E2E mit Ed25519-Auth bleiben in A1/A2 (JDK/SDK hier nicht installiert).

## Phase 1 — Kern-Runtime (Desktop)

- [x] T0 – `tunnel/`-Crate (von beiden Clients genutzt): `russh`-Forward + `--jump`-Kette + TOFU-Pinning pro Hop + Watchdog (`nodelay`, Keepalive 15 s/3, Connect-Timeout) + Reconnect (`connect_chain_retry`); Unit-/Loopback-Tests gegen Wegwerf-sshd (Direkt + 1 Jump, Ed25519-Key, Spätstart, Neustart).
- [x] T1 – `common` v4: `Challenge`/`Auth`/`Hello`/`ClearText`/`NewText`/`MoveText`/`RemoveText`/`Tile`/`Input` + Bincode + `[u32LE]`-Framing, `MAX_MSG` 8 MB; Token-HMAC (`hmac`+`sha2`, konstantzeitiger Vergleich) als `common`-Helfer mit Tests; 9 Protokoll-Tests (= source10-`common`).
- [x] T2 – `server` Grundgerüst aus source10 minus Log: Capture/Av1/OCR/Stubs, Config (`--token`/`--token-file` statt `--record`), Bind nur localhost; **ohne** `lbw-log`; `server`-Tests: 42 Unit + 4 Loopback (Vollbild NewText+Tile, Stillstand-Stille, Token-/Versions-Abweisung).
- [x] T3a – `09_stabilize.rs` Anti-Flacker (Plan §2.2a): Normalisieren (Text/Farben/Rechteck quantisieren), Ähnlichkeits-Matching (Levenshtein + Position, Multimenge, Tipp-Gate ≤24 px), Debounce (neu nach 2 Frames, weg nach 5), Move-Totband (≥6 px); 5 Unit-Tests (Jitter → null, Tippen → Update nach 2 Frames, Scroll → nur `MoveText`, Fehlen 2 vs. 6, Duplikate); in Session verdrahtet (Maske aus gesendeten Rechtecken). 71 Tests grün.
- [x] T3b – Delta + Latenzpfad (Pacer, `TCP_NODELAY`, 1 Box/Frame, begrenzte Input-`mpsc` bereits aus T2); Loopback-Tests grün (`scroll_soak`: Scroll = genau 3× `MoveText`, 0× `NewText`; `texts_jump_the_tile_queue` bei 200 B/s: Text <2 s, Tile kommt ganz an). 73 Tests grün.
- [x] T4 – Neuer Client (macroquad): `03_net` (v4-Handshake Challenge→Auth→Hello, 10-ms-Poll, Stale-Flush, `--tap`-Spiegel), `04_scene` (New/Move/Remove), `05_app` (HUD mit B/s + Input→Text/Tile-Latenz); Metriken-Unit-Tests + Client-Loopback gegen Mini-Server (Handshake, Delta, Tile-Fehlerpfad). 86 Tests grün.
- [x] T5 – Fonts: `06_font.rs` Glyphen-Fallback (Plan §2.5): Font-Stapel (Noto CJK → Noto → Unifont → Default) gleichzeitig laden, `ttf-parser`-Cmap-Abdeckung + `char→Font`-Cache, Text in Font-Läufe zerlegen und kumulativ per `draw_text_ex` zeichnen; 3 Unit-Tests (Lauf-Zerlegung, gemischter String, Cache). Kein Bundling, keine Blobs im Git. 89 Tests grün.
- [x] T6 – Server-Deployment: `DEPLOY.md` (sshd-Pflicht, Token-Datei, CGNAT per `autossh -R 2332:127.0.0.1:22` + systemd-Unit mit Healthcheck); Client-Tunnel-Verdrahtung (`--via`/`--jump`/`--identity`/`--pin`/`--target-sshd`, lokaler Forward, `--tap` fixiert den Port).

## Phase 2 — Android

- [ ] A1 – `android/rust-core`: Kein JSch in Android; Tunnel-Layer = gemeinsame `tunnel/`-Crate (`russh`); JNI `CoreBridge`-Muster aus source6 (`jni-sys`, Direct-ByteBuffer, `minify=false`).
- [ ] A2 – Kotlin-UI anpassen (Connect-Form mit **konfigurierbaren IPs/Ports pro Hop** — Host/IP, Port, User, Key pro Hop, Jump-Zwischenport, Ziel-LBW-Port; Profile in SharedPreferences speichern/laden/löschen; TOFU-Pinning pro Hop; moderner OpenSSH-Key-Pfad getestet) → Tests: JVM-Host + Emulator-E2E **mit** Ed25519-Client-Key **und** Jump-Host (nicht ECDSA-Fallback) **und** geänderten Test-Ports (Beweis, dass nichts hartcodiert ist); `ci_local` angepasst.
Und zum Schluss: Task-Spicker „ohne Java-Crypto" + Dep-Liste (`russh`, `ssh-key`, rav1d …) in `android/deps.md` schreiben.

## Phase 3 — Wireshark + Docker

- [x] W1 – `wireshark/`: C-Dissector LBW-v4/7878 (alle 14 Varianten, Token nur als Längen), `check.sh` grün (Varianten-Multimenge + Baum-Pass + Feld-Stichproben), `sample.pcap`, README (Bauweg, „kein Lua", ABI-Hinweis, Mitschnitt auf `lo` bzw. Client-`--tap`).
- [x] W2 – Client-`--tap <port>` fixiert den Forward-Port (Wireshark auf `lo`); E2E-Smoke zerlegt den Tap per Dissector. Entfernt: pro-Nachricht-Spiegel-Idee (braucht keinen Hörer — ersetzt durch fixen Port).
- [x] D1 – `docker/` (`Dockerfile`, `netem.sh` opt-in mit tc-Profilen) + `scripts/smoke_e2e.sh`: Xvfb `:99` + xterm, echte Modelle, sshd-Paar (Jump 2322 + Ziel 2323 wie tinyeu→hpz6o), Client **durch die Jump-Kette**, xdotool-Eingaben, Tap per Dissector. Ergebnis (echter Lauf): NewText + Tile + getippte Text-Echos im Tap → E2E-OK. Latenz-p50-Extraktion als Follow-up (Metriken liegen im HUD).

## Phase 4 — CI, Release, Abschluss

- [x] C1 – GitHub-Actions `.github/workflows/release-29-source11.yml` (Muster `release-29-lowbandwidth.yml` + `android-lbw.yml`): Jobs Linux (fmt/clippy/tests + Dissector-`check.sh` + `package_release.sh`, Größe nur berichtet), Android-`.so` (beide ABIs, NDK-Setup wie bewährt, Direkt-Clang wie in `android/README.md`), Publish (`lbw11-v*`, Server+Modelle-, Client-, Android-Core-, SHA256SUMS; `workflow_dispatch` = Dry-Run). Ungelaufen (kein GH-Zugang hier) — erster Dry-Run fällig.
- [x] C2 – `scripts/fetch_models.sh` (lokal → HuggingFace-Fallback, SHA256; `gpa` optional): echter Lauf — alle 4 Dateien SHA256-verifiziert (52 MB), keine Blobs im Git.
- [ ] C3 – Abschluss: LOC-Ziel (~3,5–4 k) messen, README + Walkthrough + Smokes grün, Plan-Aspekte (inkl. gestrichene ≥5 Features) als Tabelle belegen; Commits pro Task (Conventional Commits), unverdientes Ops-Signing nach Plan §2.1 → `go`.
