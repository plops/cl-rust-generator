# Tasks — seriell, jeder Schritt getestet (37_live/source2)

Arbeitsordner `examples/37_live/source2` (Crate `live-silk`, Edition
2024). Gate nach jedem Task: `cargo fmt --check && cargo clippy
--all-targets -- -D warnings && cargo test`. Commit-Format siehe
[plan.md §7](./plan.md#7-commit-konvention). Dateien: Nummer + Name,
`lib.rs`/`main.rs` nur Deklaration/Verdrahtung, < ~600 Zeilen.

## T1 — Gerüst, Config, Sprachen

- `Cargo.toml` (Deps aus plan.md §4, `cargo upgrade --dry-run` = latest),
  `deps.md`, `.gitignore` (`target/`).
- `src/01_config.rs`: CLI `--listen --dir --key-file --gemini-url
  --verbose --check --watch`, `MAX_PEERS = 2`, `Transport {Pcm, Opus}`.
- `src/02_languages.rs`: 78 Sprachen (`code`, `name`), `DEFAULT_LANG
  = "de"`, `DEFAULT_TARGET = "zh-Hans"`, `find`.
- Tests: CLI-Parse, 78 Einträge, eindeutig, Defaults enthalten.
- Commit `feat(37_live): geruest, cli und sprachtabelle fuer source2`.

## T2 — Opus-SILK-Codec

- `src/03_codec.rs`: `DownlinkEncoder` (PCM24-Bytes beliebiger Länge →
  0..n Opus-Pakete à 60 ms, 12 kbit/s, Wideband-Cap, Voice),
  `UplinkDecoder` (Opus-Paket → PCM16k i16).
- Tests: Roundtrip-Energie, alle Pakete `SilkOnly`, Bitrate < 16 kbit/s,
  Framing (Stückelung 1000/333/7 Bytes ergibt gleiche Paketzahl).
- Commit `feat(37_live): opus-silk-codec ...`.

## T3 — Raumbuch (max 2, Peer-Infos)

- `src/04_state.rs`: `Peer {tx, info: PeerInfo{lang,target,transport}}`,
  `Out` = `Pcm24(Vec<u8>)` | `Text(String)`; `join` (max 2) / `leave` /
  `broadcast_audio` / `broadcast_text` (ohne Sender) / `infos` /
  `snapshot`.
- Tests: 3. Peer abgewiesen, kein Echo, leere Räume weg.
- Commit `feat(37_live): raumbuch mit zwei-peer-limit ...`.

## T4 — Gemini-Client

- `src/05_gemini.rs`: `setup_json(target)` (translationConfig in
  `generationConfig`, Transkription auf `setup`-Ebene), `audio_json`,
  `parse_server_msg` → `Audio(Vec<u8>) | InText | OutText | Other`;
  `Session::connect(url, key, target)` mit Tasks: In-Kanal PCM16 →
  Gemini, Events → Out-Kanal. `rustls` mit `ring`.
- Unit-Tests: JSON-Struktur, Parsing echter Beispielnachricht.
- Live-Test `#[ignore]` (`LIVE_KEY_FILE`): TTS-frei, 2 s Stille → nur
  `setupComplete` geprüft.
- Commit `feat(37_live): gemini-live-client mit translationconfig ...`.

## T5 — Relay (PCM- und Opus-Peers) + Router

- `src/06_relay.rs`: `/ws?room&transport&lang&target`, Hello-Nachricht
  (Key), PCM-Pfad wie source1, Opus-Pfad: Decoder → Gemini-Session →
  `broadcast_audio`, Downlink je Peer: PCM roh oder `DownlinkEncoder`.
  Untertitel als Text, `peers`-Event bei Ein/Austritt.
- `src/07_serve.rs`: `/`, `/ws`, `/healthz`, `/api/rooms` (mit
  Transport), `/api/languages`.
- `examples/fake_gemini.rs` + `tests/common` Fake: antwortet auf jedes
  Audio-Chunk mit 100 ms Sinus PCM24 + `outputTranscription`.
- Integrationstests `tests/ws_relay.rs`: PCM↔PCM, Raum voll,
  Opus→Fake-Gemini→PCM-Partner, PCM→Opus-Partner (SILK-Pakete
  dekodierbar, Bytes < 1/10 von PCM), Untertitel kommt an.
- Commit `feat(relay): ...`.

## T6 — Status-TUI + main

- `src/08_status.rs` (`--check/--watch` aus source1 übernehmen, zeigt
  Transport), `main.rs` nur Verdrahtung; `scripts/smoke.sh`.
- Commit `feat(37_live): status-tui und smoke fuer source2`.

## T7 — Frontend

- `public/index.html` (Markup/Style) + `public/app.js` (Logik): Selects
  „Ich spreche"/„Partner hört" aus `/api/languages` (Default de/zh-Hans),
  Radio „Normal (PCM)"/„Sparmodus (Opus SILK)", Key-Hinweis, PCM-Pfad
  mit `translationConfig`, Opus-Pfad mit WebCodecs, gapless Queue,
  Untertitel, kbit/s-Anzeige, `?gemini=`-Override nur für Tests.
- Smoke erweitert (Seite + app.js ausgeliefert).
- Commit `feat(37_live): frontend mit sparmodus ...`.

## T8 — Automatischer Browser-Test (Fake-Gemini)

- `scripts/browser-setup.sh` (Chrome for Testing, playwright-core, ws,
  Fake-Mikro aus `chinese/`, `ffmpeg` per apt), `scripts/browser-fake.sh`
  + `.js`: Tab A Opus, Tab B PCM gegen Fake-Gemini; PASS wenn A
  WebCodecs-Pakete sendet, B Audio + Untertitel bekommt, A Opus-Audio
  bekommt, kbit/s A < 40.
- Commit `test(37_live): automatischer browser-test ...`.

## T9 — HIL mit echtem Gemini

- `scripts/hil-real.sh`: wie T8, echtes Gemini, Key aus Datei
  (maskiert); Untertitel zh→de prüfen. Ergebnisse protokollieren.
- Commit nur bei Fix/Skript.

## T10 — Abschluss

- Gates + smoke + Browser-Test grün, `cargo upgrade --dry-run`,
  README, `deps.md` final, `walkthrough.md` (Regeln plan.md §8) in
  `plan/20261027_01_silk/`. Commit `docs(37_live): walkthrough ...`.
