# Tasks — seriell, jeder Schritt getestet (37_live/source1)

Legende: jeder Task endet mit Gate `fmt+clippy+test`; Commit nach
Conventional Commits (siehe [plan.md](./plan.md#4-commit-konvention)).
Modi: `de-to-zh`, `zh-to-de`, `en-to-zh`, `zh-to-en` (Tabelle in
`01_config.rs`, Anzeige im Frontend + `/api/modes`).

## T1 — Grundgerüst (Implementierung)

- `source1/` mit `Cargo.toml` (Edition 2024, `axum 0.8`, `tokio full`,
  `tower-http fs`, `tracing`, `futures-util`, `serde`), `src/main.rs`,
  `public/`, `deps.md`, `scripts/smoke.sh`-Skelett.
- Gate: `cargo build` grün.
- Commit: `feat(scaffold): ...`.

## T2 — Config + Modi-Tabelle (Implementierung + Host-Test)

- `src/01_config.rs`: `Cli { listen, dir }`, `MODES: [Mode; 4]`
  (`id`, `label_de`, `instruction_en`, `voice`), `/api/modes`-Quelle.
- Test `tests/modes.rs`: 4 Modi, IDs stabil, jede Instruktion enthält
  `ONLY` (kein Plaudern), Stimmen aus `{Aoede, Puck, Charon, Fenrir, Kore}`.
- Gate: `cargo test --test modes`.
- Commit: `feat(config): ...`.

## T3 — Rooms-Registry (Implementierung + Host-Test)

- `src/02_state.rs`: `Rooms` (`tokio::sync::Mutex<HashMap<String,
  HashMap<u64, mpsc::Sender<Vec<u8>>>>>`), `join/leave/peers`,
  leere Räume löschen, `max_peers_per_room = 8`.
- Unit-Tests in Datei: Join/Leave, Raum-Auflösung, Limit.
- Commit: `feat(rooms): ...`.

## T4 — Relay-Broadcast (Implementierung + Host-Test)

- `src/03_relay.rs`: `GET /ws?room=` → axum-Upgrade, Binary an alle
  Peers außer Sender, Slow-Consumer (`try_send`) überspringen,
  Text `{type:"hello"}` ok, Close räumt auf.
- Test `tests/ws_relay.rs`: A→B ankommt, A bekommt kein Echo,
  Raum-Isolation (C in anderem Raum hört nichts).
- Commit: `feat(relay): ...`.

## T5 — Serve + Health (Implementierung + Host-Test)

- `src/04_serve.rs`: `GET /` (statisch), `/healthz` (`ok`),
  `/api/rooms` (`{room: n}`), `main.rs` nur Verdrahtung.
- Test: healthz + rooms-Endpunkte per `axum-test`-freiem `reqwest`-freien
  Weg (tokio + `TcpListener` auf Port 0, plain `http` via `tokio::io`).
  Falls zu aufwendig: Test gegen Router-Handler direkt.
- Commit: `feat(serve): ...`.

## T6 — Frontend (Implementierung, je Modus verifiziert)

- `public/index.html` aus `plan/misc/live.md` portieren:
  4 `<option>`-Einträge = `MODES`-IDs, Relay-URL `/ws?room=`,
  Gemini-URL `...BidiGenerateContent?key=`, Mic 16 kHz, Queue 24 kHz,
  `localStorage`-Key, `echoCancellation:true`.
- Host-Test: `scripts/smoke.sh` (build, serve, `curl /healthz`,
  statische Seite enthält alle 4 Modus-IDs).
- Commits: `feat(frontend): ...` (ein Commit, 4 Modi in Smoke geprüft).

## T7 — HIL-Nachweis je Modus (manuell, protokolliert)

- Für jeden Modus `de-to-zh`, `zh-to-de`, `en-to-zh`, `zh-to-en`:
  2 Tabs/Browser (Linux + Android/iPhone oder 2 Tabs via xvfb/`:0`),
  gleicher Raum, je Tab Gegen-Modus; Nachweis: Partner hört TTS,
  kein Selbst-Echo. Ergebnis in Walkthrough-Tabelle.
- Kein Commit (nur Doku), außer Fix nötig → `fix(frontend|relay): ...`.

## T8 — TUI-Status (Implementierung + Host-Test, NACH allen Modi)

- `--check ROOM?`-Einmalabfrage + `--watch`-Loop gegen `/api/rooms`
  und `/healthz`, nur `std` (kein Ratatui), Ausgabe
  `raum: n peers`.
- Test: `--check` gegen Test-Server (Exit 0, Raum sichtbar).
- Commit: `feat(tui): ...`.

## T9 — Finale Gates + Walkthrough

- `cargo fmt && cargo clippy -- -D warnings && cargo test &&
  scripts/smoke.sh` alles grün, `deps.md`-Pfade final,
  `cargo upgrade --dry-run` geprüft (neueste gewollt).
- `walkthrough.md` (Deutsch, Mermaid, Glossar, Struktur 1–4 aus
  prompt.txt) nach `20261023_01_first/walkthrough.md` + Kopie
  `plan/20261023_01_first/walkthrough.md`.
- Commit: `docs(walkthrough): ...`.
