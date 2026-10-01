# Aufgabenliste (`task.md`): GitHub-Trending → DeepWiki-Algo-Analyse

## Phase 0 — Recherche & Planung
- [x] DeepWiki-MCP-Recherche für `ureq`, `serde`, `serde_json`, `chrono`
- [x] `deps.md` erstellen (GitHub-Notation + Beispiele + Einsatzzweck)
- [x] `plan.md` erstellen (Kontext, Dependencies, Architektur, Git-Strategie)
- [x] `task.md` erstellen (diese Checkliste)
- [x] Live-Probes: `initialize`, `tools/list`, `tools/call` (Erfolg + not-indexed-Form)

## Phase 1 — Projektgerüst
- [x] Cargo-Projekt initialisieren (Bin, Edition 2024, Name `github-trending-algos`)
- [x] Neueste Dependency-Versionen via `cargo add` (`ureq` + `json`, `serde` + `derive`, `serde_json`, `chrono`)
- [x] `cargo fmt`, `cargo clippy --all-targets --all-features -- -D warnings`, `cargo test` grün (Baseline)

## Phase 2 — Typen & Config (`01_types.rs`, `02_config.rs`)
- [x] API-/Typ-Definition: `Repo`, `AnalysisOutcome` (Ok/Missing/Failed), `RunReport`, Fehler-Enum
- [x] Config: Endpoint, `protocolVersion`, Delay (Default 1750 ms, `--delay-ms`/`DEEPWIKI_DELAY_MS`), Frage-Prompt-Template (exakt laut Prompt)
- [x] Unit-Tests: Prompt enthält alle Pflichtblöcke (GitHub/DeepWiki-Links, 3 Algorithmen, Mermaid, Notes, Deutsch-Hinweis)

## Phase 3 — Parser (`03_parser.rs`)
- [x] Implementierung: `owner / repo`-Extraktion (mit/ohne Spaces, `owner/repo`), Validierung, Dedup (Order-preserving)
- [x] Unit-Tests: Beispiel-Feed, Duplikate, ungültige Zeilen (`Built by @…`, Footer, leere), Bindestriche/Punkte/Unterstriche
- [x] Integrationstest: `example_input.txt` → exakt 22 Repos in Feed-Reihenfolge

## Phase 4 — MCP-Client (`04_mcp_protocol.rs`, `05_tool_eval.rs`, `06_mcp_client.rs`)
- [x] Implementierung: `initialize` → Session-ID speichern → `notifications/initialized` → `tools/call`
  (`ask_wiki_question` + Fallback `ask_question`); SSE-Parser (`data:`-Zeilen, Ping-Ignore, reines JSON ok)
- [x] Implementierung: not-indexed-Matcher (`isError`, Pflicht-Phrasen + `not found`/`to index`-Varianten, HTTP 404/500, RPC-`error`); 429-Handling ohne Abbruch
- [x] Unit-Tests: SSE-Fixtures (Erfolg, Ping+Erfolg, reines JSON), Matcher-Matrix (alle Phrasen, Case-Insensitivität), Fehler-Isolation
- [x] Mock-Integrationstest: Parser→Client-Response-Auswertung ohne Netzwerk

## Phase 5 — Pipeline (`07_pipeline.rs`)
- [x] Implementierung: serieller Loop mit `sleep(delay)` zwischen Requests, Fortschritts-Log `[i/n] repo … OK/MISSING/ERROR`
- [x] Unit-Tests: Fortschritts-Format, Delay aufrufbar/konfigurierbar (injizierbarer Sleeper), Fehler bricht Loop nicht ab

## Phase 6 — Output (`08_output.rs`)
- [x] Implementierung: `algos_<datetime>.md` (`---`-getrennt), `not-index-yet_<datetime>.md` (Checkliste mit GitHub+DeepWiki-Links)
- [x] Unit-Tests: Dateinamen-Format (`%Y-%m-%d_%H-%M-%S`), Separator, Links, leere-Mengen-Verhalten

## Phase 7 — Verdrahtung & Qualität
- [x] `main.rs`: nur Args/stdin-Verdrahtung + Exit-Codes, keine Geschäftslogik
- [x] `cargo fmt --all`, `cargo clippy --all-targets --all-features -- -D warnings`, `cargo test` grün
- [x] Smoke-Test: `cargo run -- plan/20260110_01_ask_deepwiki/example_input.txt` (kleine Repo-Auswahl, live) + `stdin`-Pipe-Variante

## Phase 8 — Abschluss
- [x] Atomare Conventional-Commits (s. `plan.md`)
- [x] `walkthrough.md` auf Deutsch (Was/Architektur/Learnings/Docker-Updates, Mermaid-Diagramme, Snippets)
