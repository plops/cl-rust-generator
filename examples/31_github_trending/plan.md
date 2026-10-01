# Implementierungsplan (`plan.md`): GitHub-Trending → DeepWiki-Algo-Analyse (CLI)

## 1. Ziel & Kontext
- Eigenständiges Rust-CLI (Edition 2024): liest GitHub-Trending-Text (Datei oder `stdin`),
  extrahiert `owner/repo`-Paare, fragt pro Repo das **DeepWiki MCP** (Streamable HTTP, JSON-RPC 2.0)
  mit einem deutschen Analyse-Prompt ab und schreibt zwei Zeitstempel-Dateien:
  `algos_<datetime>.md` (Erfolge, getrennt durch `---`) und `not-index-yet_<datetime>.md` (Checkliste).
- Robustheit: nie abstürzen (nicht-indizierte Repos, HTTP 404/500/429, Netzwerkfehler);
  Rate-Limit-Delay 1,5–2 s (konfigurierbar); Fortschritts-Log `[i/n] … OK/MISSING/ERROR`.
- Auftrag: [prompt.txt](plan/20260110_01_ask_deepwiki/prompt.txt:80),
  Beispiel-Input: [example_input.txt](plan/20260110_01_ask_deepwiki/example_input.txt:17).

## 2. Kontext-Dateien
| Datei | Beschreibung |
|---|---|
| `plan/20260110_01_ask_deepwiki/prompt.txt` | Voller Auftrag (Systemrahmen + Projektanforderungen) |
| `plan/20260110_01_ask_deepwiki/example_input.txt` | Realer Trending-Feed (22 Rust-Repos, Format `owner / repo`) |
| `deps.md` | DeepWiki-recherchierte Dependencies mit Codebeispielen |
| `task.md` | Serielle Checkliste zur Umsetzung |
| `Cargo.toml` / `src/*.rs` | Zu erstellendes CLI (s. Architektur) |
| `tests/*.rs` | Integrations-/Smoke-Tests (Parser, SSE, Writer, Prompt) |

## 3. Dependency-Plan (via DeepWiki geprüft, s. `deps.md`)
- `ureq` (`algesten/ureq`, Feature `json`): blocking POST, Custom-Header, Body als String.
  Kein async nötig; SSE wird manuell aus `data:`-Zeilen geparst.
- `serde` (`serde-rs/serde`, Feature `derive`): JSON-RPC-Structs mit `Option` + `skip_serializing_if`.
- `serde_json` (`serde-rs/json`): `json!`-Payloads, `from_str::<Value>`, sichere `.get()`-Zugriffe.
- `chrono` (`chronotope/chrono`): `Local::now().format("%Y-%m-%d_%H-%M-%S")` für Dateinamen.
- Bewusst weggelassen: `tokio`/`reqwest` (kein async nötig), `regex` (manuelles Parsing reicht),
  `clap` (nur 1 optionales Arg; `std::env::args` genügt). Details in `deps.md`.

## 4. Architektur-Entscheidungen
- **Modul-Datenfluss (Nummernpräfix = Initialisierungs-/Datenflussreihenfolge):**
  `01_types` (Repo, Ergebnisse, Fehler) → `02_config` (Endpoint, Delay, Prompt-Template) →
  `03_parser` (Text → dedup. Repos) → `04_mcp` (JSON-RPC/SSE, Session-ID, ask-Tool) →
  `05_pipeline` (Loop mit Delay + Fortschritts-Log) → `06_output` (Zeitstempel-Dateien) →
  `main.rs` (nur Verdrahtung: Args/stdin, Exit-Codes, keine Geschäftslogik).
- **300-Zeilen-Regel:** Jedes Modul genau eine Zuständigkeit; Typen + zugehörige Unit-Tests
  in derselben Datei; `main.rs`/`lib.rs`-Verdrahtung ohne Logik.
- **MCP-Protokoll (verifiziert via `curl`-Probes gegen `https://mcp.deepwiki.com/mcp`):**
  1. `initialize` (`protocolVersion: "2024-11-05"`) → Antwort ist SSE (`event: message`, `data: {jsonrpc…}`),
     ggf. `: ping`-Keepalives; `Mcp-Session-Id`-Header speichern, falls vorhanden.
  2. `notifications/initialized` (Notification ohne `id`, Antwort ignorieren).
  3. `tools/call` mit `name: "ask_wiki_question"` (realer Server-Name; Prompt nennt `ask_question`
     → Fallback: bei `Method not found` einmal mit Alternativnamen wiederholen),
     `arguments: {repoName, question}`.
- **Nicht-indiziert-Erkennung (niemals Crash):** `isError: true` ODER Text-Match (case-insensitive)
  auf `not indexed` / `is not indexed` / `isn't indexed` (Pflicht laut Prompt) plus robuste
  Erweiterungen `not found`, `to index`, `index it` (real beobachtete Servermeldung:
  `"Repository not found. Visit https://deepwiki.com to index it."`) ODER HTTP 404/500
  ODER JSON-RPC-`error`. Jede Repo-Anfrage ist in sich fehlerisoliert.
- **SSE-Parsing:** Body zeilenweise; nur `data:`-Zeilen (ohne `data:`-Präfix, getrimmt) als
  JSON-Kandidaten; `[DONE]`/`leere` Zeilen überspringen; erstes erfolgreich geparstes
  `data:`-JSON mit passender/fallback-`id` gewinnt. Auch reines JSON (nicht-SSE) wird akzeptiert.
- **Rate Limiting:** `std::thread::sleep(delay)` zwischen Anfragen (Default 1750 ms,
  per `--delay-ms` / `DEEPWIKI_DELAY_MS` konfigurierbar); HTTP 429 wird als transienter
  Fehler geloggt und bricht den Lauf nicht ab.
- **I/O:** `cargo run -- <datei>` oder `stdin`-Pipe; Erkennung via `std::env::args` (genau ein
  Positionsargument = Pfad; sonst `stdin` lesen). Output-Dateien im CWD mit
  `Local::now()`-Stempel; `algos_*.md`-Blöcke mit `\n\n---\n\n` getrennt.
- **Teststrategie:** Unit-Tests pro Modul (Parser-Edge-Cases, SSE-Varianten, not-indexed-Matcher,
  Dateinamen-Format, Prompt-Enthält-Pflichtblöcke); Integrationstests ohne Netzwerk
  (Parser+Writer gegen `example_input.txt`-Kopie, Mock-MCP-Parser); Smoke-Test optional mit
  `--smoke-live` (1 echtes Repo, manuell).

## 5. Git-Strategie (Conventional Commits, atomar)
1. `docs(plan): add deps/plan/task for deepwiki trending analyzer`
2. `feat(cli): scaffold cargo project with latest deps`
3. `feat(parser): add owner/repo extractor with dedup`
4. `feat(mcp): add streamable-http json-rpc client with sse parsing`
5. `feat(pipeline): add rate-limited analysis loop with progress log`
6. `feat(output): add timestamped algos/not-index-yet writers`
7. `test(all): add unit and integration tests`
8. `docs(walkthrough): add german walkthrough with mermaid diagrams`
