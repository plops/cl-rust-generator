# Dependencies (`deps.md`)

Recherche via **DeepWiki MCP** (`ask_wiki_question` gegen das offizielle GitHub-Repo).
Stand: 2026-10-01. Es werden ausnahmslos die neuesten Versionen per `cargo add` / `cargo upgrade` verwendet.

## `algesten/ureq` — Blocking HTTP-Client (MCP-Transport)
- **Einsatzzweck:** Streamable-HTTP-POSTs an `https://mcp.deepwiki.com/mcp` (JSON-RPC 2.0),
  Custom-Header (`Content-Type`, `Accept`, `Mcp-Session-Id`), Response-Header + Body als String lesen.
- **DeepWiki-Erkenntnisse:**
  - Blocking API: `ureq::post(url).header(k, v).send_json(&body)?` sendet JSON und setzt
    `Content-Type: application/json; charset=utf-8` automatisch.
  - Response: `response.status()`, `response.header(name)`, `response.into_reader()` + `Read::read_to_string`.
  - Feature `json` in `Cargo.toml` aktivieren: `ureq = { version = "3", features = ["json"] }`.
  - Fehler via `Result<_, ureq::Error>`; HTTP-Statusfehler sind als `Error::StatusCode(code)` abfangbar.
- **Eigenheiten / Best Practices:**
  - Kein async nötig — passt zum seriellen Rate-Limit-Loop (1,5–2 s Delay).
  - SSE-Antworten (`text/event-stream`) manuell parsen: Zeilen mit `data: {...}` extrahieren,
    `: ping`-Keepalives ignorieren. `send_json` reicht; kein SSE-Client nötig.
- **Codebeispiel (gekürzt, aus DeepWiki-Antwort):**
  ```rust
  let mut body = String::new();
  ureq::post(url)
      .header("Accept", "application/json, text/event-stream")
      .send_json(&payload)?
      .into_reader()
      .read_to_string(&mut body)?;
  ```

## `serde-rs/serde` — Serialize/Deserialize-Derives (JSON-RPC-Typen)
- **Einsatzzweck:** Typisierte JSON-RPC-2.0-Structs (`JsonRpcRequest`, `JsonRpcResponse`, `JsonRpcError`),
  MCP-Params/Results, optionale Felder.
- **DeepWiki-Erkenntnisse:**
  - `serde = { version = "1.0", features = ["derive"] }` aktiviert `#[derive(Serialize, Deserialize)]`.
  - Optionale Felder: `Option<T>` + `#[serde(default)]`; beim Serialisieren auslassen via
    `#[serde(skip_serializing_if = "Option::is_none")]` (ideal für `result`/`error` in JSON-RPC).
  - `params`/`result` als `serde_json::Value` für flexible MCP-Payloads.
- **Codebeispiel:**
  ```rust
  #[derive(Serialize, Deserialize, Debug)]
  pub struct JsonRpcResponse {
      pub jsonrpc: String,
      #[serde(skip_serializing_if = "Option::is_none")]
      pub result: Option<Value>,
      #[serde(skip_serializing_if = "Option::is_none")]
      pub error: Option<JsonRpcError>,
      pub id: u64,
  }
  ```

## `serde-rs/json` (`serde_json`) — JSON-Aufbau & -Parsing
- **Einsatzzweck:** `json!`-Macro für Request-Payloads, `from_str` für SSE-`data:`-Zeilen,
  `Value`-Zugriffe (`get`/Index), `to_string` für Debug/Fehlerausgaben.
- **DeepWiki-Erkenntnisse:**
  - `json!({"name": name, "age": age})` interpoliert Rust-Variablen direkt.
  - `serde_json::from_str::<Value>(s)?` liefert `Result<T, serde_json::Error>`.
  - Sicherer Zugriff via `.get("a").and_then(|v| v.get("b"))` (`Option<&Value>`);
    Index-Zugriff liefert bei fehlendem Key `Value::Null` statt Panic.
  - Fehler via `match` auf `serde_json::Error` (Syntax/I/O/Daten) — kein Panic-Pfad.
- **Codebeispiel:**
  ```rust
  let v: Value = serde_json::from_str(data)?;
  let text = v.get("result").and_then(|r| r.get("content"));
  ```

## `chronotope/chrono` — Zeitstempel für Output-Dateinamen
- **Einsatzzweck:** Aktueller lokaler Zeitstempel im Format `YYYY-MM-DD_HH-mm-ss`
  für `algos_<datetime>.md` / `not-index-yet_<datetime>.md`.
- **DeepWiki-Erkenntnisse:**
  - `chrono::Local::now()` liefert `DateTime<Local>`.
  - `now.format("%Y-%m-%d_%H-%M-%S").to_string()` erzeugt exakt das geforderte Format
    (`%Y` Jahr, `%m` Monat, `%d` Tag, `%H`/`%M`/`%S` Stunde/Minute/Sekunde).
- **Codebeispiel:**
  ```rust
  use chrono::Local;
  let stamp = Local::now().format("%Y-%m-%d_%H-%M-%S").to_string();
  ```

## Bewusst NICHT gewählt
- **Kein async-Runtime (`tokio`/`reqwest`):** Der serielle Analyse-Loop mit festem Delay braucht
  keine Nebenläufigkeit; `ureq` (blocking) hält Build klein und Code einfach.
- **Kein `regex`:** Das `owner / repo`-Muster ist mit manuellem `split('/')`-Parsing + Validierung
  (alphanumerisch, `-_.`) robust und ohne extra Dependency lösbar.
- **Kein `clap`:** Nur ein optionales Positionsargument (Dateipfad, sonst `stdin`);
  `std::env::args` reicht und spart Dependencies.
