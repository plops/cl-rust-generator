# Dependencies (`deps.md`)

Diese Datei trackt die verwendeten Bibliotheken und ihre GitHub-Repositories im Format `<organization>/<project>`, um die automatisierte Dokumentationssuche via DeepWiki MCP zu ermöglichen.

## Backend Web-Framework & Runtime
* `tokio-rs/axum` - Das extrem schnelle, ressourcenschonende Web-Framework für unsere HTTP-Routen.
* `tokio-rs/tokio` - Die asynchrone Runtime, die unter Axum läuft.
* `tower-rs/tower` - Middleware-Ökosystem (benötigt für Rate-Limiting, Timeouts, etc. in Axum).
* `tower-rs/tower-http` - HTTP-spezifische Middleware (Tracing, Compression, CORS).

## Datenbank & SQL
* `launchbadge/sqlx` - Typsichere, asynchrone SQL-Queries für unsere PostgreSQL Datenbank ohne schweres ORM.

## Frontend & Templating
* `bigskysoftware/htmx` - Erzeugt das Single-Page-Application (SPA) Gefühl, indem es HTML direkt vom Server per AJAX nachlädt.
* `askama-rs/askama` - Typsichere, kompilierte HTML-Templates für Rust (rasend schnell, fängt Template-Fehler zur Compile-Zeit ab).

## Sicherheit, Anti-Abuse & Verschlüsselung
* `RustCrypto/AEADs` - (Spezifisch das Crate `chacha20poly1305`) Für die symmetrische Verschlüsselung der hochsensiblen Profildaten (Einkommen, Vorlieben) in der Datenbank.
* `benwis/tower-governor` - Für das Rate-Limiting in Axum (verhindert, dass Bots Spam-Anfragen schicken oder Hunderte Profile scrapen).
* `RustCrypto/password-hashing` - (Crate `argon2`) Für sicheres Passwort-Hashing bei der Registrierung.

## API-Kommunikation (LLM Check & Stripe)
* `seanmonstar/reqwest` - Der Standard-HTTP-Client in Rust. Wird benötigt, um die Profiltexte asynchron zur OpenAI/Anthropic-API für den "AI-Spam-Check" zu senden und um mit der Stripe-API zu kommunizieren.
* `serde-rs/serde` - (Zusammen mit `serde_json`) Für das De-/Serialisieren von JSON-Daten (API-Antworten vom LLM, Stripe).

## Observability & Logging (Telemetrie)
* `tokio-rs/tracing` - Das strukturierte Logging-Framework, um das MVP zu überwachen, Fehler beim Matching-Algorithmus zu finden und anonymisierte Nutzerflüsse zu tracken.

## Konfiguration
* `allan2/dotenvy` - Um `.env` Dateien beim lokalen Entwickeln und im Docker-Container zu laden (für Stripe-Keys, LLM-Keys, Database-URL).

## End-to-End Testing (Python Tooling)
* `astral-sh/uv` - Ein extrem schneller Python-Paket- und Projektmanager (geschrieben in Rust), der pip/virtualenv ersetzt.
* `microsoft/playwright-python` - Das moderne Framework für Headless-Browser-Testing. Ideal für HTMX, da es Auto-Waiting für asynchrone DOM-Updates beherrscht.
* `pytest-dev/pytest` - Das Standard-Testing-Framework in Python zur Strukturierung der Playwright-Tests.
