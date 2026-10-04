# Anti-Tinder MVP — Aufgabenliste (`task.md`)

Abarbeitung strikt in Reihenfolge. Jeder Schritt erst dann abgeschlossen, wenn
der genannte Test-Nachweis grün ist. Erst dann weiter.

## Schritt 0 — Infra: Postgres + Projektgerüst
* `docker-compose.yml` (Postgres 16, Volume, Healthcheck), `.env.example`,
  `Dockerfile` (Multi-Stage), `Cargo.toml` (Edition 2024).
* `cargo init`-Gerüst, alle Module als leere Dateien + `main.rs`-Verdrahtung.
* Test-Nachweis: `cargo build` grün, `docker run`/compose Postgres erreichbar,
  `cargo fmt --check` grün.

## Schritt 1 — Config & Telemetry (`01_config.rs`, `07_telemetry.rs`)
* `Config::from_env()` mit allen Keys, sinnvolle Defaults für Dev, klare
  Fehlermeldungen bei fehlenden Secrets.
* `init_tracing()` mit `EnvFilter` + `TraceLayer`-Vorbereitung.
* Test-Nachweis: `cargo test config` (Env-Parsing, Defaults, Fehlerfälle) grün.

## Schritt 2 — DB-Layer + Verschlüsselung (`02_db.rs`, `migrations/001_init.sql`)
* `PgPool`, `run_migrations()` via `sqlx::migrate!`.
* `encrypt_field`/`decrypt_field` (ChaCha20-Poly1305, frische Nonce pro Wert,
  Base64 `nonce|ciphertext`), Key-Laden aus `DATA_ENCRYPTION_KEY`.
* Migration: `users`, `profiles`, `likes`, `daily_matches` + Indizes.
* Test-Nachweis: `cargo test db::` (Roundtrip Encrypt/Decrypt, Tamper schlägt
  fehl, Migration läuft auf leerer Test-DB) grün.

## Schritt 3 — Modelle (`03_models.rs`)
* `User`, `Profile`, `PublicProfile::from_profile` (Hidden-Felder nie enthalten),
  `Like`, `DailyMatch`, Formular-Structs mit `serde::Deserialize`.
* Konvertierungen DB-Row ↔ Struct (`FromRow`), Validierung (Alter ≥ 18, MBTI aus
  16 Typen, Pflichtfelder).
* Test-Nachweis: `cargo test models::` (Validierung, Public-Mapping ohne Leaks) grün.

## Schritt 4 — Scoring-Algorithmus (`04_scoring.rs`) — Unit-Tests zwingend
* Harte Filter (Alter/Geschlecht/Kinderwunsch → 0), MBTI-Matrix (35 P.),
  Hobby-Jaccard (35 P.), Hidden-Passung (30 P.), Score-Clamp 0–100.
* Reine Funktionen, keine DB-Abhängigkeit (Dependency Injection der Profile).
* Test-Nachweis: `cargo test scoring::` grün, darunter: Dealbreaker→0,
  identische Profile→hoch, disjunkte Hobbys→niedrig, MBTI-Matrix-Spots,
  Determinismus, Randomized Property-Sweep (900 Kombis, Score immer 0–100).
  (Hinweis: Der Score ist richtungsabhängig — keine Symmetrie per Design.)

## Schritt 5 — AI-Check (`08_ai_check.rs`)
* `AiChecker`-Trait (`async fn check(&self, text: &str) -> bool`),
  `MockAiChecker` (blockt URLs, OnlyFans/Telegram/CashApp-Muster, Spam-Wörter),
  `HttpAiChecker`-Stub (via `reqwest`, durch `AI_MODE=http` aktivierbar).
* Test-Nachweis: `cargo test ai_check::` (saubere Texte → true, Spam/Links →
  false, leere/große Eingaben) grün.

## Schritt 6 — Payments/Stripe-Mock (`09_payments.rs` + `05_handlers_auth.rs`)
* Checkout-Session erzeugen (Mock-ID), Success/Cancel-Routen, `paid`-Flag,
  Registrierungs-Gate (ohne `paid` kein Profil/Match-Zugriff).
* Passwort-Hashing (argon2), signierte Cookie-Session.
* Test-Nachweis: `cargo test` (Checkout-Flow, Gate blockt Unbezahlte, Login
  roundtrip) grün.

## Schritt 7 — Profile & Views (`06_handlers_profile.rs`, `11_views.rs`, `templates/`)
* Profil-CRUD mit AI-Check vor Speichern, Hidden-Felder verschlüsselt speichern.
* Öffentliche Ansicht: Hidden-Felder nur als 🔒 Tresor-Platzhalter (nie im HTML!).
* Askama-Templates rendern, `AppError → IntoResponse` (404/500-Seiten).
* Test-Nachweis: `cargo test` (AI-Block speichert nicht, View-HTML enthält keine
  Hidden-Klartexte, Tresor-Symbol vorhanden) grün.

## Schritt 8 — Matching-Job & Likes (`10_matching.rs`, `12_routes.rs`)
* `compute_top_matches` (lädt Kandidaten, scored, Top-5 nach `daily_matches`),
  `run_daily_task` (tokio-Intervall), `POST /matches/recompute`.
* Like + gegenseitiges Match → Signal-Kontakt entschlüsselt anzeigen.
* Router mit `GovernorLayer` (Rate-Limit) + `TraceLayer`, State-Verdrahtung.
* Test-Nachweis: `cargo test matching::` (Top-5-Sortierung/Limit, Mutual-Erkennung,
  Rate-Limit-Config baut) grün.

## Schritt 9 — E2E-Setup (`e2e_tests/`)
* `cd e2e_tests && uv init && uv add pytest playwright pytest-playwright &&
  uv run playwright install chromium` (ggf. `--with-deps` via Systempakete).
* `conftest.py` (Server-Spawn mit Test-DB, DB-Reset), `test_flows.py`:
  1. Registrierung → Paywall-Mock → Login.
  2. Profil anlegen → öffentliche Ansicht zeigt 🔒, kein Hidden-Klartext im DOM.
  3. Matches-Seite lädt HTMX-Partial (`#matches-list` befüllt).
* Test-Nachweis: `uv run pytest` grün (Server als Hintergrundprozess + leere
  Test-DB). Bei GUI-Bedarf `xvfb-run`.

## Schritt 10 — Qualität & Abschluss
* `cargo fmt`, `cargo clippy -- -D warnings`, `cargo upgrade` (Breaking Changes
  prüfen), `cargo test` voll grün, `uv run pytest` grün.
* `plan/walkthrough.md` auf Deutsch (Was/Architektur-Entscheidungen/Learnings/
  CLI-Tools fürs Dockerfile, Mermaid: Architektur + User-Flow).
* Test-Nachweis: alle Gates grün, Walkthrough abgelegt.
