## Goal

Re-express a minimum viable product (MVP) slice of `rs-summarizer` (YouTube-transcript summarization web app) in the Lisp input language of `cl-rust-generator`, demonstrating how Lisp macros, small reusable tools, and literate structure produce readable, maintainable, extensible Rust. Generated output goes to `examples/22_...`.

## Success Criteria

- `examples/22_<name>/gen.lisp` generates a Rust crate that compiles with `cargo build`, passes `cargo clippy` without new warnings, and is `cargo fmt`-clean.
- The MVP serves at least: submit transcript + model → queued generation → poll status → read summary, backed by the **same SQLite schema** as `rs-summarizer` (row-compatible).
- New `rs.lisp` forms added for the MVP each have unit tests in `transpiler-tests.lisp` and appear in regenerated `SUPPORTED_FORMS.md`.
- `plan/20260906_01_rs_summarizer/walkthrough.md` records results and findings; changes committed.

## Context And Current Facts

**`rs-summarizer` (v1.8.0, edition 2024)** — sources verified by read/search this run:

- Entry: `src/main.rs` (`#[tokio::main]`, `export-db` CLI branch, WAL DB init, stale-generation recovery, queued-resume loop, axum serve with graceful shutdown).
- `src/lib.rs::build_router`: 7 routes (`GET /`, `POST /process_transcript`, `POST /generations/{id}`, `POST .../retry`, `GET /browse`, `POST /summaries/{id}/rate`, `POST /search`; `ServeDir /static`).
- `src/models.rs`: `ThinkingPreference` (5 variants, lowercase DB spelling), `Summary` (~40 fields, incl. `embedding: Option<Vec<u8>>`, token counts, generation epoch/attempt state machine), `SubmitForm` and friends.
- `src/db.rs`: `init_db` (WAL, `sqlx::migrate!`), `insert_new_summary`, `fetch_summary`, chunk appends incl. epoch-guarded CAS (`append_summary_chunk_for_epoch`), `transition_generation` (single-statement CAS state machine), `retry_generation`, `recover_stale_generations`, `fetch_queued_generations`.
- `src/state.rs`: `AppState` (`SqlitePool`, `model_options`, per-model locks/counters, dedup + download limiter, optional `nn_mapper` under `nn-mapper` feature, `viz_data`).
- Services: `summary.rs` (Gemini streaming via `gemini-rust`, `InteractionAccumulator` pure event fold), `embedding.rs` (`embed_text` + `cosine_similarity` with Matryoshka truncation), plus transcript, rate_limiter, download_limiter, deduplication, hacker_news, optional nn_mapper.
- Dependencies (~20): tokio full, axum, sqlx sqlite, gemini-rust, async-openai, askama, reqwest, serde, chrono, tracing, tower-http, regex, pulldown-cmark, vtt, futures-util, thiserror/anyhow, optional burn/cubecl/fast-umap stack.

**`cl-rust-generator`** — verified this run (`rs.lisp`, `README.org`, `SUPPORTED_FORMS.md`, `transpiler-tests.lisp`, `examples/21_mandelbrot/gen00.lisp`):

- Shallow syntax transformer (`emit-rs`/`write-source`); `-` → `:`, `--` → `::`; strings pass through verbatim (escape hatch for attributes, generics, macros).
- Supported: literals, arithmetic/bit/compare ops, `=`/`setf`/`incf`/`decf`, `ref`/`ref-mut`/`deref`/`coerce`, collections, `dot`/`aref`/full `range` family, `if`/`when`/`unless`/`if-let`/`while-let`/`while`/`loop`/`for`/`dotimes`/`case`/`return`, `do0`/`progn`/`block`/`stmt`/`unsafe`/`extern`, `let` (immutable) vs `let*` (mutable) + per-var `declare`, `defun`/`lambda`/calls, `defstruct0`/`deftrait`/`impl`/`make-instance`/`use`/`mod`, `deftype`.
- Gaps found by reading `parse-defun` and searching `rs.lisp`: **no `async`/`await` support** (search for `async|await` in `rs.lisp` returns nothing), no generics/`where` clauses on `defun`, no `?` dedicated form documented in code (README lists `?` under references — verify before relying on it). Attributes/derives are plain strings.
- Example convention: `examples/NN_<name>/genNN.lisp` with `*source-dir*`, `write-source`, Lisp helper functions (e.g. `lprint`) generating repetitive Rust; `*omit-redundant-parens*` bound for readable output.
- No `examples/22_*` exists yet (search for `22` in examples finds only latency-doc mentions) — the number is free.

## Constraints And Non-goals

- DB compatibility (if the DB is used): same `summaries` table/columns and generation status spellings as `rs-summarizer` migrations. Legacy rows must still read.
- MVP scope is explicitly delegated ("du kannst selbst entscheiden"); full parity (Hacker News, viz-tool, nn-mapper GPU stack, export-db, ratings, all providers) is a non-goal.
- Dependency reduction is desired but subordinate to maintainability; the plan keeps axum + tokio + sqlx and cuts from the edges.
- API keys live in `/root/.env` and must never be committed; tests use fakes or the suggested `gemini-3.5-flash-lite` quota (500 req/day) sparingly.
- This planning turn writes no Rust/Lisp implementation and commits nothing.

## Key Decisions

1. **MVP slice: submit → queue → poll → read + embedding search.** Covers the app's spine (routes, DB CAS state machine, one AI provider path, cosine search) while deferring streaming-UX, retries UI, ratings, browse, HN, viz, export-db. Rejected: full clone (too large for one transpiler demo) and pure-CLI MVP (would not demonstrate the web/DB forms that matter).
2. **One provider first: Gemini summary + Gemini embeddings.** `summary.rs` and `embedding.rs` are the two AI seams; async-openai/Hetzner/Gemma variants become a second provider arm behind the same Lisp macro. Rejected: starting multi-provider (multiplies new-form surface before the pattern is proven).
3. **New transpiler forms, CL-leaning, minimal set: `defun-async` (or `:async` option on `defun`), `await`, `try-let`/`?` handling, `attr` for attributes/derives.** Verified gap: `parse-defun` emits `fn ...` with no async slot, and `await`/attributes have no form. New forms imitate Common Lisp naming (`defun` + qualifier, like `let`/`let*`). Rejected: raw-string escape hatch for all async code (defeats the readability demo).
4. **DB access via `sqlx::query!`-shaped string hatch + small Lisp macros, not a query DSL.** The CAS `UPDATE` statements in `db.rs` are long single-statement strings; a full SQL DSL is out of scope. Lisp macros generate the repetitive bind lists and status-transition wrappers. Rejected: inventing a SQL s-expr dialect (large, unproven, not needed for MVP).
5. **Templates: `askama` kept, or minimal inline HTML if it complicates generation.** Decision deferred to implementation Task 2 spike; default is keep askama (fewer behavioral deltas). Rejected: deciding now without a generation spike.
6. **Target dir: `examples/22_summarizer/`.** Follows the `NN_name` convention; 22 is the next free number.
7. **Dependency cuts for MVP:** drop `burn`/`cubecl`/`fast-umap` (nn-mapper feature), `vtt` parsing and with it `yt-dlp` auto-download (manual transcript paste only in the MVP — no vtt, no yt-dlp), `pulldown-cmark` (defer rich markdown or use minimal renderer), `fantoccini`/`proptest` dev-deps as appropriate; keep tokio, axum, sqlx, serde, chrono, tracing, reqwest-or-gemini-rust (one AI client), tower-http. Final list confirmed during Task 2 spike.

## Recommended Approach

Build the MVP as a literate Lisp generator crate in three layers, reusing the `21_mandelbrot` pattern (`gen.lisp` + Lisp helpers + `write-source` per Rust file):

1. **Transpiler extensions first** (`rs.lisp` + tests + docs): async `defun`, `await`, attribute/derive form, error-propagation form. Each lands with `transpiler-tests.lisp` cases and regenerated `SUPPORTED_FORMS.md`.
2. **Reusable Lisp toolkit** (`examples/22_summarizer/toolkit.lisp`): macros for axum route tables, `AppState` struct + `Clone` impl, sqlx CAS-transition wrappers, provider-call wrappers with token/cost accounting, HTML-response helpers. These are the "wartbar/erweiterbar" demonstration.
3. **MVP app generation** (`gen.lisp` → `src/main.rs`, `src/db.rs`, `src/routes.rs`, `src/ai.rs`, `Cargo.toml` emitted or checked-in template): submit/queue/poll/read + embedding search against the compatible schema, with unit + integration tests (fake AI service; live `gemini-3.5-flash-lite` smoke test only).

Sequencing follows `tasks.md` (phases 0–6); each phase ends with `cargo build` + target tests before the next begins.

## Work Plan

See `tasks.md` for the checkbox breakdown. Phases:

- **Phase 0 — Baseline & measurement:** record `rs-summarizer` route/service/DB inventory, LOC per module, full dependency list with keep/cut verdict; confirm `/root/.env` key names present (values never logged).
- **Phase 1 — Transpiler forms:** `defun-async`/`await`, `attr`, `?`/`try-let`; tests + docs regeneration.
- **Phase 2 — Toolkit macros:** route table, state, DB CAS wrappers, provider wrapper, response helpers; each with a generated-code readability review (`cargo fmt --check` + human read).
- **Phase 3 — MVP generation:** `gen.lisp` emitting the crate; schema-compat check against real `rs-summarizer` migrations (open legacy DB, read/write one row).
- **Phase 4 — Tests:** Rust unit tests (accumulator-equivalent fold, cosine similarity incl. truncation/zero-vector, state transitions on temp DB) + Lisp-side generation tests; one live `gemini-3.5-flash-lite` smoke test.
- **Phase 5 — Docs & hardening:** README for the example, `SUPPORTED_FORMS.md` regeneration, `cargo clippy`/`fmt` clean, `walkthrough.md` with results and unexpected findings.
- **Phase 6 — Commit:** reviewable split (transpiler forms / toolkit+MVP / docs), commit.

## Validation Plan

- `./run-tests.sh` green after Phase 1 and Phase 5.
- `cargo build`, `cargo test`, `cargo clippy -- -D warnings`, `cargo fmt --check` in `examples/22_summarizer/` after Phases 3–5.
- Schema-compat probe: generated binary opens a copy of a real `rs-summarizer` SQLite DB and round-trips one summary row (Phase 3).
- Live smoke (Phase 4): one summary + one embedding call via `gemini-3.5-flash-lite`; quota use logged, failures recorded as findings not blockers.
- Highest-risk validation: the **async-form design** (Phase 1) — if `await`/`async` cannot be expressed readably, the whole MVP shape changes; spike it first.

## Risks / Rollback

- **Transpiler gaps larger than expected** (generics, lifetimes, complex `where` bounds across axum/sqlx signatures) → fall back to wider string hatch + narrower MVP (drop search or queueing), recorded in walkthrough.
- **DB schema drift** between MVP and upstream migrations → MVP pins a migration snapshot copy; compat probe catches drift.
- **Live-model quota/flakiness** → fakes are the gate; live test is informational only.
- **Scope creep toward full parity** → tasks.md Phase 3 has an explicit cut list; anything beyond needs re-approval.
- Rollback: each phase commits separately; revert to last green phase commit.

## Open Questions

None — scope delegation ("du kannst selbst entscheiden") and MVP framing resolve the plannable unknowns; remaining choices (askama vs inline HTML, final dep list) are spiked in Phase 2/3 with defaults stated above.
