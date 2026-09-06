# Tasks: rs-summarizer MVP in cl-rust-generator (20260906_01_rs_summarizer)

Source: `prompt.txt`. Plan: `implementation_plan.md`. Target: `examples/22_summarizer/`.
Keep each phase's context narrow; finish + verify one phase before starting the next.

## Phase 0 — Baseline & measurement — DONE (2026-09-06, shell unavailable so LOC by inspection only)

- [x] Inventory `rs-summarizer`: 7 routes (`lib.rs::build_router`), 8 services, ~19 `db.rs` fns, 7 askama templates (`templates.rs`), 6 error enums (`errors.rs`). Full fn list in walkthrough §Phase 0.
- [x] Dependency verdicts: keep tokio/axum/sqlx/serde/chrono/tracing/gemini-rust/reqwest/tower-http/anyhow-thiserror; cut burn/cubecl/fast-umap/vtt/pulldown-cmark/fantoccini-proptest; defer askama-vs-inline-html to Phase 2 spike.
- [ ] Confirm `/root/.env` key names exist — DEFERRED to Phase 4 (no shell; secrets never read into context).
- [x] Verify `examples/22_summarizer/` is free (no `22_*` match in examples/).

## Phase 1 — Transpiler forms (`rs.lisp`) — DONE (2026-09-06, 152/152 tests)

- [x] `defun-async` (`parse-defun` gained `:async`) + `await` form.
- [x] `attr` form for attributes/derives.
- [x] `?` error-propagation form (README claim was aspirational — no form existed; now implemented + tested).
- [x] 5 unit tests in `transpiler-tests.lisp`; `./run-tests.sh` green (152/152, incl. rustfmt + rustc value tests).
- [x] `SUPPORTED_FORMS.md` regenerated; `README.org` updated (table + async section, example output verified).

## Phase 2 — Lisp toolkit (`examples/22_summarizer/toolkit.lisp`) — DONE (2026-09-06)

- [x] Route-table builder `routes` (axum `Router` + handler list; `Router::new()` head is a verbatim string since `dot` adds no call parens).
- [x] Handler builder `handler` (`defun-async` + injected `State(app)` first param).
- [x] Struct builder `model` (`attr` derive set + `defstruct0`).
- [x] DB builders `db-exec` / `db-fetch-option` (sqlx bind chains + turbofish string head); the long CAS `UPDATE`s stay SQL strings per plan Decision 4.
- [x] `check-toolkit.lisp`: 5/5 expansion assertions pass (`toolkit OK`).
- [x] Readability review: item demos through `write-source`+rustfmt, output human-read (see walkthrough §Phase 2).
- [ ] Deferred to Phase 3: provider-call wrapper + askama-vs-inline-HTML spike (needs the crate context); AppState emitted directly by `gen.lisp`.

## Phase 3 — MVP generation (split `gen00..gen06` → Rust crate) — DONE (2026-09-06, commits `78b8bcb`+`f717aa3`)

- [x] Monolith split per reviewer advice: `gen00_utils` (paths/builders/`Cargo.toml` text/migrations copy), `gen01_models`, `gen02_db`, `gen03_ai`, `gen04_routes`, `gen05_main`, `gen06_generate` entry. Stale `gen.lisp` superseded (kept in tree, not loaded).
- [x] `Cargo.toml` (reduced deps, **edition 2024** per user request), `src/main.rs`, `src/models.rs`, `src/db.rs`, `src/routes.rs`, `src/ai.rs` all emitted; SBCL `gen06_generate.lisp` load clean.
- [x] Endpoints: submit → queue → poll → read; embedding search (`cosine_similarity` port; unequal lengths handled by min-length, no explicit Matryoshka step — out of slice).
- [x] HTTP smoke test (no API key): `GET /` → 200, empty submit → guard message, real submit → `queued #1` → background task → `status: failed` with Gemini 403 recorded. `init_db` creates parent `data/` (fresh-start panic fixed).
- [x] Schema-compat probe (`/tmp/compat_probe.py`, `COMPAT_OK`): 9 migrations byte-identical to upstream; lifecycle replay on fresh db; 2000 real legacy rows (reference `summaries.db` applied only migration 001) upgraded through 002–009, readable via MVP `fetch_row`, lifecycle runs. Finding: 007 backfills `succeeded`/`queued`, 008 quarantines legacy queued — upstream behavior, preserved.
- [x] Cut list enforced: manual transcript paste only; no VTT parsing, no yt-dlp auto-download, no HN/viz/nn-mapper/export-db/ratings/browse UI. Runtime DBs covered by `data/*.db` gitignore.
- [x] `cargo build` 0 warnings, `cargo clippy` 0 warnings (`Ok(true)` fold, `drop()` over empty `if`s, intermediate `let`s vs `collapsible_if`, `gen`→`generation` for the 2024 keyword), `cargo fmt --check` clean.
- [ ] Scope cuts (upstream concepts with no MVP counterpart — no code exists, nothing to test): event-fold accumulator, epoch-guard, explicit Matryoshka truncation step.

## Phase 4 — Tests (rescoped to MVP slice 2026-09-06) — DONE (2026-09-06)

- [x] Rust unit tests **emitted by the generator** (`cfg(test)` + `space "mod tests"` escape, no transpiler change): 9/9 pass — cosine identical/orthogonal/zero/empty/unequal-length, bytes roundtrip; DB lifecycle on `:memory:` (insert→claim CAS incl. double-claim `false`→success/fetch, failed path, embedding roundtrip). `&vec![…]`→`&[…]` via `bracket`/`comma` for clippy `useless_vec`.
- [x] Lisp-side: `check-toolkit.lisp` 10/10 incl. golden assertions for `Ok(true)` fold, `drop()` discard, unnest-`let`, `cfg(test)` module, `bracket`/`comma`.
- [ ] Integration test with fake AI service — DEFERRED: Gemini URL is hardcoded in `ai.rs`; injecting a fake base requires plumbing it through `AppState` (generator + routes change). No epoch-guard exists in the slice, so that case is moot.
- [x] Live smoke tests (quota ≈ 2 generates + 3 embeds): keyless run → `failed` with 403 recorded/pollable ($0); keyed run → `succeeded` (50/102 tokens), 12288 B embedding stored, search `ferry harbor dawn` → `summary #1: 0.720`. Embed model fixed: `text-embedding-004` retired (404) → `gemini-embedding-001`.
- [x] `/root/.env` provides `GEMINI_API_KEY` (+`HETZNER_API_KEY`); names only, values never read.

## Phase 5 — Docs & hardening

- [x] `cargo build`, `cargo test` (9/9), `cargo clippy --all-targets`, `cargo fmt --check` clean (0 warnings).
- [x] Example README (`examples/22_summarizer/README.md`: how to run generation, how to extend via toolkit).
- [ ] `SUPPORTED_FORMS.md` final regeneration; README cross-links (with Phase 1 owner — earlier phases' tree changes, not this slice).
- [x] `plan/20260906_01_rs_summarizer/walkthrough.md` with results + unexpected findings.

## Phase 6 — Commit

- [x] Reviewable split, slice part: (2) toolkit + MVP → `78b8bcb` (+`f717aa3` scratch removal).
- [ ] (1) transpiler forms + remaining docs — with their owners; report hashes in walkthrough.
