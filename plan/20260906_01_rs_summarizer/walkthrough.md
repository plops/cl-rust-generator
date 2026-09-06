# Walkthrough: rs-summarizer MVP in cl-rust-generator (20260906_01)

Target `examples/22_summarizer/` (generator `gen00..gen06` + `toolkit.lisp`
→ `summarizer-mvp/` crate). Constraints kept: manual transcript paste only
(no VTT, no yt-dlp), numbered generator files, Rust 2024, `data/*.db` ignored.

## Phase 0 — Baseline

`rs-summarizer`: 7 routes (`lib.rs::build_router`), 8 services,
~19 `db.rs` fns, 7 askama templates, 6 error enums. Kept: tokio/axum/sqlx/
serde/chrono/tracing/reqwest; cut: burn/cubecl/fast-umap/vtt/pulldown-cmark/
fantoccini/proptest. `/root/.env` provides `GEMINI_API_KEY` (names only read).

## Phase 1 — Transpiler forms

Earlier phases' work (`defun-async`/`await`, `attr`, `?`, precedence):
`./run-tests.sh` 156/156 green. Their tree changes are not this slice's.

## Phase 2 — Toolkit

`routes`/`handler`/`model`/`db-exec`/`db-fetch-option` builders;
`check-toolkit.lisp` now 10/10 (`toolkit OK`), incl. golden assertions for
the Phase 3 patterns (`Ok(true)` fold, `drop()` discard, intermediate-`let`
unnesting, `cfg(test)` module escape, `bracket`/`comma` slices).

## Phase 3 — MVP generation

Split-monolith regeneration after a backquote regression. Findings:

- **`,(attr …)` calls a function; `(attr …)` is syntax.** Comma-prefix is
  only for real builders (`pub_`, `handler`, `fallible`, `routes`). The
  same class of bug: commas inside a `,@(list …)` operand don't belong to
  the backquote — `list` must close first ("Comma not inside a backquote").
- **Rust 2024:** `edition = "2024"` + rustfmt flag; `gen`→`generation`
  (new keyword); clippy-zero via `Ok(true)` fold, `drop()` over empty
  `if`s, intermediate `let`s.
- **Fresh-start panic:** sqlx won't create parent dirs → `init_db` now
  `create_dir_all`s the URL's parent (sqlite code 14 fixed).
- **Compat probe** (`COMPAT_OK`): 9 migrations byte-identical; lifecycle
  replay; 2000 legacy rows (reference DB applied only migration 001)
  upgraded 002–009 and readable. 007 backfills `succeeded`/`queued`,
  008 quarantines legacy queued — preserved upstream behavior.
- **Retired model:** `text-embedding-004` → 404; swapped to
  `gemini-embedding-001` (URL + `embedding_model` label).

## Phase 4 — Tests

- `cargo test`: 9/9 generated tests pass (6 cosine/bytes incl. zero/empty/
  unequal-length, 3 DB lifecycle incl. double-claim CAS `false`, failed
  path, embedding roundtrip on `:memory:`).
- Live smoke (quota ≈ 2 generates + 3 embeds): ferry transcript →
  `succeeded` (50/102 tokens), embedding 12288 B stored, search
  `ferry harbor dawn` → `summary #1: 0.720`. Earlier keyless run verified
  the `failed` path (403 recorded, pollable). Fake-AI integration
  DEFERRED (Gemini URL hardcoded; needs `AppState` plumbing).

## Phase 5 — Gates

`cargo build` 0 warnings, `cargo clippy --all-targets` 0 warnings,
`cargo fmt --check` clean, `cargo test` 9/9. Example README written.
`SUPPORTED_FORMS.md`/README cross-links left with Phase 1 owner.

## Phase 6 — Commits

- `78b8bcb` toolkit + MVP slice; `f717aa3` scratch removal.
- Transpiler/docs commits with their owners.
