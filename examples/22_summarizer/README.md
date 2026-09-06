# summarizer-mvp — generated YouTube-summary slice

Minimal submit → queue → poll → read service with embedding search,
generated from Lisp (`gen00..gen06` + `toolkit.lisp`) into compilable,
clippy-clean Rust (edition 2024). Schema = the verbatim upstream
`rs-summarizer` migration set, so rows stay compatible with the real DB.

## Run

```sh
cd /workspace/src/cl-rust-generator
export GEMINI_API_KEY=...            # empty also works; generations then fail visibly
export GEMINI_BASE_URL=...           # optional; defaults to the Google endpoint (tests point it at a local mock)
cargo run --offline --manifest-path examples/22_summarizer/summarizer-mvp/Cargo.toml
# open http://127.0.0.1:5001/ — paste a transcript, submit, poll /status/<id>
```

The server listens on `127.0.0.1:5001` and stores
`data/summarizer-mvp.db` relative to its working directory
(`data/*.db` is git-ignored). First start applies `migrations/`.

## Regenerate

```sh
sbcl --disable-debugger --non-interactive \
  --eval '(ql:register-local-projects)' \
  --load examples/22_summarizer/gen06_generate.lisp --quit
cargo test --offline --manifest-path examples/22_summarizer/summarizer-mvp/Cargo.toml
```

`gen06` rewrites `Cargo.toml`, `migrations/` (verbatim upstream copy) and
`src/*.rs` (rustfmt applied). Never hand-edit `src/` — change the `gen*.lisp`
source of truth. `gen.lisp` is the superseded monolith (kept, not loaded).

## Extend via the toolkit

`toolkit.lisp` provides `routes`, `handler`, `model`, `db-exec`,
`db-fetch-option`; `check-toolkit.lisp` asserts their expansions
(`toolkit OK`, 10 checks incl. the Phase 3 golden patterns).
Handler bodies merge sequential bindings into one `let`; `,(fn …)`
calls a real builder function, while bare `(attr …)`/`(space …)` is
emitter syntax — never comma-prefix syntax. `,@(list …)` operands must
close before any later comma (SBCL backquote rule).

## Notes

- Manual transcript paste only: no VTT parsing, no yt-dlp auto-download.
- Embedding model: `gemini-embedding-001` (`text-embedding-004` retired → 404).
- Without `GEMINI_API_KEY` the queue still works; generations record the
  HTTP error and stay pollable (`failed` status).
