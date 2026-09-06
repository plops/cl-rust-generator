# Tasks: operator precedence, CL naming, semicolons (20260906_02_precedence) — prompt.txt

Goal: code review of the Rust generator against `operator-precedence.md`
(Table 5-1, "Programming Rust") and `rust.md`; CL-leaning forms; robust +
easy semicolon handling; precedence unit tests; no backward compat
(delete old forms, record migration); docs updates; walkthrough.md; commit.

Prior work (do not redo): `plan/20260905_01_forms/` added range family,
if-let/while-let, deftrait, stmt, `*omit-redundant-parens*` + 19 string
tests + 23 value tests run in both modes.

## 0. Baseline
- [x] Inventory form heads used in `examples/*/gen*.lisp` vs `case` table
- [x] Confirm no test/example `:lisp` uses raw `(&` / `(^` heads
- [x] `./run-tests.sh` green before changes (record counts)

## 1. Deletions (no backward compat; migration in MIGRATION.md)
- [x] `(slice a b)` -> `(range a b)`; migrate `21_mandelbrot`, `13_vulkano`
      gen files; prove output byte-identical
- [x] `(cast v t)` -> `(coerce v t)`; no example usage
- [x] `(string# s)` -> `(string-r s)`; migrate `03_glium` (2 uses)
- [x] `(& a b)` -> `(logand a b)`; `(^ a b)` -> `(logxor a b)`; no usage
      anywhere; remove from `*rust-precedence*`, `*rust-associative-ops*`,
      `*rust-loose-heads*` (`cast` too)
- [x] Keep `tuple`/`paren`/`values` and `bracket`/`list`: all load-bearing
      in examples (incl. destructuring patterns in 06/21)

## 2. Additions (Table 5-1 gaps)
- [x] Compound assigns `<<= >>= &= |=` (same style as existing `/= *= ^= %=`)
- [x] `(array-repeat val n)` -> `[val; n]` (repeat array literal row)
- [x] `(let-else (pattern scrutinee) form*)` (rust.md documents let-else)
- [x] `(expr form)` — semicolon counterpart of `stmt`: forces NO `;`
- [x] Pin `(dot pair 0)` tuple-field access (Table row) with a test

## 3. Precedence unit tests (rust.md already describes, tests missing)
- [x] Omit-mode string tests: logior/logxor chains, `>>` left-flat,
      `or` right-nested flat, `(and (or a b) c)` parens, `<<` left-flat,
      `(+ (<< a 1) b)` parens, `+` right-assoc flat
- [x] Value tests: shl-left-nested, xor-chain, bitor, or-right-nested,
      add-right-nested, coerce-bitand

## 4. Semicolon tests (prompt: missing `;` = return value)
- [x] `(progn (= x 5))` -> `{ x=5 }` vs `(block (= x 5))` -> `{ x=5; }`
      (pins implicit-return vs `()` semantics for assignments)
- [x] `(do0 (expr (space foo bar)) (g))` pins the `expr` override

## 5. Docs, verification, commit
- [x] `MIGRATION.md` (removed forms + sed recipes for out-of-tree examples)
- [x] README: fix stale "Not supported" (async/await, enums exist now),
      update form table rows
- [x] Short precedence chain paragraph in `rust.md`
- [x] Regen `SUPPORTED_FORMS.md`; `./run-tests.sh` green (both modes);
      `rustfmt`/`rustc`/`clippy` on a generated snippet incl. all new forms;
      regen `21_mandelbrot`, prove byte-identical
- [x] `plan/20260906_02_precedence/walkthrough.md`; commit (prompt asks)
