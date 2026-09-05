# Tasks: forms review, semicolons, omit-paren (20260905_01_forms) — DONE, see walkthrough.md

Goal: prompt.txt — code review of the Rust generator, CL-leaning forms
(esp. ranges), robust semicolon handling + proposal, omit-paren like
cl-cpp-generator2 / cl-py-generator with precedence table + unit tests,
docs/code updates, walkthrough.md, commit.

## 0. Baseline (broken at HEAD)
- [x] `./run-tests.sh` fails: `SYSTEM-NOT-FOUND cl-rust-generator`
      (missing `ql:register-local-projects`). Workaround known, scripts to fix.
- [x] 6 `let-*` tests fail: `f9c2739` changed `parse-let` from `do0` to
      `progn` (block with implicit-return value semantics) without updating
      expectations. Decision: keep `progn` (CL-faithful scope + value),
      update test expectations + docs.
- [ ] Fix `run-tests.sh` / `generate-docs.sh` (register local projects).

## 1. Forms: missing, misnamed, untested
- [ ] Ranges: `slice` is a misnomer (emits `a..b`, used for ranges in
      vulkano/mandelbrot). Add `range` family, keep `slice` as alias:
      `range` (a..b), `range-inclusive` (a..=b), `range-from` (a..),
      `range-to` (..b), `range-to-inclusive` (..=b), `range-full` (..).
- [ ] `if-let` / `while-let` (README lists as "not supported", no form).
- [ ] `deftrait`: dead entry in `*keywords-without-semicolon*`, no `case`
      clause (silently falls into function-call branch). Implement minimal
      trait with header-only `defun` signatures via `parse-defun :header-only`.
- [ ] `stmt`: explicit "this is a statement, terminate with `;`" wrapper for
      escape-hatch forms like `(space ...)` in statement position.
- [ ] Add tests for existing-but-untested forms: `angle`, `scope`, `cast`
      alias, `%=`, `do`, `string-r` alias.
- [ ] Remove/justify dead whitelist entries; add missing `do`, `block`?
      (verify whether the string-suffix check already covers them).

## 2. Semicolons
- [ ] Harden `do0` against empty emission (aref crash on `""`).
- [ ] Document the current rule (suffix check + head whitelist) in walkthrough;
      proposal: explicit `stmt` override (implemented) + future tracking of
      statement-vs-expression instead of string sniffing.

## 3. Omit-paren (precedence-based elision)
- [ ] `*rust-precedence*` table (from rust.md / operator-precedence.md:
      unary > `as` > `* / %` > `+ -` > `<< >>` > `&` > `^` > `|` >
      comparison > `&&` > `||` > `.. ..=`), with associativity.
      Non-chainable: comparison, ranges (Rust rejects `a==b==c` at parse).
- [ ] `*omit-redundant-parens*` flag (default NIL = current output,
      byte-identical). Helpers `emit-binary-op` / operand paren decision
      with left/right position.
- [ ] Wire flag through binary/unary operators; keep `strip-outer-parens`
      for conditions/return/assign.
- [ ] Unit tests: `:omit-parens t` string tests + run `*value-tests*`
      a second time with flag bound (differential oracle: a wrong elision
      changes the computed value).

## 4. Docs, verification, commit
- [ ] Regenerate `SUPPORTED_FORMS.md`; update README (ranges, if-let,
      trait, omit-paren, semicolon rule).
- [ ] `./run-tests.sh` green; `rustfmt` syntax check; regenerate + build
      `01_gcd` (`cargo test --offline`); `cargo clippy` on generated code
      where offline possible.
- [ ] `plan/20260905_01_forms/walkthrough.md`; commit.
