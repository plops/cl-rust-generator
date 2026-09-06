# Migration: removed forms (2026-09, plan/20260906_02_precedence)

There is no backward compatibility: when a form is replaced by a better
one, the old one is deleted and using it signals a loud error that names
the replacement. This file lists every removal so out-of-tree generators
and examples can be adapted.

In-tree examples were already migrated (`03_glium`, `10_wasm_webgl`,
`13_vulkano`, `21_mandelbrot`); their generated `.rs` output is
byte-identical before and after, because each pair emits the same text.

## Removals

| Old | Replacement | Notes |
|-----|-------------|-------|
| `(slice a b)` | `(range a b)` | `slice` was a misnomer: it emitted a range `a..b`, never a slice. The full family is `range`, `range-inclusive`, `range-from`, `range-to`, `range-to-inclusive`, `range-full`. |
| `(cast v t)` | `(coerce v t)` | `coerce` is the Common Lisp name and was always the primary form; `cast` only duplicated it. Argument order stays value-first. |
| `(string# s)` | `(string-r s)` | Same raw-string emission; the hyphenated name fits the other `string-*` forms. |
| `(& a b)` | `(logand a b)` | `logand`/`logxor` are the Common Lisp names for bitwise `&`/`^` and were always the primary forms. |
| `(^ a b)` | `(logxor a b)` | See above. Note: write the replacement head as `logxor`, never bare `^`. |

Using any removed head now fails at generation time, e.g.:

```
cl-rust-generator: the form SLICE is not supported.
Removed: use (range a b), (range-inclusive a b), ...
```

## sed recipes

Run from the directory holding your `gen*.lisp` files. Check the diff
afterwards; heads like `castRay` (a function *name* starting with
"cast") must not be touched, which is why the patterns below require a
space or an opening paren position after the head:

```sh
# (slice 0 n) -> (range 0 n)
sed -i 's/(slice /(range /g' gen00.lisp
# (cast v t) -> (coerce v t)
sed -i 's/(cast /(coerce /g' gen00.lisp
# (string# s) -> (string-r s)
sed -i 's/(string#/(string-r/g' gen00.lisp
# (& a b) -> (logand a b); (^ a b) -> (logxor a b)
sed -i 's/(& /(logand /g; s/(\^ /(logxor /g' gen00.lisp
```

## Kept aliases (deliberately not removed)

`(tuple ...)` / `(paren ...)` / `(values ...)` and `(bracket ...)` /
`(list ...)` overlap, but all of them are load-bearing in the examples:
`tuple`/`values` are used as destructuring patterns in `let`, `for` and
`lambda` (`06_parallel_text`, `21_mandelbrot`), and `bracket` is used in
`22_summarizer`. Removing them would force pattern syntax to change for
no benefit, so they stay.

## A Lisp reader caveat (why `|=` is written `\|=`)

`|` is the Common Lisp multiple-escape character, so a form head such
as `|=` must be written with a single-escape (`\|=`) in Lisp source;
a bare `(|= ...)` silently corrupts reading of the whole file that
contains it (diagnosed in the 2026-09 walkthrough: an "unmatched close
parenthesis" far away from the actual cause). This affects only the
*spelling in Lisp files*, not the emitted Rust (`x|=(mask)`).
