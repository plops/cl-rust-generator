# Plan: redundante Block-Verschachtelung entfernen (20260919_01_blocks) — prompt.txt

Ziel: `emit-rs` soll fuer einen Zweig, der genau eine block-emittierende Form
(`progn`, `block`, `let`, `let*`) enthaelt, keine zweite Klammer-Ebene mehr
oeffnen. Statt `if c { { let x = …; … } }` soll `if c { let x = …; … }`
emittiert werden. Gleiches gilt fuer Funktionsruempfe mit einem einzigen
`let` (`fn main() { { … } }` → `fn main() { … }`).

## 0. Wurzelursache (gelesen, nicht geraten)

- `parse-let` wickelt Bindungen + Rumpf immer in ein `progn` (`{…}`).
- Jede Verzweigung (`if`, `when`, `unless`, `if-let`, `while-let`, `while`,
  `for`, `loop`, `dotimes`, `case`-Arme, `let-else`) wickelt ihren Zweig
  unbedingt in ein `progn`.
- Ein einzelnes `let` (oder `progn`/`block`) als Zweig erzeugt daher
  zwangsläufig `{{…}}`.

## 1. Design: ein Engpass statt Sonderfaelle je Form

- `*block-emitting-heads*` = `(progn block let let*)`: Köpfe, deren Emission
  bereits ein nackter `{…}`-Block ist.
- `block-form-p`: Prädikat darauf (nur Listen, `:test #'eq`).
- `emit-branch-block` (einzelne Zweig-Form: `if`-then/else,
  `if-let`-then/else): Block-Form direkt emittieren, alles andere in
  `progn` wickeln.
- `emit-body-block` (Form-Listen: alle uebrigen Verzweigungen,
  `case`-Arme, `defun`-/`lambda`-Ruempfe, `unsafe`/`extern`): genau eine
  Form und Block-Form → direkt emittieren (spleissen), sonst
  `(progn ,@forms)`. Leere Rümpfe behalten das alte `progn`-Verhalten.
- Bewusst *kein* „intelligentes `progn`": `progn` muss dumm bleiben, weil
  `defun`-Ruempfe ihre Klammern von ihm beziehen (`fn f() {…}` wuerde bei
  Kollabierung zu `fn f() if …` ohne Klammern).

## 2. Nicht-Ziele (bleibt wie es ist)

- `parse-let`-Innenleben (Strings + Rumpf mischen) wird nicht angefasst.
- `let` neben Geschwistern (z. B. zweites Statement im `loop`-Rumpf)
  behaelt seinen Block — das ist echter Scope, keine Redundanz.
- `defstruct0`/`defenum`/`deftrait`/`impl`-Ruempfe: bauen Item-Koerper aus
  Strings, dort gibt es keine Doppelbloecke.

## 3. Umsetzung

- [x] Helfer vor `parse-let` in `rs.lisp` definieren
- [x] `parse-defun`, `parse-lambda` auf `emit-body-block` umstellen
- [x] alle Verzweigungsstellen in `emit-rs` umstellen
      (`if`, `when`, `unless`, `if-let`, `while-let`, `let-else`,
      `unsafe`, `extern`, `case`-Arme, `dotimes`, `loop`, `for`, `while`)
- [x] Regressionstests in `transpiler-tests.lisp` (Singleton faellt weg,
      Mehrfach-Rumpf behaelt Scope)
- [x] `./run-tests.sh` gruen, `SUPPORTED_FORMS.md` neu generiert
- [x] Plan-Eintrag (`prompt.txt`, `plan.md`, `walkthrough.md`) + Commit
