# Operatorpräzedenz, CL-nahe Namen, Semikolons im Rust-Generator

Datum: 2026-09-06
Auftrag: `plan/20260906_02_precedence/prompt.txt`
Referenzen: `rs.lisp`, `transpiler-tests.lisp`, `operator-precedence.md`
(Tabelle 5-1 aus „Programming Rust"), `rust.md`,
`plan/20260905_01_forms/walkthrough.md`, `MIGRATION.md`

Die kleinteilige Arbeitsplanung steht in `tasks.md`.

## 1. Ausgangslage

Baseline vor den Änderungen: `./run-tests.sh` grün mit 156 Transpiler-Tests
und 2×23 Wertetests (voll geklammert + elidiert). Die Vorarbeit aus
`20260905_01_forms` (range-Familie, if-let/while-let, deftrait, stmt,
`*omit-redundant-parens*`) wurde nicht wiederholt, sondern darauf aufgebaut.

## 2. Code-Review: Tabelle 5-1 gegen den Generator

Jede Zeile aus `operator-precedence.md` wurde gegen die `case`-Tabelle in
`emit-rs` gehalten. Ergebnis: fast alles ist abgedeckt; die Lücken waren
klein und sind jetzt geschlossen:

| Tabellenzeile | Befund |
|---|---|
| Repeat array literal `[0; 50]` | fehlte → neu: `(array-repeat val n)` |
| Compound assignment | `<<= >>= &= \|=` fehlten (`/= *= ^= %=` gab es) → neu, im selben Stil |
| `let ... else` (in `rust.md` beschrieben) | fehlte → neu: `(let-else (pattern scrutinee) form*)` |
| Tuple field access `pair.0` | ging schon, war aber unbelegt → Test `(dot pair 0)` pinnt es |
| Closure, `?`, `as`, Ranges, Vergleiche, `&&`/`\|\|`, Shifts, Bitops | abgedeckt, Elision per Wertetest verifiziert |

Bewusst *nicht* gebaut: Labels für `loop`/`block`/`break` (größeres Design,
in `rust.md` nur als Idiom erwähnt) und Match-Guards (stehen weiter in der
README unter „Not supported").

## 3. Benennungen: fünf Aliase gelöscht

Der Prompt erlaubt (und verlangt) Inkompatibilität: ersetzte Forms werden
gelöscht, Migration in `MIGRATION.md`. Gelöscht — jede Verwendung scheitert
jetzt laut mit Nennung des Ersatzes:

- `(slice a b)` → `(range a b)` (Fehlbenennung: emittierte nie einen Slice)
- `(cast v t)` → `(coerce v t)` (`coerce` ist der CL-Name)
- `(string# s)` → `(string-r s)` (Bindestrich wie alle `string-*`-Forms)
- `(& a b)` → `(logand a b)`, `(^ a b)` → `(logxor a b)` (CL-Namen waren
  immer die primären Forms; die Einzeichen-Köpfe hatte kein Beispiel
  benutzt)

Dazu aus den Tabellen entfernt: `&`/`^`/`cast` aus `*rust-precedence*`,
`&`/`^` aus `*rust-associative-ops*`, `cast` aus `*rust-loose-heads*`.
`array-repeat` ist als `:primary` klassifiziert (selbst-begrenzt wie die
Ranges), `let-else`/`expr`/`<<=`/`>>=`/`&=`/`\|=` als `:loose`.

Behalten trotz Überlappung: `tuple`/`paren`/`values` und `bracket`/`list`.
Alle werden in den Beispielen gebraucht — `tuple`/`values` sogar als
Destrukturierungs-Pattern in `let`/`for`/`lambda` (`06_parallel_text`,
`21_mandelbrot`). Entfernen hätte Pattern-Syntax geändert ohne Nutzen.

In-tree migriert: `(slice`→`(range` in `13_vulkano`, `21_mandelbrot`;
`(string#`→`(string-r` in `03_glium`, `10_wasm_webgl`, `13_vulkano`.
Die Paare emittieren textidentisch; `for i in 0..limit` im Mandelbrot steht
im Diff unverändert — der In-situ-Beweis.

## 4. Semikolons: `expr` und selbst-terminierendes `let-else`

Regel (unverändert, jetzt auch für die neuen Köpfe): `do0` hängt `;` an,
außer die Emission endet schon auf `;`, die Form ist ein String, oder der
Kopf steht in `*keywords-without-semicolon*`. `progn` unterdrückt das `;`
der letzten Form (implizites Return), `block` behält es (evaluiert zu `()`).

Zwei Ergänzungen:

- **`(expr form)`**, das Gegenstück zu `(stmt form)`: erzwingt
  Expressions-Position (kein `;`). Für Escape-Hatch-Forms am `progn`-Ende,
  wo ein fehlendes Semikolon „returniere diesen Wert" bedeutet.
- **`(let-else ...)`** terminiert sich selbst (Rust verlangt dort immer
  `;`), Kopf in der Whitelist gegen Verdopplung. Damit ist die Form in
  jeder Statement-Position sicher — auch als letzte `progn`-Form.

Zwei neue Tests pinnen die Kernsemantik direkt: `(progn (= x 5))` →
`{ x=5 }` gegen `(block (= x 5))` → `{ x=5; }`.

## 5. Präzedenz-Tests: +9 String-, +8 Wertetests

String-Tests mit `:omit-parens t`: `logior`-Kette flach, `logxor` rechts
flach (ganzzahlig assoziativ), `&`-vor-`^` ohne Klammern, `<<`/`>>` links
flach, `||` rechts flach, `(a || b) && c`-Klammer, `(a << 1) + b`-Klammer,
`+` rechts flach (mit Float-Rundungsvorbehalt wie dokumentiert).
Wertetests: `shl/shr-left-nested`, `xor-chain`, `xor-right-nested`,
`bitor-basic`, `or-right-nested`, `add-right-nested`, `coerce-bitand` —
alle laufen doppelt (voll geklammert + elidiert) als differentielles Orakel.

## 6. Verifikation

```sh
./run-tests.sh
# Transpiler tests run: 173 / Assertions passed: 173 / failed: 0
# Running 31 value tests via rustc... PASS
# Running 31 value tests (omit-parens) via rustc... PASS
./generate-docs.sh  # SUPPORTED_FORMS.md neu (keine slice/cast/string#-Reste)
```

- Snippet mit allen neuen Forms (`<<= >>= &= \|= %=`, `array-repeat`,
  `let-else`, `range`, `coerce`, `dot`-Tuple-Feld) durch `rustfmt`
  (sauber), `rustc --crate-type lib` (0 Fehler) und `cargo clippy`
  (nur Snippet-Qualitäts-Lints wie dead_code/unused, keine Klammer- oder
  Präzedenz-Beschwerden).
- `examples/21_mandelbrot` neu generiert: `cargo check --offline` mit
  0 Fehlern. Der `main.rs`-Diff (14 Zeilen, reine Klammer-Entfernung durch
  das installierte rustfmt 1.9) entsteht mit dem HEAD-Generator identisch
  (per `git stash` gegengeprüft) — Drift des Toolchains, nicht dieser
  Sitzung; die migrierte `for i in 0..limit`-Zeile ist unberührt.
- `examples/01_gcd` neu generiert: Byte-identisch, `cargo test --offline`
  → `test_gcd ... ok`.
- Entfernte Forms scheitern laut, z.B.:
  `the form slice is not supported. Removed: use (range a b), ...`

## 7. Unerwartete Funde

- **`|` ist LISPs Multiple-Escape-Zeichen.** Ein nackter Kopf `(|= ...)`
  korrumpiert beim *Lesen* die ganze Datei — der Fehler („unmatched close
  parenthesis" in Zeile 1397) liegt weit weg von der Ursache. Der Kopf muss
  `\|=` geschrieben werden (nur Lisp-Schreibweise; das Rust bleibt
  `x|=(mask)`). In `MIGRATION.md` festgehalten. Derselbe Fallstrick gälte
  für jede künftige Form mit `|` im Namen.
- **Vorheriger Walkthrough zu optimistisch.** „Mandelbrot byte-identisch"
  galt damals; mit dem installierten rustfmt 1.9 räumt die Neugenerierung
  14 Klammerpaare weg — auch ohne Generatoränderung. Generierte `.rs`-Dateien
  driften mit der Toolchain; der Diff ist kosmetisch und kompiliert.
- **README war stale.** „Not supported" listete noch `async`/`await` und
  Enums — `defun-async`/`await` bzw. `defenum` existieren längst. Korrigiert;
  außerdem Formtabelle (`<<= >>= &= \|=`, `array-repeat`, `let-else`,
  `expr`) und ein Präzedenz-Absatz in `rust.md`.
- **Pre-commit-Hook** (`.git/hooks/pre-commit`, prüft `20_webprox_avif`)
  ist komplett auskommentiert — blockiert nichts.

## 8. Reproduktion

```sh
./run-tests.sh     # 173 Transpiler-Tests + 2x31 Wertetests via rustc
./generate-docs.sh # SUPPORTED_FORMS.md neu erzeugen
cd examples/21_mandelbrot/mandelbrot && cargo check --offline
cd ../../01_gcd/rs01_gcd && cargo test --offline
```

# Kosten

```
│  USAGE          12,122,100 tokens · 92 turns · 0 subagents │
│  CONTEXT        81% left · 191K used / 1008K · normal      │
│                                                            │
│  SESSION        01a07625-7dc6-7492-81d0-70410a257037       │
│  ACTIVITY       no tasks                                   │
│                 0 terminals · inbox clear                  │
│                                                            │
│  BILLING        Subscription · Muse Code Everyday Usage    │
└────────────────────────────────────────────────────────────┘

  Session usage

    Input      12,068,554
    Cached     11,872,939
    Output         53,546
    Total      12,122,100

    Turns               92
    Subagents         none

  Subscription · Muse Code Everyday Usage
    Current        85% used · Resets at 1:59 PM
    Weekly         78% used · Resets Sep 7 at 12:00 AM
    as of 10:11 AM
```
