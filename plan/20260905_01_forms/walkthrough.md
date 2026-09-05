# Forms, Semikolons, Klammer-Elision im Rust-Generator

Datum: 2026-09-05
Auftrag: `plan/20260905_01_forms/prompt.txt`
Referenzen: `rs.lisp`, `rust.md`, `examples/21_mandelbrot/gen00.lisp`,
`/workspace/src/cl-cpp-generator2/plan/20260830_01_omit_paren_bug/walkthrough.md`,
`/workspace/src/cl-py-generator/plan/20260831_02_omit_paren/walkthrough.md`

Die kleinteilige Arbeitsplanung steht in `tasks.md`.

## 1. Ausgangslage: die Suite war rot

Zwei Befunde gleich zu Beginn, beide auf HEAD reproduzierbar:

1. **`./run-tests.sh` lief gar nicht.** `transpiler-tests.lisp` macht
   `(ql:quickload :cl-rust-generator)`, aber ohne vorheriges
   `(ql:register-local-projects)` findet Quicklisp das lokale System nicht
   (`SYSTEM-NOT-FOUND`). Auf der Entwicklungsmaschine hatte der Quicklisp-Daemon
   die `local-projects` offenbar schon gescannt. Fix: beide Skripte
   (`run-tests.sh`, `generate-docs.sh`) rufen jetzt zuerst
   `(ql:register-local-projects)` auf.
2. **6 `let-*`-Tests schlugen fehl.** Commit `f9c2739` hatte `parse-let` von
   `do0` auf `progn` umgestellt (damit ein `let` als Block seinen letzten Wert
   zurückgibt — implizites Return, z.B. `pixel_to_point` im Mandelbrot), aber
   die Testerwartungen nie angepasst. Entscheidung: `progn` bleibt (CL-nahe
   Scope- und Wert-Semantik: Bindungen entkommen dem Block nicht, der Block
   evaluiert zur letzten Form), die 6 Erwartungen wurden auf die Klammerform
   gebracht (`{ let x = 5; f(x) }`). Neu dazu: `let-statement-position`
   pinnt das Verhalten in Statement-Position.

## 2. Forms: was fehlte, was falsch hieß, was ungetestet war

Eine Kopfzähler-Auswertung aller `examples/*/gen*.lisp` (~621 Köpfe) gegen die
`case`-Tabelle in `emit-rs` ergab:

### 2.1 Ranges hießen `slice`

`(slice 0 limit)` emittiert `(0..limit)` — also eine Range, keinen Slice.
Slices als Typ (`&[T]`) werden ohnehin als Strings geschrieben. Neu:

| Form | Rust |
| --- | --- |
| `(range a b)` | `(a..b)` |
| `(range-inclusive a b)` | `(a..=b)` |
| `(range-from a)` | `(a..)` |
| `(range-to b)` | `(..b)` |
| `(range-to-inclusive b)` | `(..=b)` |
| `(range-full)` | `(..)` |

`slice` bleibt als (dokumentiert veralteter) Alias, weil `13_vulkano`
(`depth_range`) und `21_mandelbrot` (`for (i (slice 0 limit))`) es benutzen.
Alle sieben Formen behalten ihre äußeren Klammern als Operand (sicher als
`dot`-Empfänger etc.); `for`/`if`/`return`/Zuweisung streifen sie wie bisher
per `strip-outer-parens` ab (`for x in 0..n`).

### 2.2 `if-let` / `while-let` (README: „not supported", jetzt implementiert)

```lisp
(if-let ((Some x) y) (return x) (return 0))
;; if let Some(x) = y { return x } else { return 0 }
(while-let ((Some x) (dot it (next))) (f x))
;; while let Some(x) = it.next() { f(x) }
```

Pattern sind gewöhnliche Forms: `(Some x)` → `Some(x)`, `None`/`_`/`true` gehen
wörtlich. Vorsicht, keine Generator-Lücke sondern Rust-Semantik: ein Call als
Pattern ist ungültig — `(if-let ((true) x) ...)` emittiert `if let true() = x`
(stirbt in `rustc`, zu Recht). Es muss `(if-let (true x) ...)` heißen.

### 2.3 `deftrait`: toter Whitelist-Eintrag, jetzt implementiert

`deftrait` stand in `*keywords-without-semicolon*`, hatte aber keine
`case`-Klausel und fiel still in den Function-Call-Zweig (`deftrait(...)` ohne
Semikolon). Jetzt minimal, aber echt — Members sind `defun`s, nur die
Signatur wird emittiert (wiederverwendet `parse-defun :header-only`):

```lisp
(deftrait Shape (defun area (&self) (declare (values f64))))
;; trait Shape { fn area(&self) -> f64; }
```

Bounds als String-Name (`"Shape: Debug"`). Nicht-Member brechen mit `assert`
und lesbarer Meldung ab statt stillem Müll.

### 2.4 `stmt`: die explizite Antwort auf die Semikolon-Frage

Siehe Abschnitt 3.

### 2.5 Nachgetestet (existierte, war aber unbelegt)

`angle`, `scope` (zusammen: Turbofish `parse_pair::<i32>`), `cast`-Alias,
`%=`, `string-r`-Alias, `do`. Jede dieser Forms hat jetzt einen Testfall und
damit einen Doku-Eintrag in `SUPPORTED_FORMS.md`.

## 3. Semikolons: Regel, Härtung, Vorschlag

Die aktuelle Regel (jetzt auch in der README dokumentiert):

1. Emission endet bereits auf `;` → nichts anhängen.
2. Form ist ein String (Escape-Luke, verbatim) → nichts anhängen.
3. Kopf steht in `*keywords-without-semicolon*` (Block-/Item-Forms: `defun`,
   `if`, `when`, `case`, `for`, `while`, `loop`, `let`, `progn`, neu auch
   `do`, `if-let`, `while-let`, `stmt`, `deftrait`) → nichts anhängen.
4. Sonst `;` anhängen. `progn` unterdrückt es zusätzlich für die *letzte* Form
   (implizites Return), `block` behält es (evaluiert zu `()`).

Zwei Änderungen dazu:

* **Härtung.** `do0`/`do0-no-final-semicolon` machten
  `(aref b (1- (length b)))` auf der Emission — bei leerer Emission (z.B.
  `(emit-rs :code nil)` → `""`) ein Crash. Neu: `emission-ends-with-semicolon-p`
  mit Längenwache, an beiden Stellen verwendet.
* **Der Vorschlag aus dem Prompt, umgesetzt als `(stmt form)`.** Die Heuristik
  kann prinzipiell nicht wissen, ob eine `(space ...)`-Expansion Statement
  oder Expression ist. Statt die Heuristik zu verkomplizieren, gibt es jetzt
  den expliziten Override: `(stmt form)` hängt genau ein `;` an (kein Doppel
  bei bereits terminierter Emission). Beispiel:
  `(do0 (stmt (space foo bar)) (g))` → `foo bar; g();`.
  Ein Tracking von Statement-vs-Expression als dynamische Variable wäre der
  nächste Schritt, wurde aber bewusst *nicht* gebaut: es würde jede Klausel
  anfassen und alle Beispiele umwerfen, für einen Gewinn, den `stmt` an den
  drei echten Vorkommen bereits abdeckt.

Whitelist-Lücken, geprüft und begründet *nicht* geändert: `block` fehlt
absichtlich (sein `;` nach `}` ist die dokumentierte `()`-Semantik, harmlos),
`=`/`setf`/`incf` brauchen keinen Eintrag (Suffix-Check greift).

## 4. Klammer-Elision: `*omit-redundant-parens*`

Anders als der C++-Walkthrough vermutete, braucht Rust sehr wohl einen
Elisionsmodus — der Prompt fordert ihn ausdrücklich, und `cargo fix`/`clippy
--fix` räumen nur auf, was der Generator überflüssig einklammert. Design wie
bei `cl-py-generator`: Default (voll geklammert) bleibt Byte-identisch und ist
das Orakel; `*omit-redundant-parens*` (default NIL) schaltet die Elision ein.

### 4.1 Tabelle

`*rust-precedence*` folgt Tabelle 5-1 aus „Programming Rust"
(`operator-precedence.md`), hoch bindet stärker: unär (15) > `as` (14) >
`* / %` (13) > `+ -` (12) > `<< >>` (11) > `&` (10) > `^` (9) > `|` (8) >
Vergleiche (7, **non**) > `&&` (6) > `||` (5). Rust-Spezifika gegenüber C:
bitweise Operatoren binden *stärker* als Vergleiche
(`x & mask == 0` braucht keine Klammern), Vergleiche und Ranges sind
*nicht verkettbar* (`a==b==c` ist ein Parse-Fehler, kein Code) — daher
Assoziativität `non` mit Klammerzwang auf beiden Seiten.

Dazu `rust-expression-operator` (Form → `:primary`/`:loose`/Tabellenkopf,
analog zu `effective-operator` im C++/Python-Bericht; einstellige `+` sind
transparent, einstellige `-` werden `-unary`, einstellige `/` bleiben `/`),
Positionsargument (`:left`/`:right`) und drei Emit-Helfer für binäre, unäre
und „tighte" Operanden (`dot`-Empfänger, `aref`-Objekt, `?`-Operand — dort
gilt: alles außer `:primary` wird geklammert, damit `(dot (% a b) c)` auch
elidiert `((a)%(b)).c` bleibt und nie `a%b.c`).

Flach bleiben darf nur der gleiche linksassoziative Operator auf seiner
bevorzugten Seite; rechts zusätzlich nur Mitglieder von
`*rust-associative-ops*` (`+ * & | ^ && ||`, mit dokumentiertem
Float-Rundungsvorbehalt wie in den Geschwister-Transpilern). Konservativ
geklammert wird bei verschiedenem Operator gleicher Stufe
(`a * (b / c)` — Integer-Division!) und bei `--x` (ungültiges Rust).

### 4.2 Tests

* 19 String-Tests mit `:omit-parens t` (neue Harness-Option, bindet das Flag
  pro Testfall; `generate-documentation` beachtet sie ebenfalls).
* **Differentielles Orakel:** `run-value-tests` läuft jetzt zweimal — voll
  geklammert und elidiert — gegen dieselben Erwartungen. Eine verlorene
  Klammer ändert den Wert, nicht den Text. Dazu 7 neue Wertfälle als
  gezielte Differenzierer (`100/(10/2)=20` vs. flat `5`,
  `64>>(8>>1)=4` vs. flat `0`, `6&3==2`, `-(-5)=5` u.a.).
* Gegenprobe per Hand: der einzige Fehlschlag während der Entwicklung war
  meine eigene Erwartung (`(1+1)<<3` braucht keine Klammern, `+` bindet
  stärker) — die Implementierung hatte recht, der Wertetest bestätigte es
  unabhängig.

## 5. Verifikation

```sh
./run-tests.sh
# Transpiler tests run: 147 / Assertions passed: 147 / failed: 0
# Running 23 value tests via rustc... PASS
# Running 23 value tests (omit-parens) via rustc... PASS
```

* `examples/01_gcd` neu generiert: `cargo test --offline` → `test_gcd ... ok`,
  `cargo clippy --offline` sauber, `rustfmt --check` sauber. (Die Datei war
  seit `eb8cbc7` stale — der Diff gegen den eingecheckten Stand stammt aus
  `f9c2739` (`let`→`progn`) und älteren Generatorständen, nicht aus dieser
  Sitzung; per Konstruktion — alle Default-Zweige sind unverändert — ist der
  Default-Output dieser Sitzung Byte-identisch.)
* `examples/21_mandelbrot` neu generiert: **Byte-identisch** zum eingecheckten
  Stand, `rustfmt --check` sauber, `cargo check --offline` kompiliert mit
  0 Fehlern und nur 11 `unused_parens`-Stilwarnungen (der bekannte konservative
  Default-Stil, genau das, was der Elisionsmodus optional entfernt).
* Neue Forms Ende-zu-Ende: generiertes Snippet mit `deftrait`, `if-let`,
  `while-let`, `range(-inclusive)`, `stmt`, `deref` läuft durch `rustfmt`
  und `rustc --crate-type lib` fehlerfrei (nur die bekannte
  `unused_parens`-Warnung an einer Call-Stelle).
* Beispiele 02–13/20 wurden bewusst *nicht* neu generiert (brauchen Crates von
  crates.io bzw. waren schon vorher stale); kein Beispiel benutzt die neuen
  Köpfe, also ändert sich für sie nichts.

## 6. Unerwartete Funde am Rand

* **`angle` ohne Separator.** `(angle Vec T)` → `<VecT>`. Der einzige echte
  Gebrauch ist einstellige (`(angle i32)` im Turbofish); mehrstellig ist
  nutzlos, aber harmlos und jetzt per Test dokumentiert.
* **`let`-Scope überrascht beim Puzzlen.** `(let ((i 0)) (declare ...))` mit
  leerem Body emittiert `{ let mut i: usize = 0; }` — `i` entkommt nicht.
  Korrekt nach CL-Semantik, aber beim Schreiben leicht zu vergessen (einmal
  selbst reingelaufen, siehe Abschnitt 5-Snippet). Erwägenswert: Warnung bei
  leerem `let`-Body.
* **`if let true = ...` geht, `(true)` als Pattern nicht.** Listen-Pattern
  emittieren Calls — für `Some(x)`/`Ok(v)` genau richtig, für Literale muss
  das Pattern ein Atom sein. Dokumentiert, kein Codeproblem.
* **Pre-commit-Hook.** `.git/hooks/pre-commit` prüft `examples/20_webprox_avif`
  (`cargo fmt --check`, `clippy -D warnings`) und blockiert damit Commits aus
  unverwandter Arbeit — bekannt aus dem Review-Walkthrough 20260831, hier nur
  notiert: dieser Commit berührt `20_webprox_avif` nicht, der Hook läuft
  trotzdem bei jedem Commit mit.
* **Fasl-Falle (aus den Geschwister-Berichten bestätigt).** Wer per
  `git stash` gegenprüft, muss die `~/.cache/common-lisp`-Fasls löschen oder
  auf neue mtimes achten — sonst testet SBCL gegen den alten Generator.

## 7. Reproduktion

```sh
./run-tests.sh     # 147 Transpiler-Tests + 2x23 Wertetests via rustc
./generate-docs.sh # SUPPORTED_FORMS.md neu erzeugen
cd examples/01_gcd/rs01_gcd && cargo test --offline && cargo clippy --offline
cd ../21_mandelbrot/mandelbrot && cargo check --offline  # nur unused_parens-Warnungen
```


# Kosten

│  MUSE CODE 1.0.3 / white-protostar                COMPLETED │
│                                                             │
│  MODEL          muse-spark-1.3-contributor · high           │
│                 meta · native-basic                         │
│                                                             │
│  WORKSPACE      /workspace/src/cl-rust-generator/plan       │
│                 trusted · not found                         │
│  ACCESS         Unrestricted                                │
│                 Meta account                                │
│                                                             │
│  USAGE          18,529,547 tokens · 123 turns · 0 subagents │
│  CONTEXT        78% left · 221K used / 1008K · normal       │
│                                                             │
│  SESSION        01a07335-0140-7bd1-9258-474d897da86b        │
│  ACTIVITY       no tasks                                    │
│                 0 terminals · inbox clear                   │
│                                                             │
│  BILLING        Subscription · Muse Code Everyday Usage     │

  Session usage

    Input      18,457,259
    Cached     17,843,463
    Output         72,288
    Total      18,529,547

    Turns              123
    Subagents         none

  Subscription · Muse Code Everyday Usage
    Current        26% used · Resets at 1:13 AM
    Weekly         11% used · Resets Sep 7 at 12:00 AM
