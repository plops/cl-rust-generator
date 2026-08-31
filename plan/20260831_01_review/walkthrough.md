# Code-Review `cl-rust-generator` — Befunde und Korrekturen

Datum: 2026-08-31
Auftrag: `plan/20260831_01_review/prompt.txt`
Referenz für den Teststil: `/workspace/src/cl-py-generator/transpiler-tests.lisp`
Referenz für die Klammer-Thematik: `/workspace/src/cl-cpp-generator2/plan/20260830_01_omit_paren_bug/walkthrough.md`

## 0. Kurzfassung

Der Generator war **in seinem Kernpfad kaputt**. `(defun f (x) (declare (type i32 x)) ...)`
brach mit *undefined function* ab, und jeder Block (`progn`) verlor sämtliche
Semikolons. Beides sind Regressionen aus dem Umbau „immutable → mutable“
(Commits `a74745e`, `ab2e5a3`); die eingecheckten `main.rs`-Dateien stammen aus
der Zeit davor und lassen sich zum Teil gar nicht kompilieren.

Ergebnis dieser Sitzung:

* 15 Bugs in `rs.lisp` behoben, davon 5 „erzeugt garantiert nicht-kompilierbares Rust“.
* Neue Testsuite: `transpiler-tests.lisp` + `run-tests.sh` — 109 Checks in drei
  Schichten, inklusive eines von `rustc` kompilierten und *ausgeführten*
  Wertetests.
* `SUPPORTED_FORMS.md` (1551 Zeilen) wird aus der Testsuite generiert
  (`generate-docs.sh`) — jedes Beispiel darin ist verifiziert.
* `README.org` neu geschrieben; die alte Fassung war in mehreren Punkten falsch.
* Alle 13 generierten Beispiele (`01`–`13`) neu erzeugt; sie werden jetzt
  ausnahmslos von `rustfmt` akzeptiert. `01_gcd` kompiliert, sein `#[test]`
  läuft durch, und `cargo run -- 12 18 30` liefert `6`.

## 1. Vorgehen

1. `rs.lisp` gelesen, dann die `case`-Tabelle in `emit-rs` gegen die tatsächlich
   in den Beispielen benutzten Forms gehalten. Dazu habe ich alle
   `examples/**/*.lisp` mit dem Lisp-Reader eingelesen und die Häufigkeit jedes
   Kopf-Symbols gezählt (621 verschiedene Köpfe, davon ~60 echte Generator-Forms,
   der Rest Rust-Funktionsnamen).
2. Jede Form einzeln durch `emit-rs` geschickt und die Ausgabe gegen die
   Rust-Grammatik geprüft.
3. Alle Beispiele neu generiert und die Diffs gegen den eingecheckten Stand
   gelesen. Das ist das eigentliche Orakel: wo der neue Stand vom alten abweicht,
   muss man erklären können, welche der beiden Fassungen richtig ist.
4. `rustfmt` als Syntaxprüfer benutzt (Exit-Code ≠ 0 = kein gültiges Rust).

## 2. Kritische Bugs

### 2.1 `type-definition-immutable` existiert nicht

```lisp
(m (when decl-m (type-definition-immutable decl-m)))   ; parse-defun
```

Die Struktur heißt seit dem Umbau `mutable`, nicht `immutable`. Jede `defun`
mit *typisierten* Parametern lief damit in
`The function type-definition-immutable is undefined`. Untypisierte `defun`s
(alle Beispiele, die Parameter als Strings schreiben) waren nicht betroffen —
deshalb ist es so lange nicht aufgefallen.

### 2.2 `progn` verlor alle Semikolons

```lisp
(format s "{~{~&~a~}~&}"
        (mapcar #'(lambda (x) (emit `(indent (do0-no-final-semicolon ,x)) 1))
                (cdr code)))
```

Jede Form wurde **einzeln** in ein `do0-no-final-semicolon` gewickelt. Damit war
jede Form die letzte und bekam kein Semikolon:

```rust
fn f() { a() b() return 1 }   // vorher
fn f() { a(); b(); return 1 } // nachher
```

Betroffen war jeder Funktionskörper, jeder `if`-Zweig, jeder Schleifenkörper und
jeder `match`-Arm — also praktisch alles. Korrektur: **ein** gemeinsames
`do0-no-final-semicolon` über alle Forms.

Nebenbefund: dieser Bug hat einen zweiten maskiert. `do0-no-final-semicolon`
schreibt `"; // <N>"` statt `";"` — ein vergessener Debug-Marker. Weil `progn`
nie mehr als eine Form durchließ, wurde er nie sichtbar. Nach der Korrektur
stand plötzlich in jedem Block `a(); // 0`. Jetzt `";"`.

### 2.3 `remove-ampersand` war entkernt

```lisp
(defun remove-ampersand (rname)
  (let* ((sname (if (listp rname) rname (format nil "~a" rname)))
         (name sname))            ; (remove #\& sname) auskommentiert
    (values name nil)))           ; ref immer NIL
```

Damit war das Referenz-Bit `type-definition-reference` permanent `NIL` und

```lisp
(defun get_form (&_request)
  (declare (type Request &_request) ...))
```

wurde zu `fn get_form(&_request: Request)` statt zu
`fn get_form(_request: &Request)`. Der eingecheckte Stand von `02_webgcd`
enthält die *richtige* Fassung — er ist älter als die Regression.

### 2.4 `parse-lambda` hat Parametertypen stillschweigend verworfen

`parse-lambda` benutzte `(gethash p env)` direkt. Die Schlüssel in `env` sind
aber Strings (`remove-ampersand` formatiert das Symbol), der Lookup mit dem
Symbol traf also nie. Zusätzlich war die Reihenfolge C-artig (`i32 x`):

```
|| x|-> i32 { return x }     vorher
|x: i32| -> i32 { return x } nachher
```

`parse-lambda` und `parse-defun` teilen jetzt eine gemeinsame Funktion
`render-parameter`.

### 2.5 Präfixoperatoren klammerten die falsche Seite

Genau die Fehlerklasse aus Abschnitt 3.5 des cpp-Walkthroughs — in *beiden*
Modi falsch, also durch keinen Modusvergleich zu finden:

| Form | vorher | Rust liest das als | nachher |
| --- | --- | --- | --- |
| `(dot (deref p) x)` | `*(p).x` | `*((p).x)` | `(*p).x` |
| `(aref (deref p) 0)` | `*(p)[0]` | `*(p[0])` | `(*p)[0]` |
| `(dot (not p) x)` | `!(p).x` | `!((p).x)` | `(!p).x` |
| `(dot (% a b) c)` | `a%b.c` | `a%(b.c)` | `((a)%(b)).c` |

`deref`, `ref`, `not` klammern jetzt den *ganzen* Ausdruck. Neu: `ref-mut` für
`&mut x`.

Analog fehlte den binären Operatoren `< > <= >= == != % << >>` die äußere
Klammer um das Ergebnis (`(a)<=(b)` statt `((a)<=(b))`). `+ - * / and or logand
logior logxor` hatten sie schon. Jetzt sind alle einheitlich.

Der Steering-Hinweis war hier hilfreich: **für Rust brauchen wir keinen
`omit-parens`-Modus.** Die Korrektur läuft deshalb in die andere Richtung als in
`cl-cpp-generator2` — konsequent *mehr* klammern statt eine Präzedenztabelle zu
pflegen. Der Preis sind redundante Klammern, die `cargo fix` bzw. `cargo clippy
--fix` entfernen können.

Damit `rustc` nicht wegen `unused_parens` meckert, entfernt
`strip-outer-parens` eine Klammerebene genau an den Stellen, an denen der Lint
zuschlägt: `if`/`while`-Bedingung, `match`-Subjekt, `for`-Iterator,
`return`-Wert und rechte Seite einer Zuweisung. Die Funktion ist absichtlich
feige:

* Sie zählt Klammertiefe und bricht ab, wenn die erste Klammer nicht die letzte
  schließt (schützt gegen `(f(")"))`).
* Sie klammert nicht ab, wenn im Inhalt ein Komma auf Tiefe 0 steht — sonst
  würde `return (a, b)` zu `return a, b`. Dafür gibt es explizite
  Regressionstests (`return-tuple-kept`, `assign-tuple-kept`, `match-tuple-kept`).

### 2.6 `dotimes` erzeugte eine C-Schleife

```lisp
(emit `(for (,(format nil "int ~a = 0" ...) (< ...) (incf ...)) ,@body))
```

`for` erwartet aber `(item collection)` — der Aufruf brach mit einem
`destructuring-bind`-Fehler ab. Neu:

```lisp
(dotimes (i 4) ...)     -> for i in 0..4 { }
(dotimes (i 10 2) ...)  -> for i in (0..10).step_by(2) { }
```

### 2.7 Float-Ausgabe mit angehängten Leerzeichen

`~G` polstert rechts mit Blanks. Der Generator schrieb `let x = 0.50    ;`.
Außerdem lieferte `0s0` das Literal `0.` und `1.2d10` konnte `1.e+10` ergeben.
Neu: `clean-float-string` trimmt und repariert `0.` → `0.0`, `.5` → `0.5`,
`1.e+10` → `1.0e+10`. Im Diff von `13_vulkano` sieht man das an ~40 Stellen
(`position: [0., 0., 0.250]` → `position: [0.0, 0.0, 0.250]`).

### 2.8 `rustfmt` mit hartkodiertem Pfad

```lisp
(sb-ext:run-program "/home/martin/.cargo/bin/rustfmt" (list (namestring fn)))
```

Auf jeder anderen Maschine schlug das fehl (SBCL wirft, wenn das Programm nicht
existiert) — deshalb sind mehrere eingecheckte `main.rs` unformatiert.
Neu: `*rustfmt-program*` (Default `"rustfmt"`, über `PATH` gesucht),
`*rustfmt-arguments*`, `uiop:run-program`, Fehler nur als `warning`.
Damit ist `write-source` auch nicht mehr SBCL-gebunden.

### 2.9 Weitere kleinere Fehler

| Stelle | Befund |
| --- | --- |
| `variable-declaration` | `(declare (type (array i32 4) a))` ergab `[i32 ; (4)]` — `mapcar` als Liste formatiert. Neu `rust-array-type`, mehrdimensional verschachtelnd: `(array i32 2 3)` → `[[i32; 3]; 2]` |
| `parse-let` | `(funcall #'emit l)` — `#'emit` bezeichnet keine lexikalische Funktion, das wäre ein *undefined function* gewesen. Toter Zweig, entfernt |
| `parse-let` | `(destructuring-bind (name &optional value) decl)` scheiterte an einem Symbol ohne Initform; jetzt `(if (listp decl) decl (list decl))` |
| `case` | Die Arm-Bodies wurden **doppelt** emittiert (`(progn ,@(mapcar #'emit forms))`). Weil Strings unverändert durchlaufen, fiel es nicht auf — aber `do0` hält Strings für „fertig“ und unterdrückte alle Semikolons im Arm. Ein `hook-defun` hätte jede `defun` in einem `match`-Arm zweimal gesehen |
| `defstruct0` | Der Name lief nicht durch `emit`, also kein `--` → `::` |
| `write-source` | Schlüssel der Hash-Tabelle war `(sxhash pathname)`; bei Kollision wurde eine Datei nie geschrieben. Jetzt der Namestring in einer `equal`-Tabelle. Zusätzlich `ensure-directories-exist` |
| `emit-rs` | Der Zahlenzweig schrieb mit `(format str ...)` statt `(format nil ...)`; mit `:str <stream>` wäre der Rückgabewert `NIL` geworden |
| `consume-declare` | `(break "unknown declaration")` — im Batch-Betrieb (`--disable-debugger`) ein Abbruch ohne Meldung. Jetzt `error` |
| Kopf von `rs.lisp` | `#-nil (progn (ql:quickload …) (defpackage …))` duplizierte `package.lisp` und rief `quickload` *während* ASDF lud. Jetzt in `(unless (find-package …) …)` gekapselt |
| `*keywords-without-semicolon*` | `case` doppelt, `loop`/`when`/`unless` fehlten (→ `if c { } ;`), `include` stand drin, obwohl es die Form nicht mehr geben sollte |
| Whitespace | `"fn ~a ~a"`, `"if  ~a  ~a"`, `"for  ~a in"`, `"~a += ~a "`, `"let ~a ~@[ = ~a~]"` — kosmetisch, aber ohne `rustfmt` sichtbar |

## 3. C/C++-Altlasten

`rs.lisp` ist aus einem C-Generator entstanden. Acht Forms konnten nur
nicht-kompilierbares Rust erzeugen:

| Form | erzeugte | jetzt |
| --- | --- | --- |
| `handler-case` | `try { } catch (…) { }` | `error` mit Hinweis auf `(case … ((Ok v) …) ((Err e) …))` bzw. `?` |
| `throw` | `throw x` | `error`, Hinweis auf `Err`/`panic!` |
| `include` | `#include <stdio.h>` | `error`, Hinweis auf `use`/`mod` |
| `defclass` | `class X : public Y { }` | `error`, Hinweis auf `defstruct0` + `impl` |
| `protected`, `public` | `protected x` | `error`, Hinweis auf `"pub"` als String |
| `->` | `a->b` | `error`, Hinweis auf `dot` |
| `new` | `new Foo` | `error`, Hinweis auf assoziierte Funktionen |

Bewusste Entscheidung: **lauter Fehler statt stiller Durchfall.** Ohne
`case`-Klausel wären die Forms in den Funktionsaufruf-Zweig gefallen und hätten
`defclass(…)` erzeugt — schlechter als eine Fehlermeldung.

Zwei Forms haben eine sinnvolle Rust-Bedeutung bekommen statt gelöscht zu werden:

* `deftype` — war `typedef u64 myint`, jetzt `type myint = u64`
* `cast` — war der C-Cast `(u8) x`, jetzt Alias von `coerce`: `(cast x u8)` → `(x as u8)`.
  **Achtung:** die Argumentreihenfolge hat sich mitgedreht (Wert zuerst, wie bei
  `coerce`). In den Beispielen wird `cast` nicht benutzt.

Neu ergänzt, weil offensichtlich fehlend: `>` und `>=` (fielen vorher in den
Funktionsaufruf-Zweig und erzeugten `>(a, b)` — die alte README hat `(> x 0)`
sogar als Beispiel geführt), `break`, `continue`, `ref-mut`, sowie `string-r`
als Alias von `string#`.

## 4. Unerwartete Funde

### 4.1 `string-r` war weg, `02_webgcd` konnte nicht mehr generieren

`examples/02_webgcd/gen00.lisp` benutzt `(string-r "…")` für ein Rust-Rohliteral.
Die Form heißt inzwischen `string#`; `string-r` fiel in den
Funktionsaufruf-Zweig und wurde zu `string:r("…")`. Der eingecheckte
`main.rs` enthält aber `r#"…"#` — er ist älter als die Umbenennung.
`string-r` ist jetzt wieder ein Alias, `02_webgcd` erzeugt seinen historischen
Stand bit-identisch.

### 4.2 Ein Escape-Wechsel bei Strings hat eine Fehlermeldung verstümmelt — und eine andere Stelle zerbrochen

Früher galt die `-` → `:` Substitution für Symbole **und** Strings:

```lisp
((or (stringp code) (symbolp code))
 (substitute #\: #\- (format nil "~a" code)))
```

Das ist inzwischen (richtigerweise) getrennt, Strings laufen verbatim durch.
Zwei Konsequenzen, beide erst jetzt sichtbar:

* **Gefixt:** `06_parallel_text` schrieb `.expect("could not convert to utf:8")`
  — die Substitution hatte den Bindestrich in einer Benutzermeldung erwischt.
  Jetzt steht dort `utf-8`.
* **Kaputt:** dieselbe Datei deklariert `(declare (values "io--Result<()>"))` und
  verließ sich auf die Substitution. Ergebnis war
  `-> io--Result<()>`, was `rustfmt` mit *expected type, found `-`* ablehnt.

Ich habe die Beispielquelle korrigiert (`"io::Result<()>"`) statt die
Substitution in Strings wiederzubeleben. Begründung: in allen Beispielen gibt es
genau **eine** Stelle, die `--` in einem Roh-String braucht, aber mehrere, die
einen echten Bindestrich enthalten (Kommentare in `13_vulkano`). Verbatim ist
die richtige Semantik für die Escape-Luke.

### 4.3 Die eingecheckten Beispiele sind teils gar kein Rust

`examples/01_gcd/rs01_gcd/src/main.rs` — laut README „Finished“ — enthielt

```rust
fn gcd (n: mut u64, m: mut u64) -> u64 {
```

`mut u64` als Typ existiert nicht. Die Datei war außerdem nie durch `rustfmt`
gelaufen (Abschnitt 2.8). `07_glutin_gl` enthielt

```rust
gl::load_with(| symbol|{ (gl_window.get_proc_address(symbol) as *const); return _; });
```

— aus `(coerce (dot …) *const)` plus `(return _)`; gemeint war der Typ
`*const _`, aber `*const` und `_` waren zwei Lisp-Symbole. Auch das war schon im
eingecheckten Stand falsch und ist mir nur aufgefallen, weil `rustfmt` jetzt
tatsächlich läuft.

### 4.4 `let` vs. `let*` ist eine semantische Falle

`let` bindet unveränderlich, `let*` veränderlich — das ist der *einzige*
Unterschied, `let*` sequenzialisiert nichts. Solange Bug 2.1/2.2 aktiv waren,
kamen die `mut` aus einem anderen Codeweg, und mehrere Beispiele benutzten `let`,
wo `let mut` gebraucht wird. `01_gcd` und `02_webgcd` habe ich korrigiert
(`let*` bzw. `(declare (mutable n m))`). Beispiele 03–13 lassen sich hier nicht
prüfen, weil ihre Crates offline nicht auflösbar sind — sie sind syntaktisch
korrekt, aber `mut`-Fehler würde erst der Borrow-Checker finden.

### 4.5 `/=` heißt Division-Zuweisung, nicht „ungleich“

In Common Lisp ist `/=` der Ungleich-Test. Hier ist es `a /= b`. Wer aus
Gewohnheit `/=` schreibt, bekommt lautlos eine Zuweisung. Jetzt im Code
kommentiert, in `SUPPORTED_FORMS.md` und in der README dokumentiert. Für den
Vergleich ist `!=` zuständig.

### 4.6 Der globale Nebeneffekt beim Laden

`rs.lisp` führt auf Toplevel

```lisp
(setf (readtable-case *readtable*) :invert)
(declaim (optimize (speed 0) (safety 3) (debug 3)))
```

aus. Beides wirkt auf das *aktuelle* Readtable bzw. global für alles, was danach
kompiliert wird. Das `:invert` ist unverzichtbar (sonst überlebt kein `Ok`,
`Some` oder `CamelCase`), das `declaim` ist für eine Bibliothek unhöflich. Ich
habe nichts davon geändert, aber in der README dokumentiert — wer
`cl-rust-generator` in ein größeres Image lädt, sollte es wissen. Die Testsuite
setzt `:invert` selbst noch einmal, damit sie unabhängig von der Ladereihenfolge
funktioniert.

### 4.7 `progn` gegen `block`

`progn` lässt das letzte Semikolon weg (impliziter Rückgabewert), `block` setzt
es. Nach dem Fix von 2.2 heißt das: das *jeweils letzte* Statement eines
Funktionskörpers bzw. Schleifenkörpers steht ohne Semikolon da:

```rust
for x in v { filenames.push(entry.path()) }   // kein ;
```

Das ist gültig, solange der Ausdruck den Typ `()` hat — bei `push`, `+=` und `?`
auf `Result<()>` ist das der Fall. Wo es nicht passt, ist `block` die richtige
Form. In den 13 regenerierten Beispielen ist kein Fall aufgetreten, den
`rustfmt` bemängelt hätte; ein Typfehler wäre allerdings erst beim Kompilieren
sichtbar.

### 4.8 `trace.comp` in `13_vulkano` gehört nicht zu diesem Generator

`13_vulkano/gen00.lisp` erzeugt seinen Compute-Shader über
**`cl-cpp-generator2`**. Beim Regenerieren ändert sich dadurch auch
`code/src/trace.comp` — als Folge der dortigen Klammer-Arbeit von 2026-08-30,
nicht als Folge dieser Sitzung. Ich habe die Datei bewusst auf den
eingecheckten Stand zurückgesetzt, um die Commits nicht zu vermischen. Wer
`13_vulkano` neu baut, bekommt die (besseren) `0.0F`-Literale und `}` ohne
Semikolon automatisch.

### 4.9 Toter Code, nicht angefasst

Am Ende von `rs.lisp` stehen ~150 Zeilen in `#+nil` — Reste eines
Go-Generators (`definterface`, `parse-defmethod`, `chan`, `defer`, `go`). Sie
sind für den Reader unsichtbar und harmlos, aber verwirrend. Ich habe sie
stehengelassen, weil sie offenkundig als Notizzettel dienen.

## 5. Testsuite

`./run-tests.sh` — 109 Assertions, Exit-Code 1 bei Fehlern. Drei Schichten mit
absichtlich verschiedenen Fehlermodi:

1. **String-Tests** (`*test-cases*`, 104 Fälle). Vergleich gegen
   handverifizierte Referenzstrings, nach Whitespace-Normalisierung — Einrückung
   ist Aufgabe von `rustfmt`, nicht des Generators.
2. **`rustfmt`-Syntaxprüfung.** Fälle mit `:item t` sind vollständige
   Rust-Items; ihre Ausgabe wird `rustfmt --edition 2018` vorgeworfen. Ein
   Exit-Code ≠ 0 heißt: kein gültiges Rust. Diese Schicht fängt Fehler, die eine
   falsche Referenzerwartung nicht fängt.
3. **Wertetests** (`*value-tests*`, 16 Ausdrücke). Der Generator baut *ein*
   Rust-Programm, das jeden Ausdruck ausrechnet und mit einem erwarteten `i64`
   vergleicht; `rustc` kompiliert und führt es aus. Eine fehlende Klammer ändert
   hier den Wert, nicht nur den Text. Das ist die Schicht, die die
   Präzedenz-Korrekturen aus 2.5 semantisch absichert.

Warum kein Modusvergleich wie in `cl-cpp-generator2`: es gibt für Rust nur
*einen* Modus (vollständig geklammert). Der Wertetest ersetzt das Orakel.

Warum die Tests **im Paket `cl-rust-generator`** liegen: `emit-rs` dispatcht mit
`case` auf das Kopfsymbol. Ein `(dot a b)`, das in einem anderen Paket gelesen
wird, ist ein *anderes* Symbol (`dot` ist nicht exportiert) und würde stumm in
den Funktionsaufruf-Zweig fallen. Der Test wäre dann grün-für-die-falsche-Sache.

## 6. Dokumentation

* `SUPPORTED_FORMS.md` — aus den Testfällen generiert (`./generate-docs.sh`),
  gruppiert nach Tag, mit Inhaltsverzeichnis. Jedes Beispiel ist ein Testfall,
  also verifiziert.
* `README.org` neu. Die alte Fassung enthielt unter anderem:
  * `(write-source "src/main.rs" (emit-rs :code …))` — falsch, `write-source`
    nimmt die S-Expression, nicht den String.
  * `(if (= m 0) …)` als Vergleich — `=` ist die Zuweisung, `==` der Vergleich.
  * `(> x 0)` als Beispiel — die Form gab es nicht (jetzt schon).
  * `(char 'a')` und `(hex 255)` mit Lisp-untypischer Notation.
  * Ein `distance`-Beispiel, dessen behauptete Ausgabe (`let dx = …` ohne
    Typannotation) nicht zur gezeigten `declare`-Form passt.
  * Eine Statustabelle, die `01_gcd` als „Finished“ führt, obwohl die Datei kein
    gültiges Rust war.
  Die beiden Beispiele in der neuen README sind gegen die tatsächliche Ausgabe
  geprüft.

## 7. Verifikation

```sh
./run-tests.sh
# Transpiler tests run: 104 / Assertions passed: 109 / failed: 0
# Running 16 value tests via rustc... PASS

# alle generierten Beispiele neu erzeugen, keine rustfmt-Fehler mehr
for d in examples/0* examples/1[0-3]*; do
  (cd $d && sbcl --disable-debugger --load gen00.lisp --quit)
done

cd examples/01_gcd/rs01_gcd
cargo build --offline   # 0 warnings, 0 errors
cargo test  --offline   # test test_gcd ... ok
cargo run   --offline -- 12 18 30
# The greatest common divisor of [12, 18, 30] is 6
```

Nicht verifizierbar in dieser Umgebung: `02`–`13` brauchen Crates von crates.io
(offline nicht auflösbar). Für sie gilt nur die `rustfmt`-Syntaxprüfung; Typ- und
Borrow-Fehler (insbesondere fehlende `mut`, siehe 4.4) können dort weiterhin
schlummern.

## 8. Empfehlungen für den nächsten Durchgang

1. **`mut`-Audit für `02`–`13`.** Der `let`/`let*`-Wechsel ist die
   wahrscheinlichste verbleibende Fehlerquelle, und nur der Compiler findet sie.
   Ein Crate-Vendoring (`cargo vendor`) würde CI ermöglichen.
2. **Fehlende Rust-Konstrukte als Forms.** `enum`, `trait`, `if let`,
   `while let`, `pub`/`const`/`static`, Attribute und Generics werden heute alle
   über Strings gebaut. Das funktioniert, kostet aber die `--` → `::`
   Konvertierung und jede Prüfbarkeit.
3. **`f32`-Suffix.** `0.5s0` wird zu `0.50`, also einem `f64`-Literal, das nur
   dank Rusts Typinferenz passt. In generischem Kontext (`vec2(0.5)`) kann das
   knallen. Ein `f32`-Suffix wäre korrekter, würde aber die Ausgabe vieler
   Beispiele ändern und ist deshalb hier nicht angefasst.
4. **`hook-defun`** ist im Rust-Generator sinnlos (Rust braucht keine
   Vorwärtsdeklarationen) und wird von keinem Beispiel benutzt. Kandidat zum
   Entfernen.
