;;;; transpiler-tests.lisp --- unit tests for the s-expression -> Rust transpiler
;;;;
;;;; Run with ./run-tests.sh, or from a REPL:
;;;;   (load "transpiler-tests.lisp")
;;;;   (cl-rust-generator::run-transpiler-tests)
;;;;   (cl-rust-generator::generate-documentation)
;;;;
;;;; NOTE: the tests live *inside* the CL-RUST-GENERATOR package on purpose.
;;;; EMIT-RS dispatches with CASE on the head symbol, so a form like (dot a b)
;;;; read in another package would produce a different, non-matching symbol and
;;;; silently fall through to the "function call" branch.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cl-rust-generator)
  (ql:quickload "uiop")
  (ql:quickload "cl-ppcre"))

(in-package :cl-rust-generator)

;; rs.lisp inverts the readtable case; make sure that is also true while this
;; file is read, otherwise `Ok' and `dot' would not round-trip.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (readtable-case *readtable*) :invert))

;;; ===================================================================
;;; Tier 1 + 2: transpilation tests
;;;
;;; Each entry is a plist:
;;;   :name         short identifier
;;;   :description  what the form does (also used for SUPPORTED_FORMS.md)
;;;   :lisp         the s-expression (unquoted, *test-cases* is quoted as a whole)
;;;   :rust         expected output, compared after whitespace normalisation
;;;   :tags         list of tags, the first one groups the documentation
;;;   :item         when non-NIL the expected output is a complete Rust item and
;;;                 is additionally handed to rustfmt as a syntax check
;;; ===================================================================

(defparameter *test-cases*
  '(;; ---------------- literals ----------------
    (:name "integer-literal"
     :description "Positive integers are emitted verbatim."
     :lisp 42
     :rust "42"
     :tags (:literal))

    (:name "negative-integer-literal"
     :description "Negative integers are wrapped in parentheses so that they can
be used as an operand without gluing to a preceding operator."
     :lisp -42
     :rust "(-42)"
     :tags (:literal))

    (:name "f32-literal"
     :description "Single floats print with just enough digits to round-trip.
Note that no f32 suffix is added; the Rust type is inferred from the context."
     :lisp 0.5s0
     :rust "0.50"
     :tags (:literal))

    (:name "f32-zero-literal"
     :description "Zero must not degenerate to `0.', which the Rust lexer of the
generator's own test harness would still accept but which reads badly."
     :lisp 0.0s0
     :rust "0.0"
     :tags (:literal))

    (:name "f64-literal"
     :description "Double floats print with enough digits to round-trip."
     :lisp 3.14159265358979d0
     :rust "3.14159265359"
     :tags (:literal))

    (:name "f64-exponent-literal"
     :description "Large doubles use exponent notation."
     :lisp 1.2345678d10
     :rust "1.23456780e+10"
     :tags (:literal))

    (:name "string-literal"
     :description "(string x) emits a Rust string literal."
     :lisp (string "hello")
     :rust "\"hello\""
     :tags (:literal))

    (:name "raw-string-literal"
     :description "(string-r x) emits a raw string literal.  Enough hashes
are added so that the payload may contain quote-hash."
     :lisp (string-r "a\"b")
     :rust "r#\"a\"b\"#"
     :tags (:literal))

    (:name "byte-string-literal"
     :description "(string-b x) emits a byte string literal."
     :lisp (string-b "abc")
     :rust "b\"abc\""
     :tags (:literal))

    (:name "char-literal"
     :description "(char x) emits a character literal."
     :lisp (char a)
     :rust "'a'"
     :tags (:literal))

    (:name "byte-literal"
     :description "(byte x) emits a byte literal."
     :lisp (byte a)
     :rust "b'a'"
     :tags (:literal))

    (:name "hex-literal"
     :description "(hex n) prints n in hexadecimal."
     :lisp (hex 255)
     :rust "0xFF"
     :tags (:literal))

    ;; ---------------- symbols ----------------
    (:name "symbol-double-dash-is-path"
     :description "A single - in a symbol becomes :, so -- becomes the Rust path
separator ::.  This is why identifiers in generated code use underscores."
     :lisp (std--sync--Arc--new x)
     :rust "std::sync::Arc::new(x)"
     :tags (:core))

    (:name "string-escape-hatch"
     :description "Strings are passed through unchanged.  This is the escape
hatch for everything the generator has no form for."
     :lisp (do0 "#[derive(Clone)]" (defstruct0 Point (x f64)))
     :rust "#[derive(Clone)]
struct Point { x: f64, }"
     :tags (:core))

    (:name "angle"
     :description "(angle args*) emits turbofish brackets <...>."
     :lisp (angle Vec T)
     :rust "<VecT>"
     :tags (:core))

    (:name "scope-turbofish"
     :description "(scope a b c) joins with ::, so (scope name (angle type))
emits the turbofish call syntax name::<type>."
     :lisp (scope parse_pair (angle i32))
     :rust "parse_pair::<i32>"
     :tags (:core))

    ;; ---------------- arithmetic ----------------
    (:name "addition"
     :description "(+ a b ...) emits a parenthesised sum."
     :lisp (+ 1 2 3)
     :rust "(1+2+3)"
     :tags (:operator :arithmetic))

    (:name "subtraction"
     :description "(- a b) emits a difference, (- a) a unary minus."
     :lisp (- 5 3)
     :rust "(5-3)"
     :tags (:operator :arithmetic))

    (:name "negation"
     :description "A single argument to - is a unary minus."
     :lisp (- (+ a b))
     :rust "(-(a+b))"
     :tags (:operator :arithmetic))

    (:name "multiplication"
     :description "(* a b ...) parenthesises every factor."
     :lisp (* 2 3)
     :rust "((2)*(3))"
     :tags (:operator :arithmetic))

    (:name "division"
     :description "(/ a b) divides, (/ a) emits the reciprocal."
     :lisp (/ 10 2)
     :rust "((10)/(2))"
     :tags (:operator :arithmetic))

    (:name "reciprocal"
     :description "A single argument to / yields 1.0/x."
     :lisp (/ x)
     :rust "(1.0/(x))"
     :tags (:operator :arithmetic))

    (:name "modulo"
     :description "(% a b) is the remainder operator.  The result is
parenthesised, otherwise (dot (% a b) c) would regroup into (a)%((b).c)."
     :lisp (% a b)
     :rust "((a)%(b))"
     :tags (:operator :arithmetic))

    (:name "modulo-in-dot"
     :description "Regression test: a binary operator used as the receiver of a
method call must keep its own parentheses."
     :lisp (dot (% a b) (to_string))
     :rust "((a)%(b)).to_string()"
     :tags (:operator :precedence))

    ;; ---------------- omit-parens ----------------
    ;; Each case below is emitted with *OMIT-REDUNDANT-PARENS* bound (see
    ;; :omit-parens).  The fully parenthesised output stays the default.
    (:name "omit-add-mul"
     :description "With elision * binds tighter than +, so no parentheses
are needed."
     :lisp (+ 1 (* 2 3))
     :rust "1 + 2 * 3"
     :omit-parens t
     :tags (:operator :precedence))

    (:name "omit-call-tight-operand"
     :description "Calls bind tightest: no parentheses as ? operand or
dot receiver."
     :lisp (do0 (? (fetch row))
                (dot (fetch row) (to_string)))
     :rust "fetch(row)?; fetch(row).to_string();"
     :omit-parens t
     :tags (:operator :precedence))

    (:name "omit-mul-add"
     :description "A looser operand keeps its parentheses."
     :lisp (* (+ 1 2) 3)
     :rust "(1 + 2) * 3"
     :omit-parens t
     :tags (:operator :precedence))

    (:name "omit-sub-left"
     :description "Left-nested subtraction stays flat (left associative)."
     :lisp (- (- a b) c)
     :rust "a - b - c"
     :omit-parens t
     :tags (:operator :precedence))

    (:name "omit-sub-right"
     :description "Right-nested subtraction is parenthesised: a-(b-c) is
not (a-b)-c."
     :lisp (- a (- b c))
     :rust "a - (b - c)"
     :omit-parens t
     :tags (:operator :precedence))

    (:name "omit-div-right"
     :description "Same-level mixed * and / never stay flat on the right:
a*(b/c) is not (a*b)/c for integers."
     :lisp (* a (/ b c))
     :rust "a * (b / c)"
     :omit-parens t
     :tags (:operator :precedence))

    (:name "omit-compare-nested"
     :description "Comparisons cannot be chained in Rust (a==b==c is a
compile error), so a nested comparison is always parenthesised."
     :lisp (== a (== b c))
     :rust "a == (b == c)"
     :omit-parens t
     :tags (:operator :precedence))

    (:name "omit-bitand-compare"
     :description "Unlike C, Rust's bitwise & binds tighter than ==, so no
parentheses are needed here."
     :lisp (== (logand x mask) 0)
     :rust "x & mask == 0"
     :omit-parens t
     :tags (:operator :precedence))

    (:name "omit-and-or"
     :description "&& binds tighter than ||, so this stays flat."
     :lisp (or (and a b) c)
     :rust "a && b || c"
     :omit-parens t
     :tags (:operator :precedence))

    (:name "omit-neg-sum"
     :description "Unary minus keeps parentheses around a looser operand."
     :lisp (- (+ a b))
     :rust "-(a + b)"
     :omit-parens t
     :tags (:operator :precedence))

    (:name "omit-neg-neg"
     :description "Minus inside minus would glue into the invalid --x."
     :lisp (- (- x))
     :rust "-(-x)"
     :omit-parens t
     :tags (:operator :precedence))

    (:name "omit-not-compare"
     :description "The == operand binds looser than unary !, so it keeps
its parentheses."
     :lisp (not (== a b))
     :rust "!(a == b)"
     :omit-parens t
     :tags (:operator :precedence))

    (:name "omit-dot-receiver"
     :description "Method call binds tightest, so a binary receiver keeps
its parentheses even with elision."
     :lisp (dot (% a b) (to_string))
     :rust "(a % b).to_string()"
     :omit-parens t
     :tags (:operator :precedence))

    (:name "omit-deref-dot"
     :description "*(p).x parses as *((p).x), so the deref keeps its
parentheses as a dot receiver."
     :lisp (dot (deref p) x)
     :rust "(*p).x"
     :omit-parens t
     :tags (:operator :precedence))

    (:name "omit-coerce-sum"
     :description "`as' binds tighter than +, so the sum keeps its
parentheses."
     :lisp (coerce (+ a b) i64)
     :rust "(a + b) as i64"
     :omit-parens t
     :tags (:operator :precedence))

    (:name "omit-ref-add"
     :description "& binds tighter than +, but the operand is looser, so
&(a+b) keeps its parentheses."
     :lisp (ref (+ a b))
     :rust "&(a + b)"
     :omit-parens t
     :tags (:operator :precedence))

    (:name "omit-shift-add"
     :description "+ binds tighter than <<, so the sum needs no parentheses."
     :lisp (<< (+ 1 1) 3)
     :rust "1 + 1 << 3"
     :omit-parens t
     :tags (:operator :precedence))

    (:name "omit-shift-right-nested"
     :description "Shifts are left associative but not flattenable, so a
right-nested shift keeps its parentheses: 64>>(8>>1) is not (64>>8)>>1."
     :lisp (>> 64 (>> 8 1))
     :rust "64 >> (8 >> 1)"
     :omit-parens t
     :tags (:operator :precedence))

    (:name "omit-reciprocal"
     :description "The single-argument / reciprocal keeps a tight operand
bare."
     :lisp (/ x)
     :rust "1.0 / x"
     :omit-parens t
     :tags (:operator :precedence))

    (:name "omit-question"
     :description "A call needs no parentheses under ?."
     :lisp (? (dot f (read_to_string "&mut s")))
     :rust "f.read_to_string(&mut s)?"
     :omit-parens t
     :tags (:operator :precedence))

    (:name "omit-logior-chain"
     :description "Left-nested bitwise | stays flat."
     :lisp (logior (logior a b) c)
     :rust "a | b | c"
     :omit-parens t
     :tags (:operator :precedence))

    (:name "omit-logxor-right"
     :description "logxor is associative for integers, so a right-nested
chain stays flat like + and * do."
     :lisp (logxor a (logxor b c))
     :rust "a ^ b ^ c"
     :omit-parens t
     :tags (:operator :precedence))

    (:name "omit-logxor-mixed"
     :description "& binds tighter than ^, so no parentheses are needed."
     :lisp (logxor (logand a b) c)
     :rust "a & b ^ c"
     :omit-parens t
     :tags (:operator :precedence))

    (:name "omit-shl-left"
     :description "Left-nested shifts stay flat (left associative)."
     :lisp (<< (<< a 1) 2)
     :rust "a << 1 << 2"
     :omit-parens t
     :tags (:operator :precedence))

    (:name "omit-shr-left"
     :description "Same for >>."
     :lisp (>> (>> a 1) 2)
     :rust "a >> 1 >> 2"
     :omit-parens t
     :tags (:operator :precedence))

    (:name "omit-or-right"
     :description "|| is associative, so a right-nested chain stays flat."
     :lisp (or a (or b c))
     :rust "a || b || c"
     :omit-parens t
     :tags (:operator :precedence))

    (:name "omit-and-or-nested"
     :description "|| binds looser than &&, so it keeps its parentheses
as an && operand."
     :lisp (and (or a b) c)
     :rust "(a || b) && c"
     :omit-parens t
     :tags (:operator :precedence))

    (:name "omit-add-shift"
     :description "<< binds looser than +, so the shift keeps its
parentheses as a + operand."
     :lisp (+ (<< a 1) b)
     :rust "(a << 1) + b"
     :omit-parens t
     :tags (:operator :precedence))

    (:name "omit-add-right"
     :description "+ is associative, so a right-nested sum stays flat
(up to floating point rounding)."
     :lisp (+ 1 (+ 2 3))
     :rust "1 + 2 + 3"
     :omit-parens t
     :tags (:operator :precedence))

    ;; ---------------- bit operations ----------------
    (:name "bitwise-and"
     :description "(logand a b) emits Rust's bitwise &."
     :lisp (logand a b)
     :rust "((a) & (b))"
     :tags (:operator :bitwise))

    (:name "bitwise-or"
     :description "(logior a b) emits Rust's bitwise |."
     :lisp (logior a b)
     :rust "((a) | (b))"
     :tags (:operator :bitwise))

    (:name "bitwise-xor"
     :description "(logxor a b) emits Rust's bitwise ^."
     :lisp (logxor a b)
     :rust "((a) ^ (b))"
     :tags (:operator :bitwise))

    (:name "shift-left"
     :description "(<< a b) shifts left."
     :lisp (<< a 3)
     :rust "((a)<<(3))"
     :tags (:operator :bitwise))

    (:name "shift-right"
     :description "(>> a b) shifts right."
     :lisp (>> a 3)
     :rust "((a)>>(3))"
     :tags (:operator :bitwise))

    ;; ---------------- comparison / logic ----------------
    (:name "equality"
     :description "(== a b) compares for equality.  Note that Common Lisp's =
means assignment here and /= means division-assignment."
     :lisp (== a 5)
     :rust "((a)==(5))"
     :tags (:operator :comparison))

    (:name "inequality"
     :description "(!= a b) compares for inequality."
     :lisp (!= a 5)
     :rust "((a)!=(5))"
     :tags (:operator :comparison))

    (:name "less-than"
     :description "(< a b) compares."
     :lisp (< 3 5)
     :rust "((3)<(5))"
     :tags (:operator :comparison))

    (:name "greater-than"
     :description "(> a b) compares."
     :lisp (> 3 5)
     :rust "((3)>(5))"
     :tags (:operator :comparison))

    (:name "less-equal"
     :description "(<= a b) compares."
     :lisp (<= 3 5)
     :rust "((3)<=(5))"
     :tags (:operator :comparison))

    (:name "greater-equal"
     :description "(>= a b) compares."
     :lisp (>= 3 5)
     :rust "((3)>=(5))"
     :tags (:operator :comparison))

    (:name "logical-and"
     :description "(and a b) emits Rust's short circuiting &&."
     :lisp (and a b)
     :rust "((a)&&(b))"
     :tags (:operator :comparison))

    (:name "logical-or"
     :description "(or a b) emits Rust's short circuiting ||."
     :lisp (or a b)
     :rust "((a)||(b))"
     :tags (:operator :comparison))

    (:name "logical-not"
     :description "(not a) emits !a.  The parentheses enclose the whole
expression so that (dot (not a) b) does not turn into !((a).b)."
     :lisp (not a)
     :rust "(!a)"
     :tags (:operator :comparison))

    ;; ---------------- assignment ----------------
    (:name "assignment"
     :description "(= place value) assigns."
     :lisp (= x 5)
     :rust "x=5"
     :tags (:assignment))

    (:name "setf-multiple"
     :description "(setf a 1 b 2) expands into a sequence of assignments."
     :lisp (setf a 1 b 2)
     :rust "a=1;
b=2;"
     :tags (:assignment))

    (:name "incf"
     :description "(incf place [delta]) emits +=, delta defaults to 1."
     :lisp (incf i)
     :rust "i += 1"
     :tags (:assignment))

    (:name "decf"
     :description "(decf place [delta]) emits -=."
     :lisp (decf i 2)
     :rust "i -= 2"
     :tags (:assignment))

    (:name "divide-assign"
     :description "(/= place value) is division-assignment, NOT `not equal'."
     :lisp (/= i 2)
     :rust "i/=(2)"
     :tags (:assignment))

    (:name "times-assign"
     :description "(*= place value) is multiplication-assignment."
     :lisp (*= i 2)
     :rust "i*=(2)"
     :tags (:assignment))

    (:name "xor-assign"
     :description "(^= place value) is xor-assignment."
     :lisp (^= i 2)
     :rust "(i)^=(2)"
     :tags (:assignment))

    (:name "rem-assign"
     :description "(%= place [delta]) emits remainder-assignment %=."
     :lisp (%= i 2)
     :rust "i %= 2"
     :tags (:assignment))

    (:name "shl-assign"
     :description "(<<= place value) is shift-left-assignment."
     :lisp (<<= x 2)
     :rust "x<<=(2)"
     :tags (:assignment))

    (:name "shr-assign"
     :description "(>>= place value) is shift-right-assignment."
     :lisp (>>= x 2)
     :rust "x>>=(2)"
     :tags (:assignment))

    (:name "bitand-assign"
     :description "(&= place value) is bitwise-and-assignment."
     :lisp (&= x mask)
     :rust "x&=(mask)"
     :tags (:assignment))

    (:name "bitor-assign"
     :description "(\|= place value) is bitwise-or-assignment."
     :lisp (\|= x mask)
     :rust "x|=(mask)"
     :tags (:assignment))

    ;; ---------------- references / casts ----------------
    (:name "reference"
     :description "(ref x) takes a shared reference."
     :lisp (ref x)
     :rust "(&x)"
     :tags (:reference))

    (:name "mutable-reference"
     :description "(ref-mut x) takes a mutable reference."
     :lisp (ref-mut x)
     :rust "(&mut x)"
     :tags (:reference))

    (:name "dereference"
     :description "(deref x) dereferences."
     :lisp (deref x)
     :rust "(*x)"
     :tags (:reference))

    (:name "dereference-in-dot"
     :description "Regression test: *(p).x parses as *((p).x) in Rust, so the
parentheses have to enclose the deref, not its operand."
     :lisp (dot (deref p) x)
     :rust "(*p).x"
     :tags (:reference :precedence))

    (:name "coerce"
     :description "(coerce value type) emits Rust's `as' cast."
     :lisp (coerce x u8)
     :rust "(x as u8)"
     :tags (:reference))

    (:name "try-operator"
     :description "(? expr) appends Rust's ? error propagation operator."
     :lisp (? (dot f (read_to_string "&mut s")))
     :rust "f.read_to_string(&mut s)?"
     :tags (:reference))

    ;; ---------------- aggregates ----------------
    (:name "paren"
     :description "(paren a b) emits a comma separated, parenthesised list; also
used for tuples."
     :lisp (paren a b)
     :rust "(a, b)"
     :tags (:collection))

    (:name "values-is-paren"
     :description "(values ...) is an alias of paren, useful for tuples."
     :lisp (values a b)
     :rust "(a, b)"
     :tags (:collection))

    (:name "bracket"
     :description "(bracket a b) and (list a b) emit a Rust array literal."
     :lisp (list 1 2)
     :rust "[1, 2]"
     :tags (:collection))

    (:name "array-repeat"
     :description "(array-repeat value count) emits the repeat array
literal [value; count]."
     :lisp (array-repeat 0 50)
     :rust "[0; 50]"
     :tags (:collection))

    (:name "curly"
     :description "(curly a b) emits braces, e.g. for a use-list."
     :lisp (curly Read Write)
     :rust "{Read, Write}"
     :tags (:collection))

    (:name "comma"
     :description "(comma a b) joins with commas but adds no brackets."
     :lisp (comma a b)
     :rust "a, b"
     :tags (:collection))

    (:name "semicolon"
     :description "(semicolon a b) joins with semicolons."
     :lisp (semicolon a b)
     :rust "a; b"
     :tags (:collection))

    (:name "space"
     :description "(space a b) joins with blanks; the generic way to build
Rust syntax the generator has no dedicated form for."
     :lisp (space pub (defstruct0 P (x f64)))
     :rust "pub struct P { x: f64, }"
     :tags (:collection))

    (:name "aref"
     :description "(aref a i) indexes."
     :lisp (aref arr 0)
     :rust "arr[0]"
     :tags (:indexing))

    (:name "aref-multi"
     :description "Several indices are separated by commas (for types whose
Index impl takes a tuple-like argument)."
     :lisp (aref arr i j)
     :rust "arr[i,j]"
     :tags (:indexing))

    (:name "range"
     :description "(range a b) emits the end-exclusive Rust range a..b."
     :lisp (range 0 n)
     :rust "(0..n)"
     :tags (:indexing))

    (:name "range-inclusive"
     :description "(range-inclusive a b) emits the end-inclusive range a..=b."
     :lisp (range-inclusive 0 n)
     :rust "(0..=n)"
     :tags (:indexing))

    (:name "range-from"
     :description "(range-from a) emits the open-ended range a.. ."
     :lisp (range-from 2)
     :rust "(2..)"
     :tags (:indexing))

    (:name "range-to"
     :description "(range-to b) emits the left-open range ..b."
     :lisp (range-to 5)
     :rust "(..5)"
     :tags (:indexing))

    (:name "range-to-inclusive"
     :description "(range-to-inclusive b) emits ..=b."
     :lisp (range-to-inclusive 5)
     :rust "(..=5)"
     :tags (:indexing))

    (:name "range-full"
     :description "(range-full) emits the full range .. ."
     :lisp (range-full)
     :rust "(..)"
     :tags (:indexing))

    (:name "range-in-for"
     :description "A range used as a for collection loses its redundant
parentheses."
     :lisp (for (x (range 0 n)) (f x))
     :rust "for x in 0..n { f(x) }"
     :tags (:indexing))

    (:name "dot"
     :description "(dot a b c) chains field accesses and method calls."
     :lisp (dot v (iter) (map f) (collect))
     :rust "v.iter().map(f).collect()"
     :tags (:accessor))

    (:name "dot-tuple-field"
     :description "A numeric field access is Rust's tuple field access."
     :lisp (dot pair 0)
     :rust "pair.0"
     :tags (:accessor))

    ;; ---------------- control flow ----------------
    (:name "if-then"
     :description "(if c then) emits an if with a block.  The redundant
parentheses that the comparison operators add are stripped from the condition so
that rustc's unused_parens lint stays quiet."
     :lisp (if (< x 0) (return 0))
     :rust "if (x)<(0) { return 0 }"
     :tags (:control-flow))

    (:name "if-then-else"
     :description "(if c then else) emits both branches."
     :lisp (if (< x 0) (return 0) (return 1))
     :rust "if (x)<(0) { return 0 } else { return 1 }"
     :tags (:control-flow))

    (:name "when"
     :description "(when c form*) is an if without an else branch that accepts
several body forms."
     :lisp (when (< x 0) (f) (g))
     :rust "if (x)<(0) { f(); g() }"
     :tags (:control-flow))

    (:name "unless"
     :description "(unless c form*) negates the condition."
     :lisp (unless x (f))
     :rust "if !x { f() }"
     :tags (:control-flow))

    (:name "if-let"
     :description "(if-let (pattern scrutinee) then else) emits Rust's if
let.  A list pattern such as (Some x) emits the tuple-struct pattern
Some(x); None and _ work as written."
     :lisp (if-let ((Some x) y) (return x) (return 0))
     :rust "if let Some(x) = y { return x } else { return 0 }"
     :tags (:control-flow))

    (:name "if-let-no-else"
     :description "Without an else form if-let emits a bare if let."
     :lisp (if-let ((Some x) y) (return x))
     :rust "if let Some(x) = y { return x }"
     :tags (:control-flow))

    (:name "while-let"
     :description "(while-let (pattern scrutinee) form*) emits Rust's
while let loop."
     :lisp (while-let ((Some x) (dot it (next))) (f x))
     :rust "while let Some(x) = it.next() { f(x) }"
     :tags (:control-flow))

    (:name "let-else"
     :description "(let-else (pattern scrutinee) form*) emits Rust's
let-else.  The form terminates itself with a semicolon (Rust requires
it), so it is safe in any statement position."
     :lisp (let-else ((Some x) y) (return 1))
     :rust "let Some(x) = y else { return 1 };"
     :tags (:binding))

    (:name "let-else-in-do0"
     :description "let-else in statement position needs no help: the
following statement still gets its own semicolon and nothing is doubled."
     :lisp (do0 (let-else ((Some x) y) (return 1)) (g))
     :rust "let Some(x) = y else { return 1 }; g();"
     :tags (:binding))

    (:name "while"
     :description "(while c form*) emits a while loop."
     :lisp (while (< i n) (incf i))
     :rust "while (i)<(n) { i += 1 }"
     :tags (:control-flow))

    (:name "loop"
     :description "(loop form*) emits Rust's unconditional loop."
     :lisp (loop (break))
     :rust "loop { break }"
     :tags (:control-flow))

    (:name "break-with-value"
     :description "(break value) breaks out of a loop with a value."
     :lisp (loop (break 3))
     :rust "loop { break 3 }"
     :tags (:control-flow))

    (:name "continue"
     :description "(continue) emits continue."
     :lisp (loop (continue))
     :rust "loop { continue }"
     :tags (:control-flow))

    (:name "for-in"
     :description "(for (item collection) form*) emits Rust's for-in loop."
     :lisp (for (x v) (f x))
     :rust "for x in v { f(x) }"
     :tags (:control-flow))

    (:name "dotimes"
     :description "(dotimes (i n) form*) iterates over the range 0..n."
     :lisp (dotimes (i 4) (f i))
     :rust "for i in 0..4 { f(i) }"
     :tags (:control-flow))

    (:name "dotimes-step"
     :description "A third element in the dotimes head becomes step_by."
     :lisp (dotimes (i 10 2) (f i))
     :rust "for i in (0..10).step_by(2) { f(i) }"
     :tags (:control-flow))

    (:name "match"
     :description "(case keyform (key form*) ... (t form*)) emits a Rust match.
A key of t becomes the wildcard arm _."
     :lisp (case x
             (1 (f))
             (t (g)))
     :rust "match x { 1 => { f() }, _ => { g() }, }"
     :tags (:control-flow))

    (:name "match-pattern"
     :description "A list key is emitted as a pattern, so enum variants with
bindings work."
     :lisp (case r
             ((Ok v) v)
             ((Err e) (return (Err e))))
     :rust "match r { Ok(v) => { v }, Err(e) => { return Err(e) }, }"
     :tags (:control-flow))

    (:name "match-arm-statements"
     :description "Regression test: several forms in one match arm must be
separated by semicolons."
     :lisp (case x
             (1 (f) (g)))
     :rust "match x { 1 => { f(); g() }, }"
     :tags (:control-flow))

    (:name "return"
     :description "(return x) returns from a function.  Redundant enclosing
parentheses are removed so that rustc's unused_parens lint stays quiet."
     :lisp (return 0)
     :rust "return 0"
     :tags (:control-flow))

    (:name "return-expression-unwrapped"
     :description "The parentheses that the arithmetic forms add around their
result are stripped from a return value."
     :lisp (return (+ a b))
     :rust "return a+b"
     :tags (:control-flow :precedence))

    (:name "return-tuple-kept"
     :description "Regression test for the paren stripping: a tuple must keep
its parentheses, so the stripping bails out on a top level comma."
     :lisp (return (paren a b))
     :rust "return (a, b)"
     :tags (:control-flow :precedence))

    (:name "assign-expression-unwrapped"
     :description "Same stripping on the right hand side of an assignment."
     :lisp (setf m (% m n))
     :rust "m=(m)%(n);"
     :tags (:control-flow :precedence))

    (:name "assign-tuple-kept"
     :description "A tuple on the right hand side keeps its parentheses."
     :lisp (setf m (paren a b))
     :rust "m=(a, b);"
     :tags (:control-flow :precedence))

    (:name "match-tuple-kept"
     :description "Matching on a tuple keeps the parentheses."
     :lisp (case (paren a b)
             ((paren 1 2) (f)))
     :rust "match (a, b) { (1, 2) => { f() }, }"
     :tags (:control-flow :precedence))

    ;; ---------------- blocks ----------------
    (:name "do0"
     :description "(do0 form*) emits one form per line at the current
indentation and terminates each with a semicolon where needed."
     :lisp (do0 (f) (g))
     :rust "f(); g();"
     :tags (:block))

    (:name "progn"
     :description "(progn form*) is do0 in braces, but without a semicolon after
the last form, so the block has the value of that form (Rust's implicit return)."
     :lisp (progn (f) (g))
     :rust "{ f(); g() }"
     :tags (:block))

    (:name "block"
     :description "(block form*) is like progn but terminates the last form too,
so the block evaluates to ()."
     :lisp (block (f) (g))
     :rust "{ f(); g(); }"
     :tags (:block))

    (:name "unsafe"
     :description "(unsafe form*) emits an unsafe block."
     :lisp (unsafe (f))
     :rust "unsafe { f() }"
     :tags (:block))

    (:name "extern-block"
     :description "(extern form*) emits an extern block."
     :lisp (extern "fn puts(s: *const u8) -> i32;")
     :rust "extern { fn puts(s: *const u8) -> i32; }"
     :tags (:block))

    (:name "do"
     :description "(do form*) sequences statements like do0 but indented one
level deeper; it is what function and block bodies are built from."
     :lisp (do (f) (g))
     :rust "f(); g();"
     :tags (:block))

    (:name "stmt"
     :description "(stmt form) forces statement termination with a semicolon.
It is the explicit override for escape-hatch forms such as (space ...)
in statement position, where the semicolon heuristic cannot know whether
the expansion is a statement or an expression."
     :lisp (do0 (stmt (space foo bar)) (g))
     :rust "foo bar; g();"
     :tags (:block))

    (:name "expr"
     :description "(expr form) is the counterpart of stmt: it forces
expression position, i.e. no semicolon is added.  Use it for the tail
of a progn, where a missing semicolon means Rust returns the value."
     :lisp (do0 (expr (space foo bar)) (g))
     :rust "foo bar g();"
     :tags (:block))

    (:name "progn-assign-no-semi"
     :description "An assignment in tail position keeps implicit-return
semantics: no semicolon is added, so the block evaluates to the
assigned value.  This is the semicolon rule the whole generator is
built around: a missing semicolon means `return this value'."
     :lisp (progn (= x 5))
     :rust "{ x=5 }"
     :tags (:block))

    (:name "block-assign-semi"
     :description "block is the opposite of progn: the last form is
terminated too, so the block evaluates to ()."
     :lisp (block (= x 5))
     :rust "{ x=5; }"
     :tags (:block))

    ;; ---------------- bindings ----------------
    (:name "let-immutable"
     :description "(let ((x v)) ...) binds immutably by default.  The whole
form emits a Rust block, so the bindings stay scoped like in Common Lisp
and the block evaluates to its last form (implicit return)."
     :lisp (let ((x 5)) (f x))
     :rust "{ let x = 5; f(x) }"
     :tags (:binding))

    (:name "let-typed"
     :description "A (declare (type ...)) inside the let body annotates the
binding."
     :lisp (let ((x 5))
             (declare (type i32 x))
             (f x))
     :rust "{ let x: i32 = 5; f(x) }"
     :tags (:binding))

    (:name "let-star-mutable"
     :description "let* makes every binding mutable by default; that is the only
difference to let."
     :lisp (let* ((x 5)) (setf x 6))
     :rust "{ let mut x = 5; x=6; }"
     :tags (:binding))

    (:name "let-declare-mutable"
     :description "(declare (mutable x)) forces a mut binding inside a plain
let, (declare (immutable x)) does the opposite inside let*."
     :lisp (let ((x 5))
             (declare (type i32 x)
                      (mutable x))
             (setf x 6))
     :rust "{ let mut x: i32 = 5; x=6; }"
     :tags (:binding))

    (:name "let-array-type"
     :description "An (array element-type dim...) type declaration becomes a
Rust array type; several dimensions nest."
     :lisp (let ((a))
             (declare (type (array i32 4) a))
             (f a))
     :rust "{ let a: [i32; 4]; f(a) }"
     :tags (:binding))

    (:name "let-array-type-2d"
     :description "Two dimensions nest as [[T; inner]; outer]."
     :lisp (let ((a))
             (declare (type (array i32 2 3) a))
             (f a))
     :rust "{ let a: [[i32; 3]; 2]; f(a) }"
     :tags (:binding))

    (:name "let-statement-position"
     :description "In statement position (do0, defun body, loop body) the let
block needs no semicolon of its own; the following statement is unaffected
and the binding does not leak out of the block."
     :lisp (do0 (let ((x 5)) (f x)) (g))
     :rust "{ let x = 5; f(x) } g();"
     :tags (:binding))

    ;; ---------------- functions ----------------
    (:name "defun-untyped"
     :description "(defun name (args) body) emits a Rust fn."
     :lisp (defun main ()
             (println! (string "hi")))
     :rust "fn main() { println!(\"hi\") }"
     :item t
     :tags (:function))

    (:name "defun-typed"
     :description "(declare (type T a b)) types the parameters, (declare (values
T)) the return value."
     :lisp (defun gcd (n m)
             (declare (type u64 n m)
                      (values u64))
             (return n))
     :rust "fn gcd(n: u64, m: u64) -> u64 { return n }"
     :item t
     :tags (:function))

    (:name "defun-multiple-values"
     :description "Several return values become a Rust tuple."
     :lisp (defun f ()
             (declare (values u64 u64))
             (return (paren 1 2)))
     :rust "fn f() -> (u64, u64) { return (1, 2) }"
     :item t
     :tags (:function))

    (:name "defun-reference-parameter"
     :description "A leading & on a declared parameter name turns the parameter
into a Rust reference; the & moves from the name to the type."
     :lisp (defun get_form (&_request)
             (declare (type Request &_request)
                      (values "IronResult<Response>"))
             (return (Ok r)))
     :rust "fn get_form(_request: &Request) -> IronResult<Response> { return Ok(r) }"
     :item t
     :tags (:function))

    (:name "defun-mutable-reference-parameter"
     :description "A mutable reference parameter becomes x: &mut T; a mutable
by-value parameter becomes mut x: T, because that is where Rust wants the mut."
     :lisp (defun f (&a b)
             (declare (type i32 &a b)
                      (mutable &a b))
             (return 0))
     :rust "fn f(a: &mut i32, mut b: i32) { return 0 }"
     :item t
     :tags (:function))

    (:name "defun-string-parameter"
     :description "Undeclared parameters are emitted verbatim.  Writing them as
strings is the escape hatch used for self, lifetimes and generics."
     :lisp (defun merge ("&mut self" "other: InMemoryIndex")
             (return 0))
     :rust "fn merge(&mut self, other: InMemoryIndex) { return 0 }"
     :tags (:function))

    (:name "defun-async"
     :description "(defun-async name (args) body) emits an async Rust fn
for tokio/axum handlers."
     :lisp (defun-async fetch_summary (identifier)
             (declare (type i64 identifier)
                      (values String))
             (return (await (db--fetch_summary identifier))))
     :rust "async fn fetch_summary(identifier: i64) -> String { return db::fetch_summary(identifier).await }"
     :item t
     :tags (:function))

    (:name "await"
     :description "(await expr) appends .await; outer parentheses are
stripped like in return and conditions."
     :lisp (await (fetch_summary identifier))
     :rust "fetch_summary(identifier).await"
     :tags (:reference))

    (:name "question-mark"
     :description "(? expr) emits Rust's ? error-propagation operator."
     :lisp (? (parse_url link))
     :rust "parse_url(link)?"
     :tags (:reference))

    (:name "attr"
     :description "(attr s* form) emits one #[s] line per attribute string
in front of form."
     :lisp (attr "derive(Clone, Debug)"
             (defstruct0 Point (x f64) (y f64)))
     :rust "#[derive(Clone, Debug)] struct Point { x: f64, y: f64, }"
     :item t
     :tags (:item))

    (:name "defenum"
     :description "(defenum name variant*) emits a unit-variant Rust enum."
     :lisp (defenum GenerationStatus Queued Running Succeeded Failed)
     :rust "enum GenerationStatus { Queued, Running, Succeeded, Failed, }"
     :item t
     :tags (:item))

    (:name "vec-macro"
     :description "(vec! a b) emits the vec! macro with bracket syntax."
     :lisp (vec! 1 2)
     :rust "vec![1, 2]"
     :tags (:collection))

    (:name "defenum-derived"
     :description "Derives reach defenum through the attr wrapper."
     :lisp (attr "derive(Debug, Clone, Copy, PartialEq, Eq)"
             (defenum ThinkingPreference Auto Minimal Low Medium High))
     :rust "#[derive(Debug, Clone, Copy, PartialEq, Eq)] enum ThinkingPreference { Auto, Minimal, Low, Medium, High, }"
     :item t
     :tags (:item))

    (:name "attr-no-semicolon"
     :description "attr and defun-async are in *keywords-without-semicolon*:
do0 adds no stray semicolon after attributed items."
     :lisp (do0 (attr "derive(Debug)"
                  (defstruct0 A (x i32)))
                (defun-async f ()
                  (declare (values i32))
                  (return 0)))
     :rust "#[derive(Debug)] struct A { x: i32, } async fn f() -> i32 { return 0 }"
     :tags (:item))

    (:name "lambda"
     :description "(lambda (args) body) emits a Rust closure.  Declared
parameter types and return values are honoured."
     :lisp (lambda (x)
             (declare (type i32 x)
                      (values i32))
             (return x))
     :rust "|x: i32| -> i32 { return x }"
     :tags (:function))

    (:name "lambda-untyped"
     :description "Without declarations the closure has no type annotations."
     :lisp (lambda (x) (* x 2))
     :rust "|x| { ((x)*(2)) }"
     :tags (:function))

    (:name "function-call"
     :description "Any unknown head is emitted as a function call."
     :lisp (foo a b)
     :rust "foo(a, b)"
     :tags (:function))

    (:name "call-of-expression"
     :description "A list in head position is parenthesised and then called,
which is how a closure is invoked."
     :lisp ((lambda (x) (return x)) 1)
     :rust "(|x| { return x })(1)"
     :tags (:function))

    ;; ---------------- items ----------------
    (:name "defstruct0"
     :description "(defstruct0 name (slot type) ...) emits a Rust struct."
     :lisp (defstruct0 Point (x f64) (y f64))
     :rust "struct Point { x: f64, y: f64, }"
     :item t
     :tags (:item))

    (:name "make-instance"
     :description "(make-instance Name :slot value ...) emits a struct literal."
     :lisp (make-instance Point :x 1 :y 2)
     :rust "Point {x: 1, y: 2}"
     :tags (:item))

    (:name "macroexpand"
     :description "(macroexpand name :key value ...) emits a brace-style macro
invocation, e.g. for vulkano's single_pass_renderpass!."
     :lisp (macroexpand implement_vertex! :position 1)
     :rust "implement_vertex! {position: 1}"
     :tags (:item))

    (:name "impl"
     :description "(impl name form*) emits an impl block.  Use (impl (space
Trait for Type) ...) for trait impls."
     :lisp (impl Point
                 (defun x (&self)
                   (return 0)))
     :rust "impl Point { fn x(&self) { return 0 } }"
     :item t
     :tags (:item))

    (:name "deftype"
     :description "(deftype name () type) emits a Rust type alias."
     :lisp (deftype Res () "std::io::Result<()>")
     :rust "type Res = std::io::Result<()>"
     :tags (:item))

    (:name "use"
     :description "(use (a b) (c d)) emits one use declaration per list."
     :lisp (use (std io) (std (curly Read Write)))
     :rust "use std::io;
use std::{Read, Write};"
     :item t
     :tags (:item))

    (:name "mod"
     :description "(mod a b) declares modules."
     :lisp (mod read write)
     :rust "mod read;
mod write;"
     :tags (:item))

    (:name "struct-reference"
     :description "(struct name) emits just the struct keyword and a name."
     :lisp (struct Point)
     :rust "struct Point"
     :tags (:item))

    (:name "deftrait"
     :description "(deftrait name (defun method (args) declare*) ...) emits a
Rust trait.  Only the signatures are emitted; write bounds into the name
as a string, e.g. \"Shape: Debug\"."
     :lisp (deftrait Shape
             (defun area (&self)
               (declare (values f64))))
     :rust "trait Shape { fn area(&self) -> f64; }"
     :item t
     :tags (:item))))

;;; ===================================================================
;;; Tier 3: value tests -- one generated Rust program checks the numeric
;;; value of precedence sensitive expressions at run time.  A missing or
;;; misplaced pair of parentheses changes the value, not just the text.
;;; ===================================================================

(defparameter *value-tests*
  '(("add-mul"         (+ 1 (* 2 3))                      7)
    ("mul-add"         (* (+ 1 2) 3)                      9)
    ("sub-unary"       (- (- 3 7))                        4)
    ("div-mul"         (* 12 (/ 6 3))                     24)
    ("mod-sub"         (- (% 17 5) 1)                     1)
    ("mod-of-sum"      (% (+ 17 3) 6)                     2)
    ("shift-add"       (<< (+ 1 1) 3)                     16)
    ("shift-then-add"  (+ (<< 1 3) 1)                     9)
    ("and-or-mix"      (if (or (and (== 1 1) (== 1 0))
                               (== 1 1))
                        1 0)                              1)
    ("bitand-or"       (logior (logand 12 10) 1)          9)
    ("xor-precedence"  (logxor (+ 1 2) 1)                 2)
    ("compare-not"     (if (not (< 3 2)) 1 0)             1)
    ("nested-compare"  (if (== (% 10 3) 1) 1 0)           1)
    ("coerce-of-sum"   (coerce (+ 1 2) i64)               3)
    ("neg-literal"     (+ (- 5) 7)                        2)
    ("unary-div"       (coerce (/ 4.0d0) i64)             0)
    ("sub-right-nested" (- 10 (- 4 3))                     9)
    ("div-right-nested" (/ 100 (/ 10 2))                   20)
    ("shift-right-nested" (>> 64 (>> 8 1))                 4)
    ("bitand-compare"   (if (== (logand 6 3) 2) 1 0)       1)
    ("or-and-mix"       (if (or (and (== 1 0) (== 1 0))
                               (!= 2 3))
                            1 0)                          1)
    ("neg-of-neg"       (- (- 5))                          5)
    ("coerce-shift"     (coerce (<< 1 4) i64)              16)
    ("shl-left-nested"  (<< (<< 1 2) 3)                   32)
    ("shr-left-nested"  (>> (>> 64 1) 2)                  8)
    ("xor-chain"        (logxor (logxor 7 3) 1)            5)
    ("xor-right-nested" (logxor 7 (logxor 3 1))            5)
    ("bitor-basic"      (logior 5 3)                       7)
    ("or-right-nested"  (if (or (== 1 0) (or (== 2 2) (== 3 4))) 1 0) 1)
    ("add-right-nested" (+ 1 (+ 2 3))                     6)
    ("coerce-bitand"    (coerce (logand 6 3) i64)          2)))

;;; ===================================================================
;;; helpers
;;; ===================================================================

(defun normalize-whitespace (s)
  "Collapse every run of whitespace into a single blank and trim the result.
Indentation is rustfmt's job, so the tests must not depend on it."
  (string-trim '(#\Space)
	       (cl-ppcre:regex-replace-all "\\s+" s " ")))

(defun rustfmt-accepts-p (rust-code)
  "Write RUST-CODE to a temporary .rs file and let rustfmt parse it.  Returns
(values ok-p stderr).  This is a cheap syntax check for complete Rust items."
  (uiop:with-temporary-file (:pathname p :stream s :type "rs" :keep nil)
    (write-string rust-code s)
    (finish-output s)
    (close s)
    (multiple-value-bind (out err code)
	(uiop:run-program (list "rustfmt" "--edition" "2018"
				(uiop:native-namestring p))
			  :output :string :error-output :string
			  :ignore-error-status t)
      (declare (ignorable out))
      (values (eql 0 code) err))))

(defun run-value-tests (&key (tests *value-tests*) (verbose t)
			 (omit-parens nil))
  "Emit one Rust program that evaluates every expression in TESTS and compares
it against the expected value.  Returns the number of failures.  With
OMIT-PARENS the program is generated with *OMIT-REDUNDANT-PARENS* bound,
so the same expectations differentially verify the elision: a dropped
pair of parentheses changes the computed value, not just the text."
  (let* ((program
	  (let ((*omit-redundant-parens* omit-parens))
	    (emit-rs
	     :code `(do0
		     (defun main ()
		       (let* ((failed 0))
			 (declare (type i64 failed))
			 ,@(loop for (name expr expected) in tests
				 append
				 `((let ((v ,expr))
				     (declare (type i64 v))
				     (when (!= v ,expected)
				       (println! (string ,(format nil "FAIL ~a: {} != {}" name))
						 v ,expected)
				       (incf failed)))))
			 (when (< 0 failed)
			   ("std::process::exit" 1))))))))
	 (dir (uiop:ensure-directory-pathname
	       (format nil "~a/cl-rust-generator-value-tests" (uiop:temporary-directory))))
	 (src (merge-pathnames (if omit-parens
				   "value_tests_omit_parens.rs"
				   "value_tests.rs")
			       dir))
	 (bin (merge-pathnames (if omit-parens
				   "value_tests_omit_parens"
				   "value_tests")
			       dir)))
    (ensure-directories-exist dir)
    (with-open-file (s src :direction :output :if-exists :supersede
			   :if-does-not-exist :create)
      (write-string program s))
    (multiple-value-bind (out err code)
	(uiop:run-program (list "rustc" "--edition" "2018" "-A" "unused_parens"
				"-o" (uiop:native-namestring bin)
				(uiop:native-namestring src))
			  :output :string :error-output :string
			  :ignore-error-status t)
      (declare (ignorable out))
      (unless (eql 0 code)
	(format t "~&value tests~@[ (omit-parens)~]: rustc failed~%~a~%--- generated source ---~%~a~%"
		omit-parens err program)
	(return-from run-value-tests (length tests))))
    (multiple-value-bind (out err code)
	(uiop:run-program (list (uiop:native-namestring bin))
			  :output :string :error-output :string
			  :ignore-error-status t)
      (declare (ignorable err))
      (when verbose
	(format t "~&Running ~D value tests~@[ (omit-parens)~] via rustc... ~:[FAIL~;PASS~]~%"
		(length tests) omit-parens (eql 0 code))
	(unless (eql 0 code)
	  (format t "~a~%" out)))
      (if (eql 0 code) 0 1))))

(defun run-transpiler-tests (&key (tests *test-cases*) (tags nil) (value-tests t))
  "Run the transpiler test suite.  Tier 1 compares the emitted Rust against a
reference string (whitespace normalised); cases with :OMIT-PARENS T are
emitted with *OMIT-REDUNDANT-PARENS* bound.  Tier 2 hands complete items
to rustfmt as a syntax check.  Tier 3 compiles and runs the value tests
twice: fully parenthesised and with elision (differential oracle)."
  (let ((passed 0)
	(failed 0)
	(test-count 0)
	(selected (if tags
		      (remove-if-not (lambda (tc) (intersection tags (getf tc :tags))) tests)
		      tests)))
    (format t "~&Running ~D transpiler tests...~%" (length selected))
    (dolist (tc selected)
      (incf test-count)
      (block one-test
	(let* ((name (getf tc :name))
	       (lisp-code (getf tc :lisp))
	       (expected (getf tc :rust))
	       (actual (handler-case
			   (let ((*omit-redundant-parens* (getf tc :omit-parens)))
			     (emit-rs :code lisp-code))
			 (error (e)
			   (incf failed)
			   (format t "~&[~D] ~A... ERROR ~A~%" test-count name e)
			   (return-from one-test)))))
	  (format t "~&[~D] ~A... " test-count name)
	  (if (string= (normalize-whitespace actual)
		       (normalize-whitespace expected))
	      (format t "TRANSPILE [PASS]")
	      (progn
		(incf failed)
		(format t "TRANSPILE [FAIL]~%  expected: ~S~%  got:      ~S~%"
			(normalize-whitespace expected)
			(normalize-whitespace actual))
		(return-from one-test)))
	  ;; tier 2: is the output actually parseable Rust?
	  (if (getf tc :item)
	      (multiple-value-bind (ok err) (rustfmt-accepts-p actual)
		(if ok
		    (progn (incf passed) (format t ", RUSTFMT [PASS]~%"))
		    (progn (incf failed)
			   (format t ", RUSTFMT [FAIL]~%~a~%" err))))
	      (progn (incf passed) (format t "~%"))))))
    (let ((value-failed (if value-tests
			      (+ (run-value-tests)
				 ;; differential oracle for the elision: the
				 ;; same expectations must hold with redundant
				 ;; parentheses omitted.
				 (run-value-tests :omit-parens t))
			      0)))
      (incf failed value-failed)
      (format t "~2&--- Test Summary ---~%")
      (format t "Transpiler tests run: ~D~%" test-count)
      (format t "Assertions passed:    ~D~%" passed)
      (format t "Assertions failed:    ~D~%" failed)
      (format t "--------------------~%")
      (unless (= 0 failed) (uiop:quit 1)))))

(defun generate-documentation (&key (tests *test-cases*)
				 (output-file (asdf:system-relative-pathname
					       'cl-rust-generator "SUPPORTED_FORMS.md")))
  "Write a markdown overview of every supported form, derived from the tests."
  (let ((*package* (find-package :cl-rust-generator)))
    (with-open-file (s output-file :direction :output :if-exists :supersede
				   :if-does-not-exist :create)
      (format s "# Supported S-Expression Forms~%~%")
      (format s "Auto-generated from `transpiler-tests.lisp` by~%")
      (format s "`(cl-rust-generator::generate-documentation)`. Do not edit by hand.~%~%")
      (format s "Every example below is an executed test case: the Lisp form on the~%")
      (format s "left really does produce the Rust on the right.~%~%")
      (let ((groups (make-hash-table :test 'equal)))
	(dolist (tc tests)
	  (push tc (gethash (first (getf tc :tags)) groups)))
	;; table of contents
	(format s "## Contents~%~%")
	(loop for tag in (sort (alexandria:hash-table-keys groups) #'string<)
	      do (format s "- [~(~a~)](#~(~a~)-forms)~%" tag tag))
	(format s "~%")
	(loop for tag in (sort (alexandria:hash-table-keys groups) #'string<)
	      do
		 (format s "## ~(~a~) Forms~%~%" tag)
		 (dolist (tc (reverse (gethash tag groups)))
		   (format s "### `~S`~%~%" (getf tc :lisp))
		   (format s "~A~%~%" (getf tc :description))
		   (format s "```lisp~%~S~%```~%~%" (getf tc :lisp))
		   (format s "```rust~%~A~%```~%~%"
			   (handler-case
			       (let ((*omit-redundant-parens*
				       (getf tc :omit-parens)))
				 (emit-rs :code (getf tc :lisp)))
			     (error (e) (format nil "<error: ~a>" e))))))))
    output-file))
