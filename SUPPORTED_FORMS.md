# Supported S-Expression Forms

Auto-generated from `transpiler-tests.lisp` by
`(cl-rust-generator::generate-documentation)`. Do not edit by hand.

Every example below is an executed test case: the Lisp form on the
left really does produce the Rust on the right.

## Contents

- [accessor](#accessor-forms)
- [assignment](#assignment-forms)
- [binding](#binding-forms)
- [block](#block-forms)
- [collection](#collection-forms)
- [control-flow](#control-flow-forms)
- [core](#core-forms)
- [function](#function-forms)
- [indexing](#indexing-forms)
- [item](#item-forms)
- [literal](#literal-forms)
- [operator](#operator-forms)
- [reference](#reference-forms)

## accessor Forms

### `(dot v (iter) (map f) (collect))`

(dot a b c) chains field accesses and method calls.

```lisp
(dot v (iter) (map f) (collect))
```

```rust
v.iter().map(f).collect()
```

### `(dot pair 0)`

A numeric field access is Rust's tuple field access.

```lisp
(dot pair 0)
```

```rust
pair.0
```

## assignment Forms

### `(= x 5)`

(= place value) assigns.

```lisp
(= x 5)
```

```rust
x=5
```

### `(setf a 1
           b 2)`

(setf a 1 b 2) expands into a sequence of assignments.

```lisp
(setf a 1
      b 2)
```

```rust
a=1;
b=2;
```

### `(incf i)`

(incf place [delta]) emits +=, delta defaults to 1.

```lisp
(incf i)
```

```rust
i += 1
```

### `(decf i 2)`

(decf place [delta]) emits -=.

```lisp
(decf i 2)
```

```rust
i -= 2
```

### `(/= i 2)`

(/= place value) is division-assignment, NOT `not equal'.

```lisp
(/= i 2)
```

```rust
i/=(2)
```

### `(*= i 2)`

(*= place value) is multiplication-assignment.

```lisp
(*= i 2)
```

```rust
i*=(2)
```

### `(^= i 2)`

(^= place value) is xor-assignment.

```lisp
(^= i 2)
```

```rust
(i)^=(2)
```

### `(%= i 2)`

(%= place [delta]) emits remainder-assignment %=.

```lisp
(%= i 2)
```

```rust
i %= 2
```

### `(<<= x 2)`

(<<= place value) is shift-left-assignment.

```lisp
(<<= x 2)
```

```rust
x<<=(2)
```

### `(>>= x 2)`

(>>= place value) is shift-right-assignment.

```lisp
(>>= x 2)
```

```rust
x>>=(2)
```

### `(&= x mask)`

(&= place value) is bitwise-and-assignment.

```lisp
(&= x mask)
```

```rust
x&=(mask)
```

### `(|\|=| x mask)`

(|= place value) is bitwise-or-assignment.

```lisp
(|\|=| x mask)
```

```rust
x|=(mask)
```

## binding Forms

### `(let-else ((Some x) y) (return 1))`

(let-else (pattern scrutinee) form*) emits Rust's
let-else.  The form terminates itself with a semicolon (Rust requires
it), so it is safe in any statement position.

```lisp
(let-else ((Some x) y) (return 1))
```

```rust
let Some(x) = y else {
    return 1
};
```

### `(do0 (let-else ((Some x) y) (return 1)) (g))`

let-else in statement position needs no help: the
following statement still gets its own semicolon and nothing is doubled.

```lisp
(do0 (let-else ((Some x) y) (return 1)) (g))
```

```rust
let Some(x) = y else {
    return 1
};
g();
```

### `(let ((x 5))
       (f x))`

(let ((x v)) ...) binds immutably by default.  The whole
form emits a Rust block, so the bindings stay scoped like in Common Lisp
and the block evaluates to its last form (implicit return).

```lisp
(let ((x 5))
  (f x))
```

```rust
{
    let x = 5;
    f(x)
}
```

### `(let ((x 5))
       (declare (type i32 x))
       (f x))`

A (declare (type ...)) inside the let body annotates the
binding.

```lisp
(let ((x 5))
  (declare (type i32 x))
  (f x))
```

```rust
{
    let x: i32 = 5;
    f(x)
}
```

### `(let* ((x 5))
       (setf x 6))`

let* makes every binding mutable by default; that is the only
difference to let.

```lisp
(let* ((x 5))
  (setf x 6))
```

```rust
{
    let mut x = 5;
        x=6;
}
```

### `(let ((x 5))
       (declare (type i32 x)
                (mutable x))
       (setf x 6))`

(declare (mutable x)) forces a mut binding inside a plain
let, (declare (immutable x)) does the opposite inside let*.

```lisp
(let ((x 5))
  (declare (type i32 x)
           (mutable x))
  (setf x 6))
```

```rust
{
    let mut x: i32 = 5;
        x=6;
}
```

### `(let ((a))
       (declare (type (array i32 4) a))
       (f a))`

An (array element-type dim...) type declaration becomes a
Rust array type; several dimensions nest.

```lisp
(let ((a))
  (declare (type (array i32 4) a))
  (f a))
```

```rust
{
    let a: [i32; 4];
    f(a)
}
```

### `(let ((a))
       (declare (type (array i32 2 3) a))
       (f a))`

Two dimensions nest as [[T; inner]; outer].

```lisp
(let ((a))
  (declare (type (array i32 2 3) a))
  (f a))
```

```rust
{
    let a: [[i32; 3]; 2];
    f(a)
}
```

### `(do0
      (let ((x 5))
        (f x))
      (g))`

In statement position (do0, defun body, loop body) the let
block needs no semicolon of its own; the following statement is unaffected
and the binding does not leak out of the block.

```lisp
(do0
 (let ((x 5))
   (f x))
 (g))
```

```rust
{
    let x = 5;
    f(x)
}
g();
```

## block Forms

### `(do0 (f) (g))`

(do0 form*) emits one form per line at the current
indentation and terminates each with a semicolon where needed.

```lisp
(do0 (f) (g))
```

```rust
f();
g();
```

### `(progn (f) (g))`

(progn form*) is do0 in braces, but without a semicolon after
the last form, so the block has the value of that form (Rust's implicit return).

```lisp
(progn (f) (g))
```

```rust
{
    f();
    g()
}
```

### `(block (f) (g))`

(block form*) is like progn but terminates the last form too,
so the block evaluates to ().

```lisp
(block (f) (g))
```

```rust
{
    f();
    g();
}
```

### `(unsafe (f))`

(unsafe form*) emits an unsafe block.

```lisp
(unsafe (f))
```

```rust
unsafe {
    f()
}
```

### `(extern "fn puts(s: *const u8) -> i32;")`

(extern form*) emits an extern block.

```lisp
(extern "fn puts(s: *const u8) -> i32;")
```

```rust
extern {
    fn puts(s: *const u8) -> i32;
}
```

### `(do (f) (g))`

(do form*) sequences statements like do0 but indented one
level deeper; it is what function and block bodies are built from.

```lisp
(do (f) (g))
```

```rust
    f();
    g();
```

### `(do0 (stmt (space foo bar)) (g))`

(stmt form) forces statement termination with a semicolon.
It is the explicit override for escape-hatch forms such as (space ...)
in statement position, where the semicolon heuristic cannot know whether
the expansion is a statement or an expression.

```lisp
(do0 (stmt (space foo bar)) (g))
```

```rust
foo bar;
g();
```

### `(do0 (expr (space foo bar)) (g))`

(expr form) is the counterpart of stmt: it forces
expression position, i.e. no semicolon is added.  Use it for the tail
of a progn, where a missing semicolon means Rust returns the value.

```lisp
(do0 (expr (space foo bar)) (g))
```

```rust
foo bar
g();
```

### `(progn (= x 5))`

An assignment in tail position keeps implicit-return
semantics: no semicolon is added, so the block evaluates to the
assigned value.  This is the semicolon rule the whole generator is
built around: a missing semicolon means `return this value'.

```lisp
(progn (= x 5))
```

```rust
{
    x=5
}
```

### `(block (= x 5))`

block is the opposite of progn: the last form is
terminated too, so the block evaluates to ().

```lisp
(block (= x 5))
```

```rust
{
    x=5;
}
```

## collection Forms

### `(paren a b)`

(paren a b) emits a comma separated, parenthesised list; also
used for tuples.

```lisp
(paren a b)
```

```rust
(a, b)
```

### `(values a b)`

(values ...) is an alias of paren, useful for tuples.

```lisp
(values a b)
```

```rust
(a, b)
```

### `(list 1 2)`

(bracket a b) and (list a b) emit a Rust array literal.

```lisp
(list 1 2)
```

```rust
[1, 2]
```

### `(array-repeat 0 50)`

(array-repeat value count) emits the repeat array
literal [value; count].

```lisp
(array-repeat 0 50)
```

```rust
[0; 50]
```

### `(curly Read Write)`

(curly a b) emits braces, e.g. for a use-list.

```lisp
(curly Read Write)
```

```rust
{Read, Write}
```

### `(comma a b)`

(comma a b) joins with commas but adds no brackets.

```lisp
(comma a b)
```

```rust
a, b
```

### `(semicolon a b)`

(semicolon a b) joins with semicolons.

```lisp
(semicolon a b)
```

```rust
a; b
```

### `(space pub (defstruct0 P (x f64)))`

(space a b) joins with blanks; the generic way to build
Rust syntax the generator has no dedicated form for.

```lisp
(space pub (defstruct0 P (x f64)))
```

```rust
pub struct P {
    x: f64,
}
```

### `(vec! 1 2)`

(vec! a b) emits the vec! macro with bracket syntax.

```lisp
(vec! 1 2)
```

```rust
vec![1, 2]
```

## control-flow Forms

### `(if (< x 0)
         (return 0))`

(if c then) emits an if with a block.  The redundant
parentheses that the comparison operators add are stripped from the condition so
that rustc's unused_parens lint stays quiet.

```lisp
(if (< x 0)
    (return 0))
```

```rust
if (x)<(0) {
    return 0
}
```

### `(if (< x 0)
         (return 0)
         (return 1))`

(if c then else) emits both branches.

```lisp
(if (< x 0)
    (return 0)
    (return 1))
```

```rust
if (x)<(0) {
    return 0
} else {
    return 1
}
```

### `(when (< x 0) (f) (g))`

(when c form*) is an if without an else branch that accepts
several body forms.

```lisp
(when (< x 0) (f) (g))
```

```rust
if (x)<(0) {
    f();
    g()
}
```

### `(unless x (f))`

(unless c form*) negates the condition.

```lisp
(unless x (f))
```

```rust
if !x {
    f()
}
```

### `(if-let ((Some x) y)
       (return x)
       (return 0))`

(if-let (pattern scrutinee) then else) emits Rust's if
let.  A list pattern such as (Some x) emits the tuple-struct pattern
Some(x); None and _ work as written.

```lisp
(if-let ((Some x) y)
  (return x)
  (return 0))
```

```rust
if let Some(x) = y {
    return x
} else {
    return 0
}
```

### `(if-let ((Some x) y)
       (return x))`

Without an else form if-let emits a bare if let.

```lisp
(if-let ((Some x) y)
  (return x))
```

```rust
if let Some(x) = y {
    return x
}
```

### `(while-let ((Some x) (dot it (next))) (f x))`

(while-let (pattern scrutinee) form*) emits Rust's
while let loop.

```lisp
(while-let ((Some x) (dot it (next))) (f x))
```

```rust
while let Some(x) = it.next() {
    f(x)
}
```

### `(while (< i n) (incf i))`

(while c form*) emits a while loop.

```lisp
(while (< i n) (incf i))
```

```rust
while (i)<(n) {
    i += 1
}
```

### `(loop (break))`

(loop form*) emits Rust's unconditional loop.

```lisp
(loop (break))
```

```rust
loop {
    break
}
```

### `(loop (break 3))`

(break value) breaks out of a loop with a value.

```lisp
(loop (break 3))
```

```rust
loop {
    break 3
}
```

### `(loop (continue))`

(continue) emits continue.

```lisp
(loop (continue))
```

```rust
loop {
    continue
}
```

### `(for (x v) (f x))`

(for (item collection) form*) emits Rust's for-in loop.

```lisp
(for (x v) (f x))
```

```rust
for x in v {
    f(x)
}
```

### `(dotimes (i 4) (f i))`

(dotimes (i n) form*) iterates over the range 0..n.

```lisp
(dotimes (i 4) (f i))
```

```rust
for i in 0..4 {
    f(i)
}
```

### `(dotimes (i 10 2) (f i))`

A third element in the dotimes head becomes step_by.

```lisp
(dotimes (i 10 2) (f i))
```

```rust
for i in (0..10).step_by(2) {
    f(i)
}
```

### `(case x (1 (f)) (t (g)))`

(case keyform (key form*) ... (t form*)) emits a Rust match.
A key of t becomes the wildcard arm _.

```lisp
(case x (1 (f)) (t (g)))
```

```rust
match x {
    1 => {
    f()
},
    _ => {
    g()
},
}
```

### `(case r ((Ok v) v) ((Err e) (return (Err e))))`

A list key is emitted as a pattern, so enum variants with
bindings work.

```lisp
(case r ((Ok v) v) ((Err e) (return (Err e))))
```

```rust
match r {
    Ok(v) => {
    v
},
    Err(e) => {
    return Err(e)
},
}
```

### `(case x (1 (f) (g)))`

Regression test: several forms in one match arm must be
separated by semicolons.

```lisp
(case x (1 (f) (g)))
```

```rust
match x {
    1 => {
    f();
    g()
},
}
```

### `(return 0)`

(return x) returns from a function.  Redundant enclosing
parentheses are removed so that rustc's unused_parens lint stays quiet.

```lisp
(return 0)
```

```rust
return 0
```

### `(return (+ a b))`

The parentheses that the arithmetic forms add around their
result are stripped from a return value.

```lisp
(return (+ a b))
```

```rust
return a+b
```

### `(return (paren a b))`

Regression test for the paren stripping: a tuple must keep
its parentheses, so the stripping bails out on a top level comma.

```lisp
(return (paren a b))
```

```rust
return (a, b)
```

### `(setf m (% m n))`

Same stripping on the right hand side of an assignment.

```lisp
(setf m (% m n))
```

```rust
m=(m)%(n);
```

### `(setf m (paren a b))`

A tuple on the right hand side keeps its parentheses.

```lisp
(setf m (paren a b))
```

```rust
m=(a, b);
```

### `(case (paren a b) ((paren 1 2) (f)))`

Matching on a tuple keeps the parentheses.

```lisp
(case (paren a b) ((paren 1 2) (f)))
```

```rust
match (a, b) {
    (1, 2) => {
    f()
},
}
```

## core Forms

### `(std--sync--Arc--new x)`

A single - in a symbol becomes :, so -- becomes the Rust path
separator ::.  This is why identifiers in generated code use underscores.

```lisp
(std--sync--Arc--new x)
```

```rust
std::sync::Arc::new(x)
```

### `(do0 "#[derive(Clone)]" (defstruct0 Point (x f64)))`

Strings are passed through unchanged.  This is the escape
hatch for everything the generator has no form for.

```lisp
(do0 "#[derive(Clone)]" (defstruct0 Point (x f64)))
```

```rust
#[derive(Clone)]
struct Point {
    x: f64,
}
```

### `(angle Vec T)`

(angle args*) emits turbofish brackets <...>.

```lisp
(angle Vec T)
```

```rust
<VecT>
```

### `(scope parse_pair (angle i32))`

(scope a b c) joins with ::, so (scope name (angle type))
emits the turbofish call syntax name::<type>.

```lisp
(scope parse_pair (angle i32))
```

```rust
parse_pair::<i32>
```

## function Forms

### `(defun main () (println! (string "hi")))`

(defun name (args) body) emits a Rust fn.

```lisp
(defun main () (println! (string "hi")))
```

```rust
fn main() {
    println!("hi")
}
```

### `(defun gcd (n m)
       (declare (type u64 n m)
                (values u64))
       (return n))`

(declare (type T a b)) types the parameters, (declare (values
T)) the return value.

```lisp
(defun gcd (n m)
  (declare (type u64 n m)
           (values u64))
  (return n))
```

```rust
fn gcd(n: u64, m: u64) -> u64 {
    return n
}
```

### `(defun f () (declare (values u64 u64)) (return (paren 1 2)))`

Several return values become a Rust tuple.

```lisp
(defun f () (declare (values u64 u64)) (return (paren 1 2)))
```

```rust
fn f() -> (u64, u64) {
    return (1, 2)
}
```

### `(defun get_form (&_request)
       (declare (type Request &_request)
                (values "IronResult<Response>"))
       (return (Ok r)))`

A leading & on a declared parameter name turns the parameter
into a Rust reference; the & moves from the name to the type.

```lisp
(defun get_form (&_request)
  (declare (type Request &_request)
           (values "IronResult<Response>"))
  (return (Ok r)))
```

```rust
fn get_form(_request: &Request) -> IronResult<Response> {
    return Ok(r)
}
```

### `(defun f (&a b)
       (declare (type i32 &a b)
                (mutable &a b))
       (return 0))`

A mutable reference parameter becomes x: &mut T; a mutable
by-value parameter becomes mut x: T, because that is where Rust wants the mut.

```lisp
(defun f (&a b)
  (declare (type i32 &a b)
           (mutable &a b))
  (return 0))
```

```rust
fn f(a: &mut i32, mut b: i32) {
    return 0
}
```

### `(defun merge ("&mut self" "other: InMemoryIndex") (return 0))`

Undeclared parameters are emitted verbatim.  Writing them as
strings is the escape hatch used for self, lifetimes and generics.

```lisp
(defun merge ("&mut self" "other: InMemoryIndex") (return 0))
```

```rust
fn merge(&mut self, other: InMemoryIndex) {
    return 0
}
```

### `(defun-async fetch_summary (identifier)
      (declare (type i64 identifier)
               (values String))
      (return (await (db--fetch_summary identifier))))`

(defun-async name (args) body) emits an async Rust fn
for tokio/axum handlers.

```lisp
(defun-async fetch_summary (identifier)
 (declare (type i64 identifier)
          (values String))
 (return (await (db--fetch_summary identifier))))
```

```rust
async fn fetch_summary(identifier: i64) -> String {
    return db::fetch_summary(identifier).await
}
```

### `(lambda (x)
       (declare (type i32 x)
                (values i32))
       (return x))`

(lambda (args) body) emits a Rust closure.  Declared
parameter types and return values are honoured.

```lisp
(lambda (x)
  (declare (type i32 x)
           (values i32))
  (return x))
```

```rust
|x: i32| -> i32 {
    return x
}
```

### `(lambda (x) (* x 2))`

Without declarations the closure has no type annotations.

```lisp
(lambda (x) (* x 2))
```

```rust
|x| {
    ((x)*(2))
}
```

### `(foo a b)`

Any unknown head is emitted as a function call.

```lisp
(foo a b)
```

```rust
foo(a, b)
```

### `((lambda (x) (return x)) 1)`

A list in head position is parenthesised and then called,
which is how a closure is invoked.

```lisp
((lambda (x) (return x)) 1)
```

```rust
(|x| {
    return x
})(1)
```

## indexing Forms

### `(aref arr 0)`

(aref a i) indexes.

```lisp
(aref arr 0)
```

```rust
arr[0]
```

### `(aref arr i j)`

Several indices are separated by commas (for types whose
Index impl takes a tuple-like argument).

```lisp
(aref arr i j)
```

```rust
arr[i,j]
```

### `(range 0 n)`

(range a b) emits the end-exclusive Rust range a..b.

```lisp
(range 0 n)
```

```rust
(0..n)
```

### `(range-inclusive 0 n)`

(range-inclusive a b) emits the end-inclusive range a..=b.

```lisp
(range-inclusive 0 n)
```

```rust
(0..=n)
```

### `(range-from 2)`

(range-from a) emits the open-ended range a.. .

```lisp
(range-from 2)
```

```rust
(2..)
```

### `(range-to 5)`

(range-to b) emits the left-open range ..b.

```lisp
(range-to 5)
```

```rust
(..5)
```

### `(range-to-inclusive 5)`

(range-to-inclusive b) emits ..=b.

```lisp
(range-to-inclusive 5)
```

```rust
(..=5)
```

### `(range-full)`

(range-full) emits the full range .. .

```lisp
(range-full)
```

```rust
(..)
```

### `(for (x (range 0 n)) (f x))`

A range used as a for collection loses its redundant
parentheses.

```lisp
(for (x (range 0 n)) (f x))
```

```rust
for x in 0..n {
    f(x)
}
```

## item Forms

### `(attr "derive(Clone, Debug)" (defstruct0 Point (x f64) (y f64)))`

(attr s* form) emits one #[s] line per attribute string
in front of form.

```lisp
(attr "derive(Clone, Debug)" (defstruct0 Point (x f64) (y f64)))
```

```rust
#[derive(Clone, Debug)]
struct Point {
    x: f64,
    y: f64,
}
```

### `(defenum GenerationStatus Queued Running Succeeded Failed)`

(defenum name variant*) emits a unit-variant Rust enum.

```lisp
(defenum GenerationStatus Queued Running Succeeded Failed)
```

```rust
enum GenerationStatus {
    Queued,
    Running,
    Succeeded,
    Failed,
}
```

### `(attr "derive(Debug, Clone, Copy, PartialEq, Eq)"
      (defenum ThinkingPreference Auto Minimal Low Medium High))`

Derives reach defenum through the attr wrapper.

```lisp
(attr "derive(Debug, Clone, Copy, PartialEq, Eq)"
 (defenum ThinkingPreference Auto Minimal Low Medium High))
```

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ThinkingPreference {
    Auto,
    Minimal,
    Low,
    Medium,
    High,
}
```

### `(do0 (attr "derive(Debug)" (defstruct0 A (x i32)))
      (defun-async f nil (declare (values i32)) (return 0)))`

attr and defun-async are in *keywords-without-semicolon*:
do0 adds no stray semicolon after attributed items.

```lisp
(do0 (attr "derive(Debug)" (defstruct0 A (x i32)))
 (defun-async f nil (declare (values i32)) (return 0)))
```

```rust
#[derive(Debug)]
struct A {
    x: i32,
}
async fn f() -> i32 {
    return 0
}
```

### `(defstruct0 Point (x f64) (y f64))`

(defstruct0 name (slot type) ...) emits a Rust struct.

```lisp
(defstruct0 Point (x f64) (y f64))
```

```rust
struct Point {
    x: f64,
    y: f64,
}
```

### `(make-instance Point :x 1 :y 2)`

(make-instance Name :slot value ...) emits a struct literal.

```lisp
(make-instance Point :x 1 :y 2)
```

```rust
Point {x: 1, y: 2}
```

### `(macroexpand implement_vertex! :position 1)`

(macroexpand name :key value ...) emits a brace-style macro
invocation, e.g. for vulkano's single_pass_renderpass!.

```lisp
(macroexpand implement_vertex! :position 1)
```

```rust
implement_vertex! {position: 1}
```

### `(impl Point (defun x (&self) (return 0)))`

(impl name form*) emits an impl block.  Use (impl (space
Trait for Type) ...) for trait impls.

```lisp
(impl Point (defun x (&self) (return 0)))
```

```rust
impl Point {
    fn x(&self) {
        return 0
}
}
```

### `(deftype Res () "std::io::Result<()>")`

(deftype name () type) emits a Rust type alias.

```lisp
(deftype Res () "std::io::Result<()>")
```

```rust
type Res = std::io::Result<()>
```

### `(use (std io) (std (curly Read Write)))`

(use (a b) (c d)) emits one use declaration per list.

```lisp
(use (std io) (std (curly Read Write)))
```

```rust
use std::io;
use std::{Read, Write};

```

### `(mod read write)`

(mod a b) declares modules.

```lisp
(mod read write)
```

```rust
mod read;
mod write;

```

### `(struct Point)`

(struct name) emits just the struct keyword and a name.

```lisp
(struct Point)
```

```rust
struct Point
```

### `(deftrait Shape (defun area (&self) (declare (values f64))))`

(deftrait name (defun method (args) declare*) ...) emits a
Rust trait.  Only the signatures are emitted; write bounds into the name
as a string, e.g. "Shape: Debug".

```lisp
(deftrait Shape (defun area (&self) (declare (values f64))))
```

```rust
trait Shape {
    fn area(&self) -> f64;
}
```

## literal Forms

### `42`

Positive integers are emitted verbatim.

```lisp
42
```

```rust
42
```

### `-42`

Negative integers are wrapped in parentheses so that they can
be used as an operand without gluing to a preceding operator.

```lisp
-42
```

```rust
(-42)
```

### `0.5`

Single floats print with just enough digits to round-trip.
Note that no f32 suffix is added; the Rust type is inferred from the context.

```lisp
0.5
```

```rust
0.50
```

### `0.0`

Zero must not degenerate to `0.', which the Rust lexer of the
generator's own test harness would still accept but which reads badly.

```lisp
0.0
```

```rust
0.0
```

### `3.14159265358979d0`

Double floats print with enough digits to round-trip.

```lisp
3.14159265358979d0
```

```rust
3.14159265359
```

### `1.2345678d10`

Large doubles use exponent notation.

```lisp
1.2345678d10
```

```rust
1.23456780e+10
```

### `(string "hello")`

(string x) emits a Rust string literal.

```lisp
(string "hello")
```

```rust
"hello"
```

### `(string-r "a\"b")`

(string-r x) emits a raw string literal.  Enough hashes
are added so that the payload may contain quote-hash.

```lisp
(string-r "a\"b")
```

```rust
r#"a"b"#
```

### `(string-b "abc")`

(string-b x) emits a byte string literal.

```lisp
(string-b "abc")
```

```rust
b"abc"
```

### `(char a)`

(char x) emits a character literal.

```lisp
(char a)
```

```rust
'a'
```

### `(byte a)`

(byte x) emits a byte literal.

```lisp
(byte a)
```

```rust
b'a'
```

### `(hex 255)`

(hex n) prints n in hexadecimal.

```lisp
(hex 255)
```

```rust
0xFF
```

## operator Forms

### `(+ 1 2 3)`

(+ a b ...) emits a parenthesised sum.

```lisp
(+ 1 2 3)
```

```rust
(1+2+3)
```

### `(- 5 3)`

(- a b) emits a difference, (- a) a unary minus.

```lisp
(- 5 3)
```

```rust
(5-3)
```

### `(- (+ a b))`

A single argument to - is a unary minus.

```lisp
(- (+ a b))
```

```rust
(-(a+b))
```

### `(* 2 3)`

(* a b ...) parenthesises every factor.

```lisp
(* 2 3)
```

```rust
((2)*(3))
```

### `(/ 10 2)`

(/ a b) divides, (/ a) emits the reciprocal.

```lisp
(/ 10 2)
```

```rust
((10)/(2))
```

### `(/ x)`

A single argument to / yields 1.0/x.

```lisp
(/ x)
```

```rust
(1.0/(x))
```

### `(% a b)`

(% a b) is the remainder operator.  The result is
parenthesised, otherwise (dot (% a b) c) would regroup into (a)%((b).c).

```lisp
(% a b)
```

```rust
((a)%(b))
```

### `(dot (% a b) (to_string))`

Regression test: a binary operator used as the receiver of a
method call must keep its own parentheses.

```lisp
(dot (% a b) (to_string))
```

```rust
((a)%(b)).to_string()
```

### `(+ 1 (* 2 3))`

With elision * binds tighter than +, so no parentheses
are needed.

```lisp
(+ 1 (* 2 3))
```

```rust
1 + 2 * 3
```

### `(do0 (? (fetch row)) (dot (fetch row) (to_string)))`

Calls bind tightest: no parentheses as ? operand or
dot receiver.

```lisp
(do0 (? (fetch row)) (dot (fetch row) (to_string)))
```

```rust
fetch(row)?;
fetch(row).to_string();
```

### `(* (+ 1 2) 3)`

A looser operand keeps its parentheses.

```lisp
(* (+ 1 2) 3)
```

```rust
(1 + 2) * 3
```

### `(- (- a b) c)`

Left-nested subtraction stays flat (left associative).

```lisp
(- (- a b) c)
```

```rust
a - b - c
```

### `(- a (- b c))`

Right-nested subtraction is parenthesised: a-(b-c) is
not (a-b)-c.

```lisp
(- a (- b c))
```

```rust
a - (b - c)
```

### `(* a (/ b c))`

Same-level mixed * and / never stay flat on the right:
a*(b/c) is not (a*b)/c for integers.

```lisp
(* a (/ b c))
```

```rust
a * (b / c)
```

### `(== a (== b c))`

Comparisons cannot be chained in Rust (a==b==c is a
compile error), so a nested comparison is always parenthesised.

```lisp
(== a (== b c))
```

```rust
a == (b == c)
```

### `(== (logand x mask) 0)`

Unlike C, Rust's bitwise & binds tighter than ==, so no
parentheses are needed here.

```lisp
(== (logand x mask) 0)
```

```rust
x & mask == 0
```

### `(or (and a b) c)`

&& binds tighter than ||, so this stays flat.

```lisp
(or (and a b) c)
```

```rust
a && b || c
```

### `(- (+ a b))`

Unary minus keeps parentheses around a looser operand.

```lisp
(- (+ a b))
```

```rust
-(a + b)
```

### `(- (- x))`

Minus inside minus would glue into the invalid --x.

```lisp
(- (- x))
```

```rust
-(-x)
```

### `(not (== a b))`

The == operand binds looser than unary !, so it keeps
its parentheses.

```lisp
(not (== a b))
```

```rust
!(a == b)
```

### `(dot (% a b) (to_string))`

Method call binds tightest, so a binary receiver keeps
its parentheses even with elision.

```lisp
(dot (% a b) (to_string))
```

```rust
(a % b).to_string()
```

### `(dot (deref p) x)`

*(p).x parses as *((p).x), so the deref keeps its
parentheses as a dot receiver.

```lisp
(dot (deref p) x)
```

```rust
(*p).x
```

### `(coerce (+ a b) i64)`

`as' binds tighter than +, so the sum keeps its
parentheses.

```lisp
(coerce (+ a b) i64)
```

```rust
(a + b) as i64
```

### `(ref (+ a b))`

& binds tighter than +, but the operand is looser, so
&(a+b) keeps its parentheses.

```lisp
(ref (+ a b))
```

```rust
&(a + b)
```

### `(<< (+ 1 1) 3)`

+ binds tighter than <<, so the sum needs no parentheses.

```lisp
(<< (+ 1 1) 3)
```

```rust
1 + 1 << 3
```

### `(>> 64 (>> 8 1))`

Shifts are left associative but not flattenable, so a
right-nested shift keeps its parentheses: 64>>(8>>1) is not (64>>8)>>1.

```lisp
(>> 64 (>> 8 1))
```

```rust
64 >> (8 >> 1)
```

### `(/ x)`

The single-argument / reciprocal keeps a tight operand
bare.

```lisp
(/ x)
```

```rust
1.0 / x
```

### `(? (dot f (read_to_string "&mut s")))`

A call needs no parentheses under ?.

```lisp
(? (dot f (read_to_string "&mut s")))
```

```rust
f.read_to_string(&mut s)?
```

### `(logior (logior a b) c)`

Left-nested bitwise | stays flat.

```lisp
(logior (logior a b) c)
```

```rust
a | b | c
```

### `(logxor a (logxor b c))`

logxor is associative for integers, so a right-nested
chain stays flat like + and * do.

```lisp
(logxor a (logxor b c))
```

```rust
a ^ b ^ c
```

### `(logxor (logand a b) c)`

& binds tighter than ^, so no parentheses are needed.

```lisp
(logxor (logand a b) c)
```

```rust
a & b ^ c
```

### `(<< (<< a 1) 2)`

Left-nested shifts stay flat (left associative).

```lisp
(<< (<< a 1) 2)
```

```rust
a << 1 << 2
```

### `(>> (>> a 1) 2)`

Same for >>.

```lisp
(>> (>> a 1) 2)
```

```rust
a >> 1 >> 2
```

### `(or a (or b c))`

|| is associative, so a right-nested chain stays flat.

```lisp
(or a (or b c))
```

```rust
a || b || c
```

### `(and (or a b) c)`

|| binds looser than &&, so it keeps its parentheses
as an && operand.

```lisp
(and (or a b) c)
```

```rust
(a || b) && c
```

### `(+ (<< a 1) b)`

<< binds looser than +, so the shift keeps its
parentheses as a + operand.

```lisp
(+ (<< a 1) b)
```

```rust
(a << 1) + b
```

### `(+ 1 (+ 2 3))`

+ is associative, so a right-nested sum stays flat
(up to floating point rounding).

```lisp
(+ 1 (+ 2 3))
```

```rust
1 + 2 + 3
```

### `(logand a b)`

(logand a b) emits Rust's bitwise &.

```lisp
(logand a b)
```

```rust
((a) & (b))
```

### `(logior a b)`

(logior a b) emits Rust's bitwise |.

```lisp
(logior a b)
```

```rust
((a) | (b))
```

### `(logxor a b)`

(logxor a b) emits Rust's bitwise ^.

```lisp
(logxor a b)
```

```rust
((a) ^ (b))
```

### `(<< a 3)`

(<< a b) shifts left.

```lisp
(<< a 3)
```

```rust
((a)<<(3))
```

### `(>> a 3)`

(>> a b) shifts right.

```lisp
(>> a 3)
```

```rust
((a)>>(3))
```

### `(== a 5)`

(== a b) compares for equality.  Note that Common Lisp's =
means assignment here and /= means division-assignment.

```lisp
(== a 5)
```

```rust
((a)==(5))
```

### `(!= a 5)`

(!= a b) compares for inequality.

```lisp
(!= a 5)
```

```rust
((a)!=(5))
```

### `(< 3 5)`

(< a b) compares.

```lisp
(< 3 5)
```

```rust
((3)<(5))
```

### `(> 3 5)`

(> a b) compares.

```lisp
(> 3 5)
```

```rust
((3)>(5))
```

### `(<= 3 5)`

(<= a b) compares.

```lisp
(<= 3 5)
```

```rust
((3)<=(5))
```

### `(>= 3 5)`

(>= a b) compares.

```lisp
(>= 3 5)
```

```rust
((3)>=(5))
```

### `(and a b)`

(and a b) emits Rust's short circuiting &&.

```lisp
(and a b)
```

```rust
((a)&&(b))
```

### `(or a b)`

(or a b) emits Rust's short circuiting ||.

```lisp
(or a b)
```

```rust
((a)||(b))
```

### `(not a)`

(not a) emits !a.  The parentheses enclose the whole
expression so that (dot (not a) b) does not turn into !((a).b).

```lisp
(not a)
```

```rust
(!a)
```

## reference Forms

### `(ref x)`

(ref x) takes a shared reference.

```lisp
(ref x)
```

```rust
(&x)
```

### `(ref-mut x)`

(ref-mut x) takes a mutable reference.

```lisp
(ref-mut x)
```

```rust
(&mut x)
```

### `(deref x)`

(deref x) dereferences.

```lisp
(deref x)
```

```rust
(*x)
```

### `(dot (deref p) x)`

Regression test: *(p).x parses as *((p).x) in Rust, so the
parentheses have to enclose the deref, not its operand.

```lisp
(dot (deref p) x)
```

```rust
(*p).x
```

### `(coerce x u8)`

(coerce value type) emits Rust's `as' cast.

```lisp
(coerce x u8)
```

```rust
(x as u8)
```

### `(? (dot f (read_to_string "&mut s")))`

(? expr) appends Rust's ? error propagation operator.

```lisp
(? (dot f (read_to_string "&mut s")))
```

```rust
f.read_to_string(&mut s)?
```

### `(await (fetch_summary identifier))`

(await expr) appends .await; outer parentheses are
stripped like in return and conditions.

```lisp
(await (fetch_summary identifier))
```

```rust
fetch_summary(identifier).await
```

### `(? (parse_url link))`

(? expr) emits Rust's ? error-propagation operator.

```lisp
(? (parse_url link))
```

```rust
parse_url(link)?
```

