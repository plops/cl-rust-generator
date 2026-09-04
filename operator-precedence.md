Operator Precedence and Associativity
Table 5-1 summarizes Rust expression syntax. We will discuss all of these kinds
of expressions in this chapter. Operators are grouped by precedence and
ordered from highest precedence to lowest. (Like most programming
languages, Rust has operator precedence to determine the order of operations
when an expression contains multiple adjacent operators. For example, in
limit < 2 * broom.size + 1, the . operator has the highest precedence,
so the field access happens first.)
Table 5-1. Expressions
Expression type
Example
Array literal
[1, 2, 3]
Repeat array literal
[0; 50]
Tuple
(6, "crullers")
Grouping
(2 + 2)
Block
{ f(); g() }
Control flow expressions
if ok { f() }
if ok { 1 } else { 0 }
if let Some(x) = f() { x } else { 0 }
match x { None => 0, _ => 1 }
for v in e { f(v); }
while ok { ok = f(); }
while let Some(x) = it.next() { f(x); }
loop { next_event(); }
break
continue
return 0
Macro invocation
println!("ok")
Path
std::f64::consts::PI
Struct literal
Point {x: 0, y: 0}
Tuple field access
pair.0
Struct field access
point.x
Method call
point.translate(50, 50)
Function call
stdin()
Index
arr[0]
`Err`/`None` early return
create_dir("tmp")?
Logical/bitwise NOT
!ok
Negation
-num
Dereference
*ptr
Borrow
&val
Type cast
x as u32
Multiplication
n * 2
Division
n / 2
Remainder (modulus)
n % 2
Addition
n + 1
Subtraction
n - 1
Left shift
n << 1
Right shift
n >> 1
Bitwise AND
n & 1
Bitwise exclusive OR
n ^ 1
Bitwise OR
n | 1
Less than
n < 1
Less than or equal
n <= 1
Greater than
n > 1
Greater than or equal
n >= 1
Equal
n == 1
Not equal
n != 1
Logical AND
x.ok && y.ok
Logical OR
x.ok || backup.ok
End-exclusive range
start..stop
End-inclusive range
start..=stop
Assignment
x = val
Compound assignment
x *= 1
x /= 1
x %= 1
x += 1
x -= 1
x <<= 1
x >>= 1
x &= 1
x ^= 1
x |= 1
Closure
|x, y| x + y
The arithmetic and bitwise operations and their associated compound
assignments can be overloaded with arbitrary behavior for user-defined types,
as discussed in [Link to Come].
All of the operators that can usefully be chained are left-associative. That is, a
chain of operations such as a - b - c is grouped as (a - b) - c, not a -
(b - c). The operators that can be chained in this way are all the ones you
might expect:
*   /   %   +   -   <<   >>   &   ^   |   &&   ||   as
The comparison operators, the assignment operators, and the range operators
.. and ..= can’t be chained at all.
