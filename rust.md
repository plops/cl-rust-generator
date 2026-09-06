
array
[T;N]
vec![1,2]
Vec<T>  can be resized Vec::new Vec::with_capacity
&[T]  &mut [T]

more pointer types
Box<T>  Box::new(value) on heap
std::rc::Rc<T>  reference count
std::sync::Arc<T>  atomic reference count

*const T  like C pointers (don't use unless interfacing to C)
*mut T

slices &[T] as function arguments


String always valid utf 8
"\\bla"
r"\bla"
r###"\bla"###

move semantic
unless .clone()


moving elements out of a vector, leaves the vector fully populated but maybe smaller
pop
swap_remove
std::mem::replace


zugriff auf ein Option<T> struktur element kann None in der struktur zuruecklassen:
let first_name = composer[0].name.take()

copy types (like i32) are the exception, they are not moved
tuple or fixed size array of copy types is itself a copy type

every type that needs to do something special when its value is dropped cant be copy

structs are not copy by default but can be made:

#[derive(Copy, Clone)]
struct Label { number: u32 }

but copyable structs are limited in what types they can contain

in C++ operations like assignment are less predictable
in rust a move is always a byte for byte shallow copy


you can leak memory if you create two Rc objects that point to each other
std::rc::Weak can be used to resolve some of such cycles


references

have no effect on their referents' lifetimes
actually must never outlive them


&T  multiple readers
&mut T single writer


. operator dereferences its left operand when needed


compare two reference addresses std::ptr::eq

Option<&T> is just as efficient as a c pointer but safer

slice and trait references are special (fat pointers)


how to exit a labelled block:

let trimmed = 'trim: {
    if string.chars().last() != Some('\n') {
        break 'trim None;
    }
    string.pop();
    if string.chars().last() != Some('\r') {
        break 'trim Some(Newline::Unix);
    }
    string.pop();
    Some(Newline::Windows)
};

Declaration with late initialization (no need for mut):

let name;
if user.has_nickname() {
    name = user.nickname();
} else {
    name = generate_unique_name();
    user.register(&name);
}


Shadowing:

for line in file.lines() {
    let line = line?;
    ...
}
The let declaration creates a new, second variable, of a different type. The
type of the first variable line is Result<String, io::Error>. The second
line is a String.


if let <pattern> = <expr> expression:

if let Some(cookie) = request.session_cookie {
    return restore_session(cookie);
}

shorthand for

match expr {
    pattern => { block1 }
    _ => { block2 }
}


let <pattern> = <expr> expression:

let Ok(config_file) = File::open(&config_path) else {
    panic!("Unable to open config file {}.",
config_path.display());
};

shorthand for

let name = match expr {
    pattern => { convergent block }
    _ => { divergent block }
}

four loop expressions

while condition {
    block
}
while let pattern = expr {
    block
}
loop {
    block
}
for pattern in iterable { }

their value is ()

operator precedence (tightest first; see operator-precedence.md Table 5-1
and `*rust-precedence*` in rs.lisp, which the elision mode
`*omit-redundant-parens*` consults):

unary (!, -, *, &) > `as` > `* / %` > `+ -` > `<< >>` > `&` > `^` > `|`
> comparisons (never chained: `a==b==c` does not parse) > `&&` > `||`

unlike C, bitwise `& ^ |` bind tighter than comparisons, so
`x & mask == 0` needs no parentheses. `*omit-redundant-parens*` drops
exactly the parentheses this table makes redundant; the value tests in
transpiler-tests.lisp compile and run in both modes and must agree.

loop (and labelled blocks) can create value with break (not necessary in match)

continue jumps to the next loop iteration


 0..20 is the same as std::ops::Range { start: 0, end: 20 }.


..      // RangeFull
a..     // RangeFrom { start: a }
..b     // RangeTo { end: b }
a..b    // Range { start: a, end: b }

..=b     // RangeToInclusive { end: b }
a..=b    // RangeInclusive::new(a, b)

a loop can be labelled

'search:
for room in apartment {
    for spot in room.hiding_spots() {
        if spot.contains(keys) {
            println!("Your keys are {spot} in the {room}.");
            break 'search;
        }
    }
}

rust rejects some safe programs:

fn wait_for_process(process: &mut Process) -> i32 {
    while true {
        if process.wait() {
            return process.exit_code();
        }
    }
}  // error: mismatched types: expected i32, found ()


fn exit(code: i32) -> !
The ! means that exit() never returns. It’s a divergent function.


Turbofish ::<T>

One quirk of Rust syntax is that in a function call or method call, the usual
syntax for generic types, Vec<T>, does not work:
return Vec<i32>::with_capacity(1000);  // error: something about

The problem is that in expressions, < is the less-than operator. The Rust
compiler helpfully suggests writing ::<T> instead of <T> in this case, and that
solves the problem:
return Vec::<i32>::with_capacity(1000);  // ok, using ::<


Rust uses ! instead of ~ for bitwise NOT


Bit shifting is always sign-extending on signed integer types and zero-
extending on unsigned integer types.

Bitwise operations have higher precedence than comparisons, unlike C,

 x & BIT != 0, means (x & BIT) != 0, as you probably intended


casting a u16 to type
char is banned because some u16 values, like 0xd800, correspond to
Unicode surrogate code points and therefore would not make valid
char values. There is a standard method, std::char::from_u32(),
which performs the run-time check and returns an Option<char>;

User-defined types can implement the Deref trait, too. When you need to
write your own smart pointer type


closure needs block if it has a return value

let is_even = |x: u64| -> bool { x % 2 == 0 }; 