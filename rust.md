
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