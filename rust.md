
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