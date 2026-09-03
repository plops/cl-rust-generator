
array
[T;N]
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

