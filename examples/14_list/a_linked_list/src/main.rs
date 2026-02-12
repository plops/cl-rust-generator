
use std::mem;

// 1. Define the List and Node structures
// We use a generic type T so the list can hold any data type.
pub struct List<T> {
    head: Link<T>,
}

// Type alias for readability. 
// Option represents nullability (None = end of list).
// Box represents ownership (heap allocation).
type Link<T> = Option<Box<Node<T>>>;

struct Node<T> {
    elem: T,
    next: Link<T>,
}

impl<T> List<T> {
    // create an empty list
    pub fn new() -> Self {
        List { head: None }
    }

    // Push: O(1)
    // Adds an element to the front of the list
    pub fn push(&mut self, elem: T) {
        let new_node = Box::new(Node {
            elem: elem,
            // We take ownership of the current head and put it into 'next'
            // mem::replace is often used here, but Option.take() is more idiomatic
            next: self.head.take(),
        });

        self.head = Some(new_node);
    }

    // Pop: O(1)
    // Removes the first element and returns it
    pub fn pop(&mut self) -> Option<T> {
        // map takes the value inside the Option (if it exists)
        // and allows us to manipulate it.
        self.head.take().map(|node| {
            self.head = node.next;
            node.elem
        })
    }

    // Peek: O(1)
    // Return a reference to the first element without removing it
    pub fn peek(&self) -> Option<&T> {
        self.head.as_ref().map(|node| &node.elem)
    }
}

// IMPORTANT: Custom Drop Implementation
// By default, Rust drops recursively. If the list is huge, 
// dropping it could blow the stack. We implement an iterative drop.
impl<T> Drop for List<T> {
    fn drop(&mut self) {
        let mut cur_link = self.head.take();
        // While current link is Some(...)
        while let Some(mut boxed_node) = cur_link {
            // Move the next link out of the current node
            cur_link = boxed_node.next.take();
            // boxed_node goes out of scope here and is dropped
        }
    }
}

// --- ITERATOR IMPLEMENTATION ---
// A professional implementation always includes iterators.

// 1. IntoIter (Consuming Iterator)
pub struct IntoIter<T>(List<T>);

impl<T> List<T> {
    pub fn into_iter(self) -> IntoIter<T> {
        IntoIter(self)
    }
}

impl<T> Iterator for IntoIter<T> {
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        // Access the inner list and pop
        self.0.pop()
    }
}

// 2. Iter (Immutable Reference Iterator)
pub struct Iter<'a, T> {
    next: Option<&'a Node<T>>,
}

impl<T> List<T> {
// We add <'_, T> to explicitly link the lifetimes
pub fn iter(&self) -> Iter<'_, T> {
    Iter { 
        next: self.head.as_deref() 
    }
}
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;
    fn next(&mut self) -> Option<Self::Item> {
        self.next.map(|node| {
            self.next = node.next.as_deref();
            &node.elem
        })
    }
}

// --- UNIT TESTS ---
#[cfg(test)]
mod test {
    use super::List;

    #[test]
    fn basics() {
        let mut list = List::new();

        // Check empty list behaves correctly
        assert_eq!(list.pop(), None);

        // Populate list
        list.push(1);
        list.push(2);
        list.push(3);

        // Check normal removal
        assert_eq!(list.pop(), Some(3));
        assert_eq!(list.pop(), Some(2));

        // Push some more to make sure nothing is corrupted
        list.push(4);
        list.push(5);

        // Check removal again
        assert_eq!(list.pop(), Some(5));
        assert_eq!(list.pop(), Some(4));

        // Check exhaustion
        assert_eq!(list.pop(), Some(1));
        assert_eq!(list.pop(), None);
    }

    #[test]
    fn peek() {
        let mut list = List::new();
        assert_eq!(list.peek(), None);
        list.push(10);
        assert_eq!(list.peek(), Some(&10));
    }
}


fn main() {
    let mut list = List::new();
    
    println!("--- Starting Linked List Operations ---");
    
    // Push data
    println!("Pushing 10, 20, 30");
    list.push(10);
    list.push(20);
    list.push(30);

    // Iterate (using the iterator we built)
    println!("Iterating over list:");
    for item in list.iter() {
        println!("  Node: {}", item);
    }

    // Pop data
    println!("Popping: {:?}", list.pop());
    println!("Popping: {:?}", list.pop());
    
    println!("--- End Operations ---");
}