//! This module contains all objects that are available to a user of Lamb
//! including [`LambString`], [`LambFunction`], [`LambClosure`] and
//! [`LambArray`].

use crate::{
    chunk::{Chunk, Value},
    Vm,
};
use boa_gc::{custom_trace, empty_trace, Finalize, Gc, GcRefCell, Trace};

// #=========================================#
//         all Lamb objects available
// #=========================================#

/// A `LambString` represents a garbage-collected String value with a
/// precomputed hash value.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct LambString {
    st: Gc<String>,
    hash: usize,
}

impl LambString {
    /// Creates a new `LambString` with the value `st`.
    pub fn new<S: Into<String>>(st: S) -> Self {
        // TODO: Intern the string created via `into` to keep behaviour
        //       similar to that of the C++ implementation (and mostly
        //       for perf).
        let st = st.into();
        let hash = Self::hash_str(&st);

        Self {
            st: Gc::new(st),
            hash,
        }
    }

    /// Returns the length of this `LambString`, in bytes, not `char`s or
    /// graphemes. In other words, it might not be what a human considers
    /// the length of the string.
    ///
    /// # Example
    ///
    /// ```
    /// let a = LambString::from("foo");
    /// assert_eq!(a.len(), 3);
    ///
    /// let fancy_f = LambString::from("ƒoo");
    /// assert_eq!(fancy_f.len(), 4);
    /// ```
    pub fn len(&self) -> usize {
        self.st.len()
    }

    /// Returns the hash of this `LambString`, which may or may not be used
    /// for interning... we'll see...
    // TODO(Elijah) - Determine if this is necessary after introducing a
    //                string interner into the project (string_cache?).
    pub fn hash(&self) -> usize {
        self.hash
    }

    /// Returns a new `LambString` which is the result of appending the bytes
    /// of `other` to the end of `self`.
    ///
    /// # Example
    ///
    /// ```
    /// let a = LambString::from("tasty ");
    /// let b = LambString::from("potato");
    /// assert_eq!(a.concat(b).as_str(), "tasty potato");
    /// ```
    pub fn concat<S: AsRef<str>>(&self, other: S) -> Self {
        let st = Gc::new(format!("{}{}", self.st.as_str(), other.as_ref()));
        let hash = Self::hash_str(&st);

        Self { st, hash }
    }

    fn hash_str(st: &str) -> usize {
        let hash: usize = 2166136261;
        st.bytes().fold(hash, |hash, byte| {
            (hash ^ usize::from(byte)).wrapping_mul(16777619)
        })
    }
}

impl AsRef<str> for LambString {
    fn as_ref(&self) -> &str {
        self.st.as_str()
    }
}

/// A `LambArray` represents a contiguous hetergeneous container of various
/// [`Value`]s.
///
/// # Example
/// ```
/// let mut vec = LambArray::new();
/// vec.push(Value::Num(1));
/// vec.push(Value::Num(2));
///
/// assert_eq!(vec.len(), 2);
/// assert_eq!(vec[0], Value::Num(1));
///
/// assert_eq!(vec.pop(), Some(Value::Num(2)));
/// assert_eq!(vec.len(), 1);
///
/// vec[0] = Value::Nil;
/// assert_eq!(vec[0], Value::Nil);
///
/// vec.extend([Value::Num(1), Value::Num(2), Value::Num(3)]);
///
/// for x in &vec {
///     println!("{x}");
/// }
///
/// assert_eq!(vec, [Value::Nil, Value::Num(1), Value::Num(2), Value::Num(3)]);
/// ```
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct LambArray {
    inner: Vec<Value>,
}

impl LambArray {
    /// Constructs a new, empty `LambArray`.
    /// The array does not allocate until elements are pushed onto it.
    pub fn new() -> Self {
        Self { inner: vec![] }
    }
}

impl std::ops::Deref for LambArray {
    type Target = Vec<Value>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl std::ops::DerefMut for LambArray {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

/// A `LambFunc` is the internal representation of a compiled Lamb function.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct LambFunc {
    pub name: Gc<LambString>,
    pub chunk: Chunk,
    pub arity: usize,
    pub upvalue_count: usize,
}

impl LambFunc {
    /// Constructs a new, empty `LambFunc`.
    pub fn new(name: Gc<LambString>) -> Self {
        Self {
            name,
            chunk: Chunk::new(),
            arity: 0,
            upvalue_count: 0,
        }
    }
}

/// A `LambUpvalue` contains either a location on the stack, or the value that
/// it closed over.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum LambUpvalue {
    Stack { location: usize },
    Closed { value: Value, location: usize },
}

/// A `LambClosure` is the combination of a `LambFunc` and all of it's upvalues
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct LambClosure {
    pub function: Gc<LambFunc>,
    pub upvalues: Vec<Gc<GcRefCell<LambUpvalue>>>,
}

impl LambClosure {
    /// Returns the total number of upvalues that this closure contains.
    pub fn upvalue_count(&self) -> usize {
        self.upvalues.len()
    }
}

impl std::hash::Hash for LambClosure {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.function.hash(state);
        self.upvalues.len().hash(state);
    }
}

/// A `LambNative` represents a wrapped Rust function that accepts a mutable
/// reference to the vm, as well as a slice of arguments. All `LambNative`s
/// must return a [`Value`]
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct LambNative(pub fn(usize, &mut [Value]) -> Value);

// #=========================================#
//         GC Finalize and Trace impls
// #=========================================#
impl Finalize for LambString {}

unsafe impl Trace for LambString {
    custom_trace!(this, {
        mark(&this.st);
    });
}

impl Finalize for LambArray {}

unsafe impl Trace for LambArray {
    custom_trace!(this, {
        mark(&this.inner);
    });
}

impl Finalize for LambFunc {
    fn finalize(&self) {}
}

unsafe impl Trace for LambFunc {
    custom_trace!(this, {
        mark(&this.name);
    });
}

impl Finalize for LambUpvalue {}

unsafe impl Trace for LambUpvalue {
    empty_trace!();
}

impl Finalize for LambClosure {
    fn finalize(&self) {}
}

unsafe impl Trace for LambClosure {
    custom_trace!(this, {
        mark(&this.function);
        mark(&this.upvalues);
    });
}

impl Finalize for LambNative {}

unsafe impl Trace for LambNative {
    empty_trace!();
}
