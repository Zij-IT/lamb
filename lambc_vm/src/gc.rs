use std::{collections::HashMap, marker::PhantomData};

use crate::{
    chunk::Op,
    value::{
        Array, Closure, Function, ResolvedUpvalue, Str, UnresolvedUpvalue,
        Value,
    },
};

const KB: usize = 1024;
const GROWTH_FACTOR: usize = 2;

/// A [mark-and-sweep](https://en.wikipedia.org/wiki/Tracing_garbage_collection)
/// [garbage collector](https://en.wikipedia.org/wiki/Garbage_collection_(computer_science))
/// for the lamb virtual machine.
pub struct LambGc {
    bytes_alloced: usize,
    bytes_till_gc: usize,
    free_slots: Vec<usize>,
    grey_stack: Vec<usize>,
    objects: Vec<Option<GcItem>>,
    strings: HashMap<String, GcRef<Str>>,
}

impl Default for LambGc {
    fn default() -> Self {
        Self::new()
    }
}

impl LambGc {
    /// Constructs a new `LambGc` with no allocations
    pub fn new() -> Self {
        Self {
            bytes_alloced: 0,
            bytes_till_gc: KB * KB,
            strings: Default::default(),
            free_slots: Default::default(),
            grey_stack: Default::default(),
            objects: Default::default(),
        }
    }

    /// Allocates a new `T` returning a `GcRef` to the item.
    pub fn alloc<T: Allocable>(&mut self, obj: T) -> GcRef<T> {
        let obj = obj.into_raw();
        let size = obj.size() + std::mem::size_of::<GcItem>();
        let item = GcItem { is_marked: false, size, obj };

        self.bytes_alloced += size;
        let idx = match self.free_slots.pop() {
            Some(i) => {
                self.objects[i] = Some(item);
                i
            }
            None => {
                self.objects.push(Some(item));
                self.objects.len() - 1
            }
        };

        GcRef { idx, _type: PhantomData }
    }

    /// Interns a string and returns a reference to the interned value
    pub fn intern(&mut self, s: impl Into<String>) -> GcRef<Str> {
        let s = s.into();
        if let Some(s) = self.strings.get(&s) {
            *s
        } else {
            let str = Str::new(s.clone());
            let str_ref = self.alloc(str);
            self.strings.insert(s, str_ref);
            str_ref
        }
    }

    /// Dereferences an `GcRef` returning a reference to the value.
    ///
    /// # Panics
    ///
    /// This function is guarunteed to panic if an attempt is made to dereference
    /// a value which was already collected by the gc.
    ///
    /// This function can also panic, but is not guarunteed to panic, if a reference
    /// from one `LambGc` is used in another `LambGc`. It will only not panic if the
    /// reference points to a value of the same type which is not already deallocated.
    pub fn deref<T: Allocable>(&self, gcref: GcRef<T>) -> &T {
        self.objects[gcref.idx].as_ref().unwrap().obj.as_inner()
    }

    /// Dereferences an `GcRef` returning an exclusive reference to the value.
    ///
    /// # Panics
    ///
    /// This function is guarunteed to panic if an attempt is made to dereference
    /// a value which was already collected by the gc.
    ///
    /// This function can also panic, but is not guarunteed to panic, if a reference
    /// from one `LambGc` is used in another `LambGc`. It will only not panic if the
    /// reference points to a value of the same type
    pub fn deref_mut<T: Allocable>(&mut self, gcref: GcRef<T>) -> &mut T {
        self.objects[gcref.idx].as_mut().unwrap().obj.as_inner_mut()
    }

    /// returns `true` if the garbage-collector has allocated enough bytes to warrant
    /// another collection. Otherwise false.
    ///
    /// If `true` is returned all items should be marked using [`mark_value`](Self::mark_value) or
    /// [`mark_object`](Self::mark_object) and then [`collect_garbage`](Self::collect_garbage) should
    /// be called to free allocations.
    pub fn should_collect(&mut self) -> bool {
        self.bytes_alloced > self.bytes_till_gc
    }

    /// Collects all unmarked allocations, allowing for their reuse.
    pub fn collect_garbage(&mut self) {
        self.trace_refs();
        self.remove_weak_refs();
        self.sweep();
        self.bytes_till_gc = self.bytes_alloced * GROWTH_FACTOR;
    }

    /// Marks a value so that it is not collected when [`collect_garbage`](Self::collect_garbage)
    /// is next called
    pub fn mark_value(&mut self, val: Value) {
        match val {
            Value::Nil => (),
            Value::Int(_) => (),
            Value::Bool(_) => (),
            Value::Char(_) => (),
            Value::Double(_) => (),
            Value::Native(_nat) => (),
            Value::Function(func) => self.mark_object(func),
            Value::Array(arr) => self.mark_object(arr),
            Value::String(str) => self.mark_object(str),
            Value::Closure(clo) => self.mark_object(clo),
            Value::ModulePath(str) => self.mark_object(str),
        }
    }

    /// Marks a reference so that it is not collected when [`collect_garbage`](Self::collect_garbage)
    /// is next called
    pub fn mark_object<T: Allocable>(&mut self, gcref: GcRef<T>) {
        if let Some(obj) = self.objects[gcref.idx].as_mut() {
            if obj.is_marked {
                return;
            }

            obj.is_marked = true;
            self.grey_stack.push(gcref.idx)
        }
    }

    fn trace_refs(&mut self) {
        while let Some(index) = self.grey_stack.pop() {
            self.trace_object(index);
        }
    }

    fn trace_object(&mut self, index: usize) {
        let gcitem = self.objects[index].take().unwrap();
        match &gcitem.obj {
            GcItemRaw::String(_s) => (),
            GcItemRaw::Array(arr) => {
                for value in arr {
                    self.mark_value(*value);
                }
            }
            GcItemRaw::Closure(cls) => {
                self.mark_object(cls.func);
                for upvalue in &cls.upvalues {
                    self.mark_object(*upvalue);
                }
            }
            GcItemRaw::Upvalue(up) => {
                if let Some(closed) = up.closed {
                    self.mark_value(closed);
                }
            }
            GcItemRaw::Func(func) => {
                self.mark_object(func.name);
                self.mark_object(func.module);
                for constant in &func.chunk.constants {
                    self.mark_value(*constant);
                }
            }
        }
        self.objects[index] = Some(gcitem);
    }

    fn remove_weak_refs(&mut self) {
        let Self { strings, objects, .. } = self;

        strings.retain(|_, str| objects[str.idx].as_ref().unwrap().is_marked)
    }

    fn sweep(&mut self) {
        for i in 0..self.objects.len() {
            if let Some(obj) = self.objects[i].as_mut() {
                if obj.is_marked {
                    obj.is_marked = false;
                } else {
                    self.free(i);
                }
            }
        }
    }

    fn free(&mut self, index: usize) {
        if let Some(old) = self.objects[index].take() {
            self.bytes_alloced -= old.size;
            self.free_slots.push(index);
        } else {
            panic!("Nice going bud. Double free!");
        }
    }
}

/// A reference to an item within a [`LambGc`] of type `T`.
pub struct GcRef<T> {
    idx: usize,
    _type: PhantomData<T>,
}

impl<T> Copy for GcRef<T> {}

impl<T> Clone for GcRef<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> std::fmt::Debug for GcRef<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("GcRef").field(&self.idx).finish()
    }
}

impl<T> PartialEq for GcRef<T> {
    fn eq(&self, other: &Self) -> bool {
        self.idx == other.idx
    }
}

impl<T> Eq for GcRef<T> {}

impl<T> std::hash::Hash for GcRef<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.idx.hash(state);
    }
}

/// An object which is able to allocated by a [`LambGc`].
pub trait Allocable {
    /// Converts the object into a [`GcItemRaw`]
    fn into_raw(self) -> GcItemRaw;

    /// Converts a [`GcItemRaw`] reference into a `Self` reference
    fn from_raw(item: &GcItemRaw) -> &Self;

    /// Converts an exclusive [`GcItemRaw`] reference into an exclusive `Self`
    /// reference
    fn from_raw_mut(item: &mut GcItemRaw) -> &mut Self;
}

/// All possible types which are allocable by a [`LambGc`].
pub enum GcItemRaw {
    Array(Array),
    Closure(Closure),
    String(Str),
    Upvalue(ResolvedUpvalue),
    Func(Function),
}

impl GcItemRaw {
    /// Returns an approximation of the size of the item in bytes
    pub(super) fn size(&self) -> usize {
        match self {
            GcItemRaw::Upvalue(_u) => std::mem::size_of::<ResolvedUpvalue>(),
            GcItemRaw::Array(a) => a.capacity() + std::mem::size_of::<Array>(),
            GcItemRaw::String(s) => s.capacity() + std::mem::size_of::<Str>(),
            GcItemRaw::Closure(c) => {
                std::mem::size_of::<Closure>()
                    + c.upvalues.capacity()
                        * std::mem::size_of::<ResolvedUpvalue>()
            }
            GcItemRaw::Func(f) => {
                std::mem::size_of::<Function>()
                    + f.upvalues.capacity()
                        * std::mem::size_of::<UnresolvedUpvalue>()
                    + f.chunk.code.capacity() * std::mem::size_of::<Op>()
                    + f.chunk.constants.capacity()
                        * std::mem::size_of::<Value>()
            }
        }
    }

    /// Turns the `&mut Self` into `&T`
    pub(super) fn as_inner<T: Allocable>(&self) -> &T {
        <T as Allocable>::from_raw(self)
    }

    /// Turns the `&mut Self` into `&mut T`
    pub(super) fn as_inner_mut<T: Allocable>(&mut self) -> &mut T {
        <T as Allocable>::from_raw_mut(self)
    }
}

pub struct GcItem {
    pub(super) obj: GcItemRaw,
    pub(super) size: usize,
    pub(super) is_marked: bool,
}

impl Allocable for Str {
    fn into_raw(self) -> GcItemRaw {
        GcItemRaw::String(self)
    }

    fn from_raw(item: &GcItemRaw) -> &Self {
        match item {
            GcItemRaw::String(s) => s,
            _ => panic!("Bad type!"),
        }
    }

    fn from_raw_mut(item: &mut GcItemRaw) -> &mut Self {
        match item {
            GcItemRaw::String(s) => s,
            _ => panic!("Bad type!"),
        }
    }
}

impl Allocable for Function {
    fn into_raw(self) -> GcItemRaw {
        GcItemRaw::Func(self)
    }

    fn from_raw(item: &GcItemRaw) -> &Self {
        match item {
            GcItemRaw::Func(s) => s,
            _ => panic!("Bad type!"),
        }
    }

    fn from_raw_mut(item: &mut GcItemRaw) -> &mut Self {
        match item {
            GcItemRaw::Func(s) => s,
            _ => panic!("Bad type!"),
        }
    }
}

impl Allocable for Closure {
    fn into_raw(self) -> GcItemRaw {
        GcItemRaw::Closure(self)
    }

    fn from_raw(item: &GcItemRaw) -> &Self {
        match item {
            GcItemRaw::Closure(s) => s,
            _ => panic!("Bad type!"),
        }
    }

    fn from_raw_mut(item: &mut GcItemRaw) -> &mut Self {
        match item {
            GcItemRaw::Closure(s) => s,
            _ => panic!("Bad type!"),
        }
    }
}

impl Allocable for Array {
    fn into_raw(self) -> GcItemRaw {
        GcItemRaw::Array(self)
    }

    fn from_raw(item: &GcItemRaw) -> &Self {
        match item {
            GcItemRaw::Array(s) => s,
            _ => panic!("Bad type!"),
        }
    }

    fn from_raw_mut(item: &mut GcItemRaw) -> &mut Self {
        match item {
            GcItemRaw::Array(s) => s,
            _ => panic!("Bad type!"),
        }
    }
}

impl Allocable for ResolvedUpvalue {
    fn into_raw(self) -> GcItemRaw {
        GcItemRaw::Upvalue(self)
    }

    fn from_raw(item: &GcItemRaw) -> &Self {
        match item {
            GcItemRaw::Upvalue(s) => s,
            _ => panic!("Bad type!"),
        }
    }

    fn from_raw_mut(item: &mut GcItemRaw) -> &mut Self {
        match item {
            GcItemRaw::Upvalue(s) => s,
            _ => panic!("Bad type!"),
        }
    }
}
