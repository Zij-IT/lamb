use std::marker::PhantomData;

use crate::{
    chunk::Op,
    value::{FuncUpvalue, LambArray, LambClosure, LambFunc, LambString, Upvalue, Value},
};

pub struct LambGc {
    free_slots: Vec<usize>,
    objects: Vec<Option<GcItem>>,
}

impl LambGc {
    pub fn new() -> Self {
        Self {
            free_slots: Default::default(),
            objects: Default::default(),
        }
    }

    pub fn alloc<T: Allocable>(&mut self, obj: T) -> GcRef<T> {
        let obj = obj.into_raw();
        let size = obj.size() + std::mem::size_of::<GcItem>();
        let item = GcItem {
            is_marked: false,
            size,
            obj,
        };

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

        GcRef {
            idx,
            _type: PhantomData,
        }
    }

    pub fn deref<T: Allocable>(&self, gcref: GcRef<T>) -> &T {
        self.objects[gcref.idx].as_ref().unwrap().obj.as_inner()
    }

    pub fn deref_mut<T: Allocable>(&mut self, gcref: GcRef<T>) -> &mut T {
        self.objects[gcref.idx].as_mut().unwrap().obj.as_inner_mut()
    }
}

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

enum GcItemRaw {
    Array(LambArray),
    Closure(LambClosure),
    String(LambString),
    Upvalue(Upvalue),
    Func(LambFunc),
}

impl GcItemRaw {
    fn size(&self) -> usize {
        match self {
            GcItemRaw::Upvalue(u) => std::mem::size_of::<Upvalue>(),
            GcItemRaw::Array(a) => a.capacity() + std::mem::size_of::<LambArray>(),
            GcItemRaw::String(s) => s.capacity() + std::mem::size_of::<LambString>(),
            GcItemRaw::Closure(c) => {
                std::mem::size_of::<LambClosure>()
                    + c.upvalues.capacity() * std::mem::size_of::<Upvalue>()
            }
            GcItemRaw::Func(f) => {
                std::mem::size_of::<LambFunc>()
                    + f.upvalues.capacity() * std::mem::size_of::<FuncUpvalue>()
                    + f.chunk.code.capacity() * std::mem::size_of::<Op>()
                    + f.chunk.constants.capacity() * std::mem::size_of::<Value>()
            }
        }
    }

    fn as_inner<T: Allocable>(&self) -> &T {
        <T as Allocable>::from_raw(self)
    }

    fn as_inner_mut<T: Allocable>(&mut self) -> &mut T {
        <T as Allocable>::from_raw_mut(self)
    }
}

struct GcItem {
    obj: GcItemRaw,
    size: usize,
    is_marked: bool,
}

trait Allocable {
    fn into_raw(self) -> GcItemRaw;

    fn from_raw(item: &GcItemRaw) -> &Self;

    fn from_raw_mut(item: &mut GcItemRaw) -> &mut Self;
}

impl Allocable for LambString {
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

impl Allocable for LambFunc {
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

impl Allocable for LambClosure {
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

impl Allocable for LambArray {
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

impl Allocable for Upvalue {
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
