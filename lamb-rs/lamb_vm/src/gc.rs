use std::{collections::HashMap, marker::PhantomData};

use crate::{
    chunk::Op,
    value::{FuncUpvalue, LambArray, LambClosure, LambFunc, LambString, Upvalue, Value},
};

const KB: usize = 1024;
const GROWTH_FACTOR: usize = 2;

pub struct LambGc {
    bytes_alloced: usize,
    bytes_till_gc: usize,
    free_slots: Vec<usize>,
    grey_stack: Vec<usize>,
    objects: Vec<Option<GcItem>>,
    strings: HashMap<String, GcRef<LambString>>,
}

impl Default for LambGc {
    fn default() -> Self {
        Self::new()
    }
}

impl LambGc {
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

    pub fn alloc<T: Allocable>(&mut self, obj: T) -> GcRef<T> {
        let obj = obj.into_raw();
        let size = obj.size() + std::mem::size_of::<GcItem>();
        let item = GcItem {
            is_marked: false,
            size,
            obj,
        };

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

        GcRef {
            idx,
            _type: PhantomData,
        }
    }

    pub fn intern(&mut self, s: impl Into<String>) -> GcRef<LambString> {
        let s = s.into();
        if let Some(s) = self.strings.get(&s) {
            *s
        } else {
            let str = LambString::new(s.clone());
            let str_ref = self.alloc(str);
            self.strings.insert(s, str_ref);
            str_ref
        }
    }

    pub fn deref<T: Allocable>(&self, gcref: GcRef<T>) -> &T {
        self.objects[gcref.idx].as_ref().unwrap().obj.as_inner()
    }

    pub fn deref_mut<T: Allocable>(&mut self, gcref: GcRef<T>) -> &mut T {
        self.objects[gcref.idx].as_mut().unwrap().obj.as_inner_mut()
    }

    pub fn should_collect(&mut self) -> bool {
        self.bytes_alloced > self.bytes_till_gc
    }

    pub fn collect_garbage(&mut self) {
        self.trace_refs();
        self.remove_weak_refs();
        self.sweep();
        self.bytes_till_gc = self.bytes_alloced * GROWTH_FACTOR;
    }

    pub fn mark_value(&mut self, val: Value) {
        match val {
            Value::Nil => (),
            Value::Int(_) => (),
            Value::Bool(_) => (),
            Value::Char(_) => (),
            Value::Double(_) => (),
            Value::Native(_nat) => (),
            Value::Array(arr) => self.mark_object(arr),
            Value::String(str) => self.mark_object(str),
            Value::Closure(clo) => self.mark_object(clo),
        }
    }

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
                for constant in &func.chunk.constants {
                    self.mark_value(*constant);
                }
            }
        }
        self.objects[index] = Some(gcitem);
    }

    fn remove_weak_refs(&mut self) {
        let Self {
            strings, objects, ..
        } = self;

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

pub trait Allocable {
    fn into_raw(self) -> GcItemRaw;

    fn from_raw(item: &GcItemRaw) -> &Self;

    fn from_raw_mut(item: &mut GcItemRaw) -> &mut Self;
}

pub enum GcItemRaw {
    Array(LambArray),
    Closure(LambClosure),
    String(LambString),
    Upvalue(Upvalue),
    Func(LambFunc),
}

impl GcItemRaw {
    pub(super) fn size(&self) -> usize {
        match self {
            GcItemRaw::Upvalue(_u) => std::mem::size_of::<Upvalue>(),
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

    pub(super) fn as_inner<T: Allocable>(&self) -> &T {
        <T as Allocable>::from_raw(self)
    }

    pub(super) fn as_inner_mut<T: Allocable>(&mut self) -> &mut T {
        <T as Allocable>::from_raw_mut(self)
    }
}

pub struct GcItem {
    pub(super) obj: GcItemRaw,
    pub(super) size: usize,
    pub(super) is_marked: bool,
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
