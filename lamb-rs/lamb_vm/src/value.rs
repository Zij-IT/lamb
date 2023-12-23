use std::{cmp::Ordering, ops::Range};

use crate::{
    chunk::Chunk,
    gc::{GcRef, LambGc},
    vm::Vm,
};

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Value {
    Nil,
    Int(i64),
    Bool(bool),
    Char(char),
    Double(f64),
    Array(GcRef<LambArray>),
    String(GcRef<LambString>),
    Closure(GcRef<LambClosure>),
    Native(NativeFunc),
}

impl Value {
    pub(crate) fn format(&self, gc: &LambGc) -> String {
        match self {
            Value::Nil => "nil".into(),
            Value::Int(i) => i.to_string(),
            Value::Bool(b) => b.to_string(),
            Value::Char(c) => c.to_string(),
            Value::Double(d) => d.to_string(),
            Value::Array(a) => {
                let arr = gc.deref(*a);
                let mut s = String::with_capacity(32);
                s.push_str("[");
                for i in arr {
                    s.push_str(i.format(gc).as_str());
                    s.push_str(", ");
                }

                // Pop extra ", "
                if s.len() > 0 {
                    s.pop();
                    s.pop();
                }

                s.push_str("]");
                s
            }
            Value::String(s) => gc.deref(*s).0.to_string(),
            Value::Closure(c) => {
                let f = gc.deref(*c).func;
                let s = gc.deref(f).name;
                gc.deref(s).0.to_string()
            }
            Value::Native(_) => "<native fn>".into(),
        }
    }

    pub fn compare(&self, other: &Self, gc: &LambGc) -> Option<Ordering> {
        match (self, other) {
            (Value::Nil, Value::Nil) => Some(Ordering::Equal),
            (Value::Int(l), Value::Int(r)) => l.partial_cmp(r),
            (Value::Bool(l), Value::Bool(r)) => l.partial_cmp(r),
            (Value::Char(l), Value::Char(r)) => l.partial_cmp(r),
            (Value::Double(l), Value::Double(r)) => l.partial_cmp(r),
            (Value::Array(l), Value::Array(r)) => {
                let l = gc.deref(*l);
                let r = gc.deref(*r);
                l.compare(r, gc)
            }
            (Value::String(l), Value::String(r)) => {
                let l = gc.deref(*l);
                let r = gc.deref(*r);
                Some(l.0.cmp(&r.0))
            }
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct LambString(pub String);

impl LambString {
    pub fn new<S: Into<String>>(s: S) -> Self {
        // CAUTION: If you change this so that the original `S` is handled by
        // the GC, then you have to make sure that any strings used are still
        // rooted.
        LambString(s.into())
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn capacity(&self) -> usize {
        self.0.capacity()
    }

    pub fn get(&self, idx: usize) -> char {
        self.0.chars().nth(idx).unwrap()
    }

    pub fn slice(&self, range: Range<usize>) -> Self {
        Self(self.0.get(range).unwrap().into())
    }
}

#[derive(Debug)]
pub struct LambArray {
    items: Vec<Value>,
}

impl LambArray {
    pub fn new() -> Self {
        Self { items: Vec::new() }
    }

    pub fn get(&self, idx: usize) -> Value {
        self.items.get(idx).copied().unwrap()
    }

    pub fn slice(&self, range: Range<usize>) -> Self {
        Self::from(self.items[range].to_vec())
    }

    pub fn len(&self) -> usize {
        self.items.len()
    }

    pub fn capacity(&self) -> usize {
        self.items.capacity()
    }

    pub fn compare(&self, other: &Self, gc: &LambGc) -> Option<Ordering> {
        match self.items.len().cmp(&other.items.len()) {
            Ordering::Less => return Some(Ordering::Less),
            Ordering::Greater => return Some(Ordering::Greater),
            Ordering::Equal => self
                .items
                .iter()
                .zip(other.items.iter())
                .find_map(|(s, o)| match s.compare(o, gc) {
                    Some(Ordering::Equal) => None,
                    Some(other) => Some(Some(other)),
                    None => Some(None),
                })
                .unwrap_or(Some(Ordering::Equal)),
        }
    }
}

impl From<Vec<Value>> for LambArray {
    fn from(value: Vec<Value>) -> Self {
        Self { items: value }
    }
}

impl IntoIterator for LambArray {
    type Item = Value;

    type IntoIter = <Vec<Value> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.items.into_iter()
    }
}

impl<'a> IntoIterator for &'a LambArray {
    type Item = &'a Value;

    type IntoIter = <&'a Vec<Value> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        (&self.items).into_iter()
    }
}

impl<'a> IntoIterator for &'a mut LambArray {
    type Item = &'a mut Value;

    type IntoIter = <&'a mut Vec<Value> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        (&mut self.items).into_iter()
    }
}

#[derive(Debug)]
pub struct LambFunc {
    pub arity: usize,
    pub chunk: Chunk,
    pub name: GcRef<LambString>,
    pub upvalues: Vec<FuncUpvalue>,
}

impl LambFunc {
    pub fn new(name: GcRef<LambString>) -> Self {
        Self {
            name,
            arity: 0,
            chunk: Chunk::new(),
            upvalues: Vec::new(),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct FuncUpvalue {
    pub index: usize,
    pub is_local: bool,
}

#[derive(Debug)]
pub struct LambClosure {
    pub func: GcRef<LambFunc>,
    pub upvalues: Vec<GcRef<Upvalue>>,
}

impl LambClosure {
    pub fn new(func: GcRef<LambFunc>) -> Self {
        Self {
            func,
            upvalues: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub struct Upvalue {
    pub index: usize,
    pub closed: Option<Value>,
}

impl Upvalue {
    pub fn new(index: usize) -> Self {
        Self {
            index,
            closed: None,
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct NativeFunc {
    raw: fn(&Vm, &[Value]) -> Value,
}

impl NativeFunc {
    pub fn new(f: fn(&Vm, &[Value]) -> Value) -> Self {
        Self { raw: f }
    }

    pub fn call(&self, vm: &Vm, args: &[Value]) -> Value {
        (self.raw)(vm, args)
    }
}
