use std::{cmp::Ordering, ops::Range};

use crate::{
    chunk::Chunk,
    gc::{GcRef, LambGc},
    vm::{self, Vm},
};

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Value {
    Nil,
    Int(i64),
    Bool(bool),
    Char(char),
    Double(f64),
    Array(GcRef<Array>),
    String(GcRef<Str>),
    Function(GcRef<Function>),
    Closure(GcRef<Closure>),
    Native(NativeFunction),
    ModulePath(GcRef<Str>),
}

impl Value {
    pub const NIL_TYPE_NAME: &'static str = "nil";
    pub const INT_TYPE_NAME: &'static str = "i64";
    pub const BOOL_TYPE_NAME: &'static str = "bool";
    pub const CHAR_TYPE_NAME: &'static str = "char";
    pub const DOUBLE_TYPE_NAME: &'static str = "double";
    pub const ARRAY_TYPE_NAME: &'static str = "array";
    pub const STR_TYPE_NAME: &'static str = "string";
    pub const FUNCTION_TYPE_NAME: &'static str = "function";
    pub const CLOSURE_TYPE_NAME: &'static str = "closure";
    pub const NATIVE_TYPE_NAME: &'static str = "native";
    pub const MODULE_TYPE_NAME: &'static str = "module";

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
                s.push('[');
                for i in arr {
                    s.push_str(i.format(gc).as_str());
                    s.push_str(", ");
                }

                // Pop extra ", "
                if s.len() > 1 {
                    s.pop();
                    s.pop();
                }

                s.push(']');
                s
            }
            Value::String(s) => gc.deref(*s).0.to_string(),
            Value::Function(f) => gc.deref(gc.deref(*f).name).0.to_string(),
            Value::Closure(c) => {
                let f = gc.deref(*c).func;
                let s = gc.deref(f).name;
                gc.deref(s).0.to_string()
            }
            Value::Native(_) => "<native fn>".into(),
            Value::ModulePath(s) => gc.deref(*s).0.to_string(),
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

    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Nil => Self::NIL_TYPE_NAME,
            Value::Int(_) => Self::INT_TYPE_NAME,
            Value::Bool(_) => Self::BOOL_TYPE_NAME,
            Value::Char(_) => Self::CHAR_TYPE_NAME,
            Value::Double(_) => Self::DOUBLE_TYPE_NAME,
            Value::Array(_) => Self::ARRAY_TYPE_NAME,
            Value::String(_) => Self::STR_TYPE_NAME,
            Value::Function(_) => Self::FUNCTION_TYPE_NAME,
            Value::Closure(_) => Self::CLOSURE_TYPE_NAME,
            Value::Native(_) => Self::NATIVE_TYPE_NAME,
            Value::ModulePath(_) => Self::MODULE_TYPE_NAME,
        }
    }
}

#[derive(Debug)]
pub struct Str(pub String);

impl Str {
    pub fn new<S: Into<String>>(s: S) -> Self {
        // CAUTION: If you change this so that the original `S` is handled by
        // the GC, then you have to make sure that any strings used are still
        // rooted.
        Str(s.into())
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn capacity(&self) -> usize {
        self.0.capacity()
    }

    pub fn get(&self, idx: usize) -> Option<char> {
        self.0.chars().nth(idx)
    }

    pub fn slice(&self, range: Range<usize>) -> Self {
        Self(self.0.get(range).unwrap().into())
    }
}

#[derive(Debug)]
pub struct Array {
    items: Vec<Value>,
}

impl Default for Array {
    fn default() -> Self {
        Self::new()
    }
}

impl Array {
    pub fn new() -> Self {
        Self { items: Vec::new() }
    }

    pub fn get(&self, idx: usize) -> Option<Value> {
        self.items.get(idx).copied()
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
            Ordering::Less => Some(Ordering::Less),
            Ordering::Greater => Some(Ordering::Greater),
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

impl From<Vec<Value>> for Array {
    fn from(value: Vec<Value>) -> Self {
        Self { items: value }
    }
}

impl FromIterator<Value> for Array {
    fn from_iter<T: IntoIterator<Item = Value>>(iter: T) -> Self {
        Self {
            items: iter.into_iter().collect(),
        }
    }
}

impl IntoIterator for Array {
    type Item = Value;

    type IntoIter = <Vec<Value> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.items.into_iter()
    }
}

impl<'a> IntoIterator for &'a Array {
    type Item = &'a Value;

    type IntoIter = <&'a Vec<Value> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.items.iter()
    }
}

impl<'a> IntoIterator for &'a mut Array {
    type Item = &'a mut Value;

    type IntoIter = <&'a mut Vec<Value> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.items.iter_mut()
    }
}

#[derive(Debug)]
pub struct Function {
    pub arity: usize,
    pub chunk: Chunk,
    pub name: GcRef<Str>,
    pub module: GcRef<Str>,
    pub upvalues: Vec<UnresolvedUpvalue>,
}

impl Function {
    pub fn new(name: GcRef<Str>, module: GcRef<Str>) -> Self {
        Self {
            name,
            module,
            arity: 0,
            chunk: Chunk::new(),
            upvalues: Vec::new(),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct UnresolvedUpvalue {
    pub index: usize,
    pub is_local: bool,
}

#[derive(Debug)]
pub struct Closure {
    pub func: GcRef<Function>,
    pub upvalues: Vec<GcRef<ResolvedUpvalue>>,
}

impl Closure {
    pub fn new(func: GcRef<Function>) -> Self {
        Self {
            func,
            upvalues: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub struct ResolvedUpvalue {
    pub index: usize,
    pub closed: Option<Value>,
}

impl ResolvedUpvalue {
    pub fn new(index: usize) -> Self {
        Self {
            index,
            closed: None,
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct NativeFunction {
    raw: vm::RawNative,
}

impl NativeFunction {
    pub fn new(f: vm::RawNative) -> Self {
        Self { raw: f }
    }

    pub fn call(&self, vm: &Vm, args: &[Value]) -> Result<Value, crate::vm::Error> {
        (self.raw)(vm, args)
    }
}
