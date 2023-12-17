use crate::{chunk::Chunk, gc::GcRef};

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
}

#[derive(Debug)]
pub struct LambString {
    inner: String,
    hash: usize,
}

#[derive(Debug)]
pub struct LambArray {
    items: Vec<Value>,
}

impl LambArray {
    pub fn new() -> Self {
        Self { items: Vec::new() }
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

#[derive(Debug)]
pub struct FuncUpvalue {
    pub index: u8,
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
