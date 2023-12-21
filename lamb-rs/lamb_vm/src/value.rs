use crate::{
    chunk::Chunk,
    gc::{GcRef, LambGc},
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
        }
    }
}

#[derive(Debug)]
pub struct LambString(pub String);

impl LambString {
    pub fn new<S: Into<String>>(s: S) -> Self {
        LambString(s.into())
    }

    pub fn capacity(&self) -> usize {
        self.0.capacity()
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

    pub fn capacity(&self) -> usize {
        self.items.capacity()
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

#[derive(Debug)]
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
