use boa_gc::{custom_trace, Finalize, Trace};
use ordered_float::OrderedFloat;

use crate::object::{LambArray, LambClosure, LambNative, LambString};

// #=========================================#
//                  Lamb Value
// #=========================================#

/// A Lamb `Value`
#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum Value {
    /// `nil` - A nil value, for when a value doesn't exist
    Nil,

    /// `int` - A 64 bit signed integer value
    I64(i64),

    /// `float` - A 64 bit non-IEEE conformant floating point number
    F64(OrderedFloat<f64>),

    /// `char` - A unicode scalar value. See Rust [`char`]
    Char(char),

    /// `bool` - A `true` / `false` value
    Bool(bool),

    /// `array` - A heterogeneous contiguous container of `Value`
    Array(LambArray),

    /// `closure` - A compiled Lamb function
    Closure(LambClosure),

    /// `string` - A UTF-8-encoded immutable String
    String(LambString),

    /// `native` - A native Rust function
    Native(LambNative),
}

impl Finalize for Value {}

unsafe impl Trace for Value {
    custom_trace!(this, {
        match this {
            Value::Nil
            | Value::I64(_)
            | Value::F64(_)
            | Value::Char(_)
            | Value::Bool(_)
            | Value::Native(_) => (),
            Value::Array(a) => mark(a),
            Value::Closure(c) => mark(c),
            Value::String(s) => mark(s),
        }
    });
}

// #=========================================#
//         Lamb "Bytecode" Instructions
// #=========================================#
#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub enum Instr {
    Constant(u16),
    DefineGlobal(u16),
    GetGlobal(u16),
    GetLocal(u16),
    GetUpvalue(u16),
    NumNeg,
    BinNeg,
    LogNeg,
    Add,
    Sub,
    Mul,
    Mod,
    Div,
    BinAnd,
    BinOr,
    BinXor,
    Eq,
    Ne,
    Gt,
    Ge,
    Lt,
    Le,
    RShift,
    LShift,
    Return,
    Jump(u16),
    JumpIfTrue(u16),
    JumpIfFalse(u16),
    MakeArray(u16),
    Len,
    Index,
    IndexRev,
    Pop,
    Dup,
    Call(u16),
    Closure(u16),
    CloseValue,
    SaveValue,
    UnsaveValue,
    SetSlot(u16),
}

impl Instr {
    fn is_jump(self) -> bool {
        matches!(
            self,
            Instr::Jump(_) | Instr::JumpIfTrue(_) | Instr::JumpIfFalse(_)
        )
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Chunk {
    pub(crate) instrs: Vec<Instr>,
    pub(crate) constants: Vec<Value>,
}

impl Chunk {
    const REQ_PATCH: u16 = 0xffff;

    pub fn new() -> Self {
        Self {
            instrs: vec![],
            constants: vec![],
        }
    }

    pub fn write(&mut self, op: Instr) {
        self.instrs.push(op);
    }

    pub fn write_jump(&mut self, jump_op: fn(u16) -> Instr) -> usize {
        let jump = match jump_op(Self::REQ_PATCH) {
            a @ (Instr::Jump(_) | Instr::JumpIfTrue(_) | Instr::JumpIfFalse(_)) => a,
            _ => panic!("Non-jump Instr passed to `Chunk::write_jump`"),
        };

        self.instrs.push(jump);
        self.instrs.len() - 1
    }

    pub fn patch_jump(&mut self, marker: usize) {
        let jump = self.instrs.len() - marker - 1;
        let dist = u16::try_from(jump).expect("Jump shouldn't exceed 0xffff instructions.");

        match self.instrs.get_mut(marker) {
            Some(Instr::Jump(a) | Instr::JumpIfTrue(a) | Instr::JumpIfFalse(a)) => {
                *a = dist;
            }
            Some(_) => panic!("Jump marker points to non-jump instruction"),
            None => panic!("Jump marker points outside of possible instructions"),
        }
    }

    pub fn write_const(&mut self, val: Value) {
        let idx = self.add_const(val);
        let idx = u16::try_from(idx).expect("Jump shouldn't exceed 0xffff instructions.");

        self.instrs.push(Instr::Constant(idx));
    }

    pub fn add_const(&mut self, val: Value) -> usize {
        self.constants.push(val);
        self.constants.len() - 1
    }
}
