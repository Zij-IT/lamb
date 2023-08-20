use std::collections::{HashMap, HashSet};

use boa_gc::Gc;
use chunk::{Instr, Value};
use object::{LambClosure, LambString, LambUpvalue};

mod chunk;
mod object;

#[derive(Debug, PartialEq, Clone)]
struct CallFrame {
    closure: Gc<LambClosure>,
    slots: usize,
    ip: usize,
}

#[derive(Debug, PartialEq, Default, Clone)]
pub struct Vm {
    strings: HashSet<LambString>,
    globals: HashMap<LambString, Value>,
    stack: Vec<Value>,
    frames: Vec<CallFrame>,
    open_upvalues: Vec<Gc<LambUpvalue>>,
    saved_value: Option<Value>,
}

impl Vm {
    const MAX_FRAMES: usize = 64;
    const STACK_SIZE: usize = Self::MAX_FRAMES * (std::u8::MAX as usize) + 1;
    const MAX_UPVALUES: usize = std::u8::MAX as usize;

    pub fn new() -> Self {
        Self {
            strings: Default::default(),
            globals: Default::default(),
            stack: Vec::with_capacity(Self::STACK_SIZE),
            frames: Vec::with_capacity(Self::MAX_FRAMES),
            open_upvalues: Vec::with_capacity(Self::MAX_UPVALUES),
            saved_value: None,
        }
    }

    pub fn step(&mut self) {
        let frame = self.frames.last_mut().unwrap();
        let instr = frame.closure.function.chunk.instrs[frame.ip];
        frame.ip += 1;

        match instr {
            Instr::Constant(_idx) => todo!(),
            Instr::DefineGlobal(_idx) => todo!(),
            Instr::GetGlobal(_idx) => todo!(),
            Instr::GetLocal(_idx) => todo!(),
            Instr::GetUpvalue(_idx) => todo!(),
            Instr::NumNeg => todo!(),
            Instr::BinNeg => todo!(),
            Instr::LogNeg => todo!(),
            Instr::Add => todo!(),
            Instr::Sub => todo!(),
            Instr::Mul => todo!(),
            Instr::Mod => todo!(),
            Instr::Div => todo!(),
            Instr::BinAnd => todo!(),
            Instr::BinOr => todo!(),
            Instr::BinXor => todo!(),
            Instr::Eq => todo!(),
            Instr::Ne => todo!(),
            Instr::Gt => todo!(),
            Instr::Ge => todo!(),
            Instr::Lt => todo!(),
            Instr::Le => todo!(),
            Instr::RShift => todo!(),
            Instr::LShift => todo!(),
            Instr::Return => todo!(),
            Instr::Jump(_to) => todo!(),
            Instr::JumpIfTrue(_to) => todo!(),
            Instr::JumpIfFalse(_to) => todo!(),
            Instr::MakeArray(_len) => todo!(),
            Instr::Len => todo!(),
            Instr::Index => todo!(),
            Instr::IndexRev => todo!(),
            Instr::Pop => todo!(),
            Instr::Dup => todo!(),
            Instr::Call(_argc) => todo!(),
            Instr::Closure(_idx) => todo!(),
            Instr::CloseValue => todo!(),
            Instr::SaveValue => todo!(),
            Instr::UnsaveValue => todo!(),
            Instr::SetSlot(_idx) => todo!(),
        }
    }

    fn read_constant(&mut self) -> Value {
        let frame = self.frames.last_mut().unwrap();
        frame.ip += 1;

        let Instr::Constant(idx) = frame.closure.function.chunk.instrs[frame.ip - 1] else {
            panic!("Read a non-value constant with Vm::read_constant");
        };
        frame.closure.function.chunk.constants[idx as usize].clone()
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().expect("stack should not be empty")
    }

    fn drop(&mut self) {
        self.stack.pop();
    }
}
