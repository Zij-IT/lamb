use std::collections::{HashMap, HashSet};

use boa_gc::Gc;
use chunk::{Instr, Value};
use object::{LambClosure, LambString, LambUpvalue};

mod chunk;
mod object;

struct CallFrame {
    closure: Gc<LambClosure>,
    slots: usize,
    ip: usize,
}

pub struct Vm {
    strings: HashSet<LambString>,
    globals: HashMap<LambString, Value>,
    stack: Vec<Value>,
    frames: Vec<CallFrame>,
    open_upvalues: Vec<Gc<LambUpvalue>>,
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
        }
    }

    pub fn step(&mut self) {
        let frame = self.frames.last_mut().unwrap();
        let byte = frame.closure.function.chunk.bytes[frame.ip];
        frame.ip += 1;

        match Instr::from_byte(byte).expect("Grabbing non-instruction") {
            Instr::Constant => todo!(),
            Instr::DefineGlobal => todo!(),
            Instr::GetGlobal => todo!(),
            Instr::GetLocal => todo!(),
            Instr::GetUpvalue => todo!(),
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
            Instr::Jump => todo!(),
            Instr::JumpIfTrue => todo!(),
            Instr::JumpIfFalse => todo!(),
            Instr::MakeArray => todo!(),
            Instr::Len => todo!(),
            Instr::Index => todo!(),
            Instr::IndexRev => todo!(),
            Instr::Pop => todo!(),
            Instr::Dup => todo!(),
            Instr::Call => todo!(),
            Instr::Closure => todo!(),
            Instr::CloseValue => todo!(),
            Instr::SaveValue => todo!(),
            Instr::UnsaveValue => todo!(),
            Instr::SetSlot => todo!(),
        }
    }

    fn read_byte(&mut self) -> u8 {
        let frame = self.frames.last_mut().unwrap();
        frame.ip += 1;
        frame.closure.function.chunk.bytes[frame.ip - 1]
    }

    fn read_short(&mut self) -> u16 {
        let frame = self.frames.last_mut().unwrap();

        frame.ip += 1;
        let hi = frame.closure.function.chunk.bytes[frame.ip - 1];

        frame.ip += 1;
        let lo = frame.closure.function.chunk.bytes[frame.ip - 1];

        u16::from_ne_bytes([hi, lo])
    }

    fn read_constant(&mut self) -> Value {
        let idx = self.read_short();
        let frame = self.frames.last_mut().unwrap();
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
