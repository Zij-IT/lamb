mod format;

use lamb_ast::{BinaryOp, UnaryOp};

use crate::{gc::LambGc, value::Value};

use self::format::ChunkFormatter;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Op {
    Add,
    BinAnd,
    BinNeg,
    BinOr,
    BinXor,
    Call(u16),
    CloseValue,
    Closure(u16),
    Constant(u16),
    DefineGlobal(u16),
    Div,
    Dup,
    Eq,
    Ge,
    GetGlobal(u16),
    GetLocal(u16),
    GetUpvalue(u16),
    Gt,
    Index,
    IndexRev,
    Jump(u16),
    JumpIfFalse(u16),
    JumpIfTrue(u16),
    LShift,
    Le,
    Len,
    LogNeg,
    Lt,
    MakeArray(u16),
    Mod,
    Mul,
    Ne,
    NumNeg,
    Pop,
    RShift,
    Return,
    SaveValue,
    SetSlot(u16),
    Slice(u16),
    Sub,
    UnsaveValue,
}

impl From<UnaryOp> for Op {
    fn from(value: UnaryOp) -> Self {
        match value {
            UnaryOp::NumNeg => Op::NumNeg,
            UnaryOp::LogNot => Op::LogNeg,
            UnaryOp::BinNot => Op::BinNeg,
        }
    }
}

impl TryFrom<BinaryOp> for Op {
    type Error = BinaryOp;

    fn try_from(value: BinaryOp) -> Result<Op, BinaryOp> {
        Ok(match value {
            BinaryOp::Add => Op::Add,
            BinaryOp::Sub => Op::Sub,
            BinaryOp::Div => Op::Div,
            BinaryOp::Mul => Op::Mul,
            BinaryOp::Mod => Op::Mod,
            BinaryOp::Gt => Op::Gt,
            BinaryOp::Ge => Op::Ge,
            BinaryOp::Lt => Op::Lt,
            BinaryOp::Le => Op::Le,
            BinaryOp::Eq => Op::Eq,
            BinaryOp::Ne => Op::Ne,
            BinaryOp::BinOr => Op::BinOr,
            BinaryOp::BinAnd => Op::BinAnd,
            BinaryOp::BinXor => Op::BinXor,
            BinaryOp::RShift => Op::RShift,
            BinaryOp::LShift => Op::LShift,
            _ => return Err(value),
        })
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct JumpIdx(usize);

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Jump {
    Always,
    IfTrue,
    IfFalse,
}

#[derive(Debug)]
pub struct Chunk {
    pub code: Vec<Op>,
    pub constants: Vec<Value>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            constants: Vec::new(),
        }
    }

    pub fn display<'b, 'c>(&self, gc: &'b LambGc, name: &'c str) -> ChunkFormatter<'_, 'b, 'c> {
        ChunkFormatter::new(self, gc, name)
    }

    pub fn write_op(&mut self, op: Op) {
        self.code.push(op);
    }

    pub fn write_val(&mut self, val: Value) {
        self.constants.push(val);
    }

    pub fn write_jmp(&mut self, jmp: Jump) -> JumpIdx {
        let op = match jmp {
            Jump::Always => Op::Jump(0),
            Jump::IfTrue => Op::JumpIfTrue(0),
            Jump::IfFalse => Op::JumpIfFalse(0),
        };

        self.write_op(op);
        JumpIdx(self.code.len() - 1)
    }

    pub fn patch_jmp(&mut self, JumpIdx(idx): JumpIdx) {
        let patch = (self.code.len() - idx - 1).try_into().unwrap();

        let fix = match self.code.get(idx) {
            Some(Op::Jump(0)) => Op::Jump(patch),
            Some(Op::JumpIfTrue(0)) => Op::JumpIfTrue(patch),
            Some(Op::JumpIfFalse(0)) => Op::JumpIfFalse(patch),
            _ => panic!("Chunk attempting to patch invalid JumpIdx"),
        };

        self.code[idx] = fix;
    }
}

#[cfg(test)]
mod test {
    use super::{Chunk, Jump, Op};

    #[test]
    fn patches_properly() {
        let mut chunk = Chunk::new();
        let idx = chunk.write_jmp(Jump::Always);
        chunk.write_op(Op::Add);
        chunk.patch_jmp(idx);

        assert_eq!(chunk.code.get(0), Some(&Op::Jump(1)));
    }
}
