use crate::value::Value;

#[derive(Debug, Eq, PartialEq)]
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
            Some(Op::JumpIfTrue(0)) => Op::Jump(patch),
            Some(Op::JumpIfFalse(0)) => Op::Jump(patch),
            _ => panic!("Chunk attempting to patch invalid JumpIdx"),
        };

        self.code[idx] = fix;
    }
}

#[cfg(test)]
mod tests {
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
