use std::fmt::Formatter;

use crate::{
    chunk::{Chunk, Op},
    value::Value,
};

pub struct ChunkFormatter<'a, 'b> {
    chunk: &'a Chunk,
    name: &'b str,
}

impl<'a, 'b> ChunkFormatter<'a, 'b> {
    pub fn new(chunk: &'a Chunk, name: &'b str) -> Self {
        Self { chunk, name }
    }

    fn format(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Chunk: {}", self.name)?;
        writeln!(f, "=== START ===")?;
        for (i, op) in self.chunk.code.iter().enumerate() {
            self.format_op(*op, i, f)?;
        }
        writeln!(f, "===  END  ===")
    }

    fn format_op(&self, op: Op, offset: usize, f: &mut Formatter<'_>) -> std::fmt::Result {
        print!("{offset:04} ");
        match op {
            Op::Add => writeln!(f, "Add"),
            Op::Sub => writeln!(f, "Sub"),
            Op::BinAnd => writeln!(f, "BinAnd"),
            Op::BinNeg => writeln!(f, "BinNeg"),
            Op::BinOr => writeln!(f, "BinOr"),
            Op::BinXor => writeln!(f, "BinXor"),
            Op::CloseValue => writeln!(f, "CloseValue"),
            Op::Div => writeln!(f, "Div"),
            Op::Dup => writeln!(f, "Dup"),
            Op::Eq => writeln!(f, "Eq"),
            Op::Ge => writeln!(f, "Ge"),
            Op::Gt => writeln!(f, "Gt"),
            Op::Index => writeln!(f, "Index"),
            Op::IndexRev => writeln!(f, "IndexRev"),
            Op::LShift => writeln!(f, "LShift"),
            Op::Le => writeln!(f, "Le"),
            Op::Len => writeln!(f, "Len"),
            Op::LogNeg => writeln!(f, "LogNeg"),
            Op::Lt => writeln!(f, "Lt"),
            Op::Mod => writeln!(f, "Mod"),
            Op::Mul => writeln!(f, "Mul"),
            Op::Ne => writeln!(f, "Ne"),
            Op::NumNeg => writeln!(f, "NumNeg"),
            Op::Pop => writeln!(f, "Pop"),
            Op::RShift => writeln!(f, "RShift"),
            Op::Return => writeln!(f, "Return"),
            Op::SaveValue => writeln!(f, "SaveValue"),
            Op::UnsaveValue => writeln!(f, "UnsaveValue"),
            Op::Jump(j) => self.jump_op("Jump", offset, j, f),
            Op::JumpIfFalse(j) => self.jump_op("JumpIfFalse", offset, j, f),
            Op::JumpIfTrue(j) => self.jump_op("JumpIfTrue", offset, j, f),
            Op::Closure(c) => self.const_op("Closure", c, f),
            Op::Constant(c) => self.const_op("Constant", c, f),
            Op::DefineGlobal(c) => self.const_op("DefineGlobal", c, f),
            Op::GetGlobal(c) => self.const_op("GetGlobal", c, f),
            Op::SetSlot(s) => self.slot_op("SetSlot", s, f),
            Op::GetLocal(s) => self.slot_op("GetLocal", s, f),
            Op::GetUpvalue(s) => self.slot_op("GetUpvalue", s, f),
            Op::Call(c) => self.counted_args("Call", "args", c, f),
            Op::MakeArray(c) => self.counted_args("MakeArray", "elems", c, f),
            Op::Slice(c) => {
                let Value::Int(i) = self.chunk.constants[c as usize] else {
                    panic!("")
                };

                let head_len = usize::try_from(i >> (i64::BITS / 2)).unwrap();
                let tail_len = usize::try_from(i & (i32::MAX as i64)).unwrap();

                writeln!(
                    f,
                    "Slice {{ tail_len: {tail_len:4}, head_len: {head_len:4} }}"
                )
            }
        }
    }

    fn counted_args(
        &self,
        name: &str,
        arg_name: &str,
        count: u16,
        f: &mut Formatter<'_>,
    ) -> std::fmt::Result {
        writeln!(f, "{name:<16} {arg_name:<8} {count:4}")
    }

    fn const_op(&self, name: &str, idx: u16, f: &mut Formatter<'_>) -> std::fmt::Result {
        let value = self.chunk.constants[idx as usize].format();
        writeln!(f, "{name:<16} {idx:4} ({value})")
    }

    fn slot_op(&self, name: &str, slot: u16, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{name:<16} {slot:4}")
    }

    fn jump_op(
        &self,
        name: &str,
        offset: usize,
        to: u16,
        f: &mut Formatter<'_>,
    ) -> std::fmt::Result {
        // The `+ 1` is because the ip will be incremented after reading a jump off
        // meaning that after a jump, the ip is ip + offset + 1
        writeln!(f, "{name:<16} {:4}", offset + usize::from(to) + 1)
    }
}

impl<'a, 'b> std::fmt::Display for ChunkFormatter<'a, 'b> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.format(f)
    }
}
