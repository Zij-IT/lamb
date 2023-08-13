#[derive(Debug, Clone)]
pub enum Value {}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        todo!()
    }
}

#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq)]
pub enum Instr {
    Constant,
    DefineGlobal,
    GetGlobal,
    GetLocal,
    GetUpvalue,
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
    Jump,
    JumpIfTrue,
    JumpIfFalse,
    MakeArray,
    Len,
    Index,
    IndexRev,
    Pop,
    Dup,
    Call,
    Closure,
    CloseValue,
    SaveValue,
    UnsaveValue,
    SetSlot,
}

impl Instr {
    pub fn to_byte(self) -> u8 {
        self as u8
    }

    pub fn from_byte(byte: u8) -> Option<Self> {
        Some(match byte {
            0 => Self::Constant,
            1 => Self::DefineGlobal,
            2 => Self::GetGlobal,
            3 => Self::GetLocal,
            4 => Self::GetUpvalue,
            5 => Self::NumNeg,
            6 => Self::BinNeg,
            7 => Self::LogNeg,
            8 => Self::Add,
            9 => Self::Sub,
            10 => Self::Mul,
            11 => Self::Mod,
            12 => Self::Div,
            13 => Self::BinAnd,
            14 => Self::BinOr,
            15 => Self::BinXor,
            16 => Self::Eq,
            17 => Self::Ne,
            18 => Self::Gt,
            19 => Self::Ge,
            20 => Self::Lt,
            21 => Self::Le,
            22 => Self::RShift,
            23 => Self::LShift,
            24 => Self::Return,
            25 => Self::Jump,
            26 => Self::JumpIfTrue,
            27 => Self::JumpIfFalse,
            28 => Self::MakeArray,
            29 => Self::Len,
            30 => Self::Index,
            31 => Self::IndexRev,
            32 => Self::Pop,
            33 => Self::Dup,
            34 => Self::Call,
            35 => Self::Closure,
            36 => Self::CloseValue,
            37 => Self::SaveValue,
            38 => Self::UnsaveValue,
            39 => Self::SetSlot,
            _ => return None,
        })
    }

    fn is_jump(self) -> bool {
        matches!(self, Instr::Jump | Instr::JumpIfTrue | Instr::JumpIfFalse)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct Chunk {
    pub(crate) bytes: Vec<u8>,
    pub(crate) constants: Vec<Value>,
}

impl Chunk {
    const REQ_PATCH: u8 = 0xff;

    pub fn write(&mut self, byte: u8) {
        debug_assert!(!Instr::from_byte(byte).map_or(false, Instr::is_jump));

        self.bytes.push(byte);
    }

    pub fn write_jump(&mut self, jump_op: u8) -> usize {
        debug_assert!(Instr::from_byte(jump_op).map_or(false, Instr::is_jump));

        self.bytes.push(jump_op);
        self.bytes.push(Self::REQ_PATCH);
        self.bytes.push(Self::REQ_PATCH);
        self.bytes.len() - 2
    }

    pub fn patch_jump(&mut self, marker: usize) {
        let jump = self.bytes.len() - marker - 2;
        let [hi, lo] = u16::try_from(jump)
            .expect("Jump shouldn't exceed 0xffff instructions.")
            .to_ne_bytes();

        self.bytes[marker] = hi;
        self.bytes[marker + 1] = lo;
    }

    pub fn write_const(&mut self, val: Value) {
        let idx = self.add_const(val);
        let [hi, lo] = u16::try_from(idx)
            .expect("Jump shouldn't exceed 0xffff instructions.")
            .to_ne_bytes();

        self.bytes.push(Instr::Constant.to_byte());
        self.bytes.push(hi);
        self.bytes.push(lo);
    }

    pub fn add_const(&mut self, val: Value) -> usize {
        self.constants.push(val);
        self.constants.len() - 1
    }
}
