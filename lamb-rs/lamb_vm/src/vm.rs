use std::{cmp::Ordering, ops};

use crate::{
    chunk::{Chunk, Op},
    gc::{GcRef, LambGc},
    value::{LambArray, LambClosure, LambString, Upvalue, Value},
};

pub struct Vm {
    gc: LambGc,
    frames: Vec<CallFrame>,
    stack: Vec<Value>,
    saved: Option<Value>,
    open_upvalues: Vec<GcRef<Upvalue>>,
}

impl Vm {
    pub fn new(gc: LambGc) -> Self {
        Self {
            gc,
            frames: Vec::with_capacity(64),
            stack: Vec::with_capacity(u8::MAX as usize * 64),
            saved: None,
            open_upvalues: Vec::with_capacity(64),
        }
    }

    pub fn exec(mut self, rf: GcRef<LambClosure>) {
        self.stack.push(Value::Closure(rf));
        self.frames.push(CallFrame::new(rf, 0));
        self.run();
    }

    fn run(&mut self) {
        loop {
            let op = self.chunk().code[self.frame().ip];
            match op {
                Op::Constant(i) => {
                    let value = self.chunk().constants[usize::from(i)];
                    self.push(value);
                }
                Op::GetLocal(i) => {
                    let idx = self.frame().slot + usize::from(i);
                    let value = self.stack[idx];
                    self.push(value);
                }
                Op::DefineGlobal(_) => todo!(),
                Op::GetGlobal(_) => todo!(),
                Op::GetUpvalue(_) => todo!(),

                Op::Call(_) => todo!(),
                Op::Return => todo!(),

                Op::Closure(_) => todo!(),
                Op::CloseValue => todo!(),

                Op::Dup => {
                    let item = self.stack.last().copied().unwrap();
                    self.push(item);
                }
                Op::Pop => {
                    self.pop();
                }

                Op::Jump(off) => {
                    self.frame_mut().ip += usize::from(off);
                }
                Op::JumpIfFalse(off) => {
                    let Value::Bool(is_true) = self.pop() else {
                        panic!("type error!");
                    };

                    if !is_true {
                        self.frame_mut().ip += usize::from(off);
                    }
                }
                Op::JumpIfTrue(off) => {
                    let Value::Bool(is_true) = self.pop() else {
                        panic!("type error!");
                    };

                    if is_true {
                        self.frame_mut().ip += usize::from(off);
                    }
                }

                Op::SetSlot(i) => {
                    let idx = self.frame().slot + usize::from(i);
                    let value = self.stack[idx];
                    self.stack[idx] = self.stack.last().copied().unwrap();
                }
                Op::SaveValue => {
                    self.saved = Some(self.pop());
                }
                Op::UnsaveValue => {
                    let saved = self.saved.take().unwrap();
                    self.push(saved);
                }

                Op::Len => todo!(),
                Op::Index => todo!(),
                Op::IndexRev => todo!(),
                Op::Slice(_) => todo!(),
                Op::MakeArray(n) => {
                    let mut v = Vec::with_capacity(usize::from(n));
                    for _ in 0..n {
                        v.push(self.pop());
                    }

                    v.reverse();

                    let arr = LambArray::from(v);
                    let arr = self.gc.alloc(arr);
                    self.push(Value::Array(arr));
                }

                Op::NumNeg => self.num_un_op(ops::Neg::neg),
                Op::LogNeg => self.bool_un_op(ops::Not::not),

                Op::Add => self.add_op(),
                Op::Sub => self.num_bin_op(ops::Sub::sub),
                Op::BinAnd => self.num_bin_op(ops::BitAnd::bitand),
                Op::BinNeg => self.num_un_op(ops::Not::not),
                Op::BinOr => self.num_bin_op(ops::BitOr::bitor),
                Op::BinXor => self.num_bin_op(ops::BitXor::bitxor),
                Op::Div => self.num_bin_op(ops::Div::div),
                Op::Mod => self.num_bin_op(ops::Rem::rem),
                Op::Mul => self.num_bin_op(ops::Mul::mul),
                Op::LShift => self.num_bin_op(ops::Shl::shl),
                Op::RShift => self.num_bin_op(ops::Shr::shr),

                Op::Eq => self.value_cmp_op(|o| matches!(o, Ordering::Equal)),
                Op::Ge => self.value_cmp_op(|o| matches!(o, Ordering::Equal | Ordering::Greater)),
                Op::Gt => self.value_cmp_op(|o| matches!(o, Ordering::Greater)),
                Op::Le => self.value_cmp_op(|o| matches!(o, Ordering::Less | Ordering::Equal)),
                Op::Lt => self.value_cmp_op(|o| matches!(o, Ordering::Less)),
                Op::Ne => self.value_cmp_op(|o| matches!(o, Ordering::Less | Ordering::Greater)),
            }
        }
    }

    fn chunk(&self) -> &Chunk {
        let cls = self.frame().closure;
        let func = self.gc.deref(cls).func;
        &self.gc.deref(func).chunk
    }

    fn chunk_mut(&mut self) -> &mut Chunk {
        let cls = self.frame().closure;
        let func = self.gc.deref_mut(cls).func;
        &mut self.gc.deref_mut(func).chunk
    }

    fn frame(&self) -> &CallFrame {
        self.frames.last().unwrap()
    }

    fn frame_mut(&mut self) -> &mut CallFrame {
        self.frames.last_mut().unwrap()
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }

    fn push(&mut self, v: Value) {
        self.stack.push(v);
    }

    fn add_op(&mut self) {
        let rhs = self.pop();
        let lhs = self.pop();

        match (lhs, rhs) {
            (Value::Int(l), Value::Int(r)) => {
                self.push(Value::Int(l + r));
            }
            (Value::String(l), Value::String(r)) => {
                let l = &self.gc.deref(l).0;
                let r = &self.gc.deref(r).0;
                let s = LambString::new(format!("{l}{r}"));
                let new = self.gc.alloc(s);
                self.push(Value::String(new));
            }
            _ => panic!("type error!"),
        }
    }

    fn bool_un_op<F>(&mut self, f: F)
    where
        F: Fn(bool) -> bool,
    {
        let rhs = self.pop();

        let Value::Bool(r) = rhs else {
            panic!("type error!");
        };

        self.push(Value::Bool(f(r)));
    }

    fn num_un_op<F>(&mut self, f: F)
    where
        F: Fn(i64) -> i64,
    {
        let rhs = self.pop();

        let Value::Int(r) = rhs else {
            panic!("type error!");
        };

        self.push(Value::Int(f(r)));
    }

    fn num_bin_op<F>(&mut self, f: F)
    where
        F: Fn(i64, i64) -> i64,
    {
        let rhs = self.pop();
        let lhs = self.pop();

        let (Value::Int(l), Value::Int(r)) = (lhs, rhs) else {
            panic!("type error!");
        };

        self.push(Value::Int(f(l, r)));
    }

    fn value_cmp_op<F>(&mut self, f: F)
    where
        F: Fn(Ordering) -> bool,
    {
        let rhs = self.pop();
        let lhs = self.pop();

        let Some(ord) = lhs.compare(&rhs, &self.gc) else {
            panic!("type error!");
        };

        self.push(Value::Bool(f(ord)))
    }
}

struct CallFrame {
    closure: GcRef<LambClosure>,
    ip: usize,
    slot: usize,
}

impl CallFrame {
    fn new(closure: GcRef<LambClosure>, slot: usize) -> Self {
        CallFrame {
            closure,
            ip: 0,
            slot,
        }
    }
}
