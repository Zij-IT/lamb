use crate::{
    gc::{GcRef, LambGc},
    value::{LambClosure, Upvalue, Value},
};

pub struct Vm {
    gc: LambGc,
    frames: Vec<CallFrame>,
    stack: Vec<Value>,
    open_upvalues: Vec<GcRef<Upvalue>>,
}

impl Vm {
    pub fn new(gc: LambGc) -> Self {
        Self {
            gc,
            frames: Vec::with_capacity(64),
            stack: Vec::with_capacity(u8::MAX as usize * 64),
            open_upvalues: Vec::with_capacity(64),
        }
    }

    pub fn exec(mut self, rf: GcRef<LambClosure>) {
        self.stack.push(Value::Closure(rf));
        self.frames.push(CallFrame::new(rf, 0));
        self.run();
    }

    fn run(&mut self) {
        todo!()
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
