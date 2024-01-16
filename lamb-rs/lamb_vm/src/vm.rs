use std::{cmp::Ordering, collections::HashMap, convert::identity, ops};

use lamb_ast::Script;

use crate::{
    chunk::{Chunk, Op},
    compiler::Compiler,
    gc::{Allocable, GcRef, LambGc},
    value::{FuncUpvalue, LambArray, LambClosure, LambString, NativeFunc, Upvalue, Value},
};

macro_rules! num_bin_op {
    (%, $this:expr) => {{
        let rhs = $this.pop();
        let lhs = $this.pop();

        let val = match (lhs, rhs) {
            (Value::Int(l), Value::Int(r)) => Value::Int(l % r),
            _ => panic!("type error"),
        };

        $this.push(val);
    }};
    ($op:tt, $this:expr) => {{
        let rhs = $this.pop();
        let lhs = $this.pop();

        let val = match (lhs, rhs) {
            (Value::Int(l), Value::Int(r)) => Value::Int(l $op r),
            (Value::Double(l), Value::Double(r)) => Value::Double(l $op r),
            _ => panic!("type error"),
        };

        $this.push(val);
    }};
}

macro_rules! num_un_op {
    ($op:tt, $this:expr) => {{
        let rhs = $this.pop();

        let val = match rhs {
            Value::Int(r) => Value::Int($op r),
            Value::Double(r) => Value::Double($op r),
            _ => panic!("type error"),
        };

        $this.push(val);
    }};
}

pub struct Vm {
    gc: LambGc,
    globals: HashMap<GcRef<LambString>, Value>,
    frames: Vec<CallFrame>,
    stack: Vec<Value>,
    saved: Option<Value>,
    open_upvalues: Vec<GcRef<Upvalue>>,
}

impl Vm {
    pub fn new() -> Self {
        let mut this = Self {
            gc: LambGc::new(),
            globals: Default::default(),
            frames: Vec::with_capacity(64),
            stack: Vec::with_capacity(u8::MAX as usize * 64),
            saved: None,
            open_upvalues: Vec::with_capacity(64),
        };

        this.define_native("print", Self::native_print);
        this.define_native("println", Self::native_println);
        this
    }

    pub fn load_script(&mut self, script: &Script) {
        let name = self.gc.intern("__LAMB__SCRIPT__");
        let mut compiler = Compiler::new(name);
        compiler.compile(&mut self.gc, script);

        let closure = compiler.finish(&mut self.gc);
        self.stack.push(Value::Closure(closure));
        self.frames.push(CallFrame::new(closure, 0));
    }

    pub fn run(&mut self) {
        loop {
            let op = self.chunk().code[self.frame().ip];
            self.frame_mut().ip += 1;

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
                Op::DefineGlobal(i) => {
                    let Value::String(name) = self.chunk().constants[usize::from(i)] else {
                        panic!("type error");
                    };

                    let value = self.pop();
                    self.globals.insert(name, value);
                }
                Op::GetGlobal(i) => {
                    let Value::String(name) = self.chunk().constants[usize::from(i)] else {
                        panic!("type error");
                    };

                    let global = self.globals.get(&name).copied().unwrap();
                    self.push(global);
                }
                Op::GetUpvalue(i) => {
                    let clo_ref = self.frame().closure;
                    let closure = self.gc.deref(clo_ref);
                    let up = closure.upvalues[usize::from(i)];
                    let up = self.gc.deref(up);
                    let val = if let Some(value) = up.closed {
                        value
                    } else {
                        self.stack[up.index]
                    };

                    self.push(val);
                }
                Op::Call(args) => {
                    let args = usize::from(args);
                    let callee = self.peek(args);
                    match callee {
                        Value::Closure(cl) => {
                            let closure = self.gc.deref(cl);
                            let func = self.gc.deref(closure.func);
                            if args != func.arity {
                                panic!("too many args!");
                            } else {
                                let frame = CallFrame::new(cl, self.stack.len() - 1 - args);
                                self.frames.push(frame);
                            }
                        }
                        Value::Native(native) => {
                            let args = self.stack.len() - args;
                            let result = native.call(self, &self.stack[args..]);
                            self.stack.truncate(args - 1);
                            self.push(result);
                        }
                        _ => panic!("type error!"),
                    }
                }
                Op::Return => {
                    let frame = self.frames.pop().unwrap();
                    let ret_value = self.pop();
                    self.close_upvalues(frame.slot);

                    if self.frames.is_empty() {
                        return;
                    } else {
                        self.stack.truncate(frame.slot);
                        self.push(ret_value);
                    }
                }

                Op::CloseValue => {
                    let top = self.stack.len() - 1;
                    self.close_upvalues(top);
                    self.pop();
                }
                Op::Closure(i) => {
                    let Value::Closure(clo_ref) = self.chunk().constants[usize::from(i)] else {
                        panic!("type error!");
                    };

                    let func = self.gc.deref(clo_ref).func;
                    let len = self.gc.deref(func).upvalues.len();

                    for i in 0..len {
                        let FuncUpvalue { index, is_local } = self.gc.deref(func).upvalues[i];
                        if is_local {
                            let up = self.capture_upvalue(self.frame().slot + index);
                            let closure = self.gc.deref_mut(clo_ref);
                            closure.upvalues.push(up);
                        } else {
                            let curr_closure = self.frame().closure;
                            let curr_closure = self.gc.deref(curr_closure);
                            let up = curr_closure.upvalues[index];

                            let closure = self.gc.deref_mut(clo_ref);
                            closure.upvalues.push(up);
                        }
                    }

                    self.stack.push(Value::Closure(clo_ref));
                }

                Op::Dup => {
                    let item = self.peek(0);
                    self.push(item);
                }
                Op::Pop => {
                    self.pop();
                }

                Op::Jump(off) => {
                    self.frame_mut().ip += usize::from(off);
                }
                Op::JumpIfFalse(off) => {
                    let Value::Bool(is_true) = self.peek(0) else {
                        panic!("type error!");
                    };

                    if !is_true {
                        self.frame_mut().ip += usize::from(off);
                    }
                }
                Op::JumpIfTrue(off) => {
                    let Value::Bool(is_true) = self.peek(0) else {
                        panic!("type error!");
                    };

                    if is_true {
                        self.frame_mut().ip += usize::from(off);
                    }
                }

                Op::SetSlot(i) => {
                    let idx = self.frame().slot + usize::from(i);
                    self.stack[idx] = self.stack.last().copied().unwrap();
                }
                Op::SaveValue => {
                    self.saved = Some(self.pop());
                }
                Op::UnsaveValue => {
                    let saved = self.saved.take().unwrap();
                    self.push(saved);
                }

                Op::Len => {
                    let len = match self.peek(0) {
                        Value::Array(arr) => self.gc.deref(arr).len(),
                        Value::String(str) => self.gc.deref(str).len(),
                        _ => panic!("type error!"),
                    };

                    self.push(Value::Int(i64::try_from(len).unwrap()))
                }
                Op::Index => {
                    let Value::Int(idx) = self.pop() else {
                        panic!("type error!");
                    };

                    let idx = usize::try_from(idx).unwrap();

                    match self.pop() {
                        Value::String(str) => {
                            let val = self.gc.deref(str).get(idx);
                            self.push(Value::Char(val));
                        }
                        Value::Array(arr) => {
                            let val = self.gc.deref(arr).get(idx);
                            self.push(val);
                        }
                        _ => panic!("type error!"),
                    }
                }
                Op::IndexRev => {
                    let Value::Int(idx) = self.pop() else {
                        panic!("type error!");
                    };

                    let idx = usize::try_from(idx).unwrap();

                    match self.pop() {
                        Value::String(str) => {
                            let str = self.gc.deref(str);
                            if idx >= str.len() {
                                panic!("Index out of bounds!");
                            }

                            let idx = str.len() - idx - 1;
                            let val = str.get(idx);
                            self.push(Value::Char(val));
                        }
                        Value::Array(arr) => {
                            let arr = self.gc.deref(arr);
                            if idx >= arr.len() {
                                panic!("Index out of bounds!");
                            }

                            let idx = arr.len() - idx - 1;
                            let val = arr.get(idx);
                            self.push(val);
                        }
                        _ => panic!("type error!"),
                    }
                }
                Op::Slice(idx) => {
                    let Value::Int(i) = self.chunk().constants[usize::from(idx)] else {
                        panic!("type error!");
                    };

                    let start = ((i as u64 >> u32::BITS) as u32) as usize;
                    let dist_from_end = ((i as u64 as u32) & u32::MAX) as usize;

                    let val = self.peek(0);
                    match val {
                        Value::Array(arr) => {
                            let arr = self.gc.deref(arr);
                            let end = arr.len() - dist_from_end;
                            let new = arr.slice(start..end);
                            let new = self.alloc(new);
                            self.push(Value::Array(new));
                        }
                        Value::String(str) => {
                            let str = self.gc.deref(str);
                            let end = str.len() - dist_from_end;
                            let new = str.slice(start..end);
                            let new = self.alloc(new);
                            self.push(Value::String(new));
                        }
                        _ => panic!("type error!"),
                    }
                }
                Op::MakeArray(n) => {
                    let mut v = Vec::with_capacity(usize::from(n));
                    for _ in 0..n {
                        v.push(self.pop());
                    }

                    v.reverse();

                    let arr = LambArray::from(v);
                    let arr = self.alloc(arr);
                    self.push(Value::Array(arr));
                }

                Op::NumNeg => num_un_op!(-, self),
                Op::BinNeg => self.num_un_op(ops::Not::not),
                Op::LogNeg => self.bool_un_op(ops::Not::not),

                Op::Add => self.add_op(),
                Op::Sub => num_bin_op!(-, self),
                Op::Div => num_bin_op!(/, self),
                Op::Mod => num_bin_op!(%, self),
                Op::Mul => num_bin_op!(*, self),
                Op::BinAnd => self.num_bin_op(ops::BitAnd::bitand),
                Op::BinOr => self.num_bin_op(ops::BitOr::bitor),
                Op::BinXor => self.num_bin_op(ops::BitXor::bitxor),
                Op::LShift => self.num_bin_op(ops::Shl::shl),
                Op::RShift => self.num_bin_op(ops::Shr::shr),

                Op::Eq => self.value_eq_op(identity),
                Op::Ne => self.value_eq_op(ops::Not::not),
                Op::Ge => self.value_cmp_op(|o| matches!(o, Ordering::Equal | Ordering::Greater)),
                Op::Gt => self.value_cmp_op(|o| matches!(o, Ordering::Greater)),
                Op::Le => self.value_cmp_op(|o| matches!(o, Ordering::Less | Ordering::Equal)),
                Op::Lt => self.value_cmp_op(|o| matches!(o, Ordering::Less)),
            }
        }
    }

    fn chunk(&self) -> &Chunk {
        let cls = self.frame().closure;
        let func = self.gc.deref(cls).func;
        &self.gc.deref(func).chunk
    }

    fn frame(&self) -> &CallFrame {
        self.frames.last().unwrap()
    }

    fn frame_mut(&mut self) -> &mut CallFrame {
        self.frames.last_mut().unwrap()
    }

    fn peek(&self, offset: usize) -> Value {
        self.stack.iter().rev().nth(offset).copied().unwrap()
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
                let s = self.gc.intern(format!("{l}{r}"));
                self.push(Value::String(s));
            }
            (Value::Array(larr), Value::Array(rarr)) => {
                let l = self.gc.deref(larr);
                let r = self.gc.deref(rarr);
                let arr = l.into_iter().chain(r).copied().collect();
                let arr_ref = self.alloc(arr);
                self.push(Value::Array(arr_ref));
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

    fn value_eq_op<F>(&mut self, f: F)
    where
        F: Fn(bool) -> bool,
    {
        let rhs = self.pop();
        let lhs = self.pop();
        let eq = matches!(lhs.compare(&rhs, &self.gc), Some(Ordering::Equal));

        self.push(Value::Bool(f(eq)))
    }

    fn close_upvalues(&mut self, slot: usize) {
        let Self {
            gc,
            open_upvalues,
            stack,
            ..
        } = self;

        open_upvalues.retain(|up| {
            let up = gc.deref_mut(*up);
            if up.index >= slot {
                let loc = up.index;
                up.closed = Some(stack[loc]);
                false
            } else {
                true
            }
        });
    }

    fn capture_upvalue(&mut self, index: usize) -> GcRef<Upvalue> {
        let up_ref = self
            .open_upvalues
            .iter()
            .find(|&&up| self.gc.deref(up).index == index)
            .copied();

        match up_ref {
            Some(up_ref) => up_ref,
            None => {
                let up_ref = self.alloc(Upvalue::new(index));
                self.open_upvalues.push(up_ref);
                up_ref
            }
        }
    }

    fn alloc<T: Allocable>(&mut self, item: T) -> GcRef<T> {
        if self.gc.should_collect() {
            self.mark_roots();
            self.gc.collect_garbage();
        }

        self.gc.alloc(item)
    }

    fn mark_roots(&mut self) {
        for value in &self.stack {
            self.gc.mark_value(*value);
        }

        for frame in &self.frames {
            self.gc.mark_object(frame.closure);
        }

        for upvalue in &self.open_upvalues {
            self.gc.mark_object(*upvalue)
        }

        for (k, v) in &self.globals {
            self.gc.mark_object(*k);
            self.gc.mark_value(*v);
        }

        if let Some(val) = self.saved {
            self.gc.mark_value(val);
        }
    }

    fn define_native(&mut self, name: &str, f: fn(&Vm, &[Value]) -> Value) {
        let name = self.gc.intern(name);
        self.globals.insert(name, Value::Native(NativeFunc::new(f)));
    }

    fn native_print(vm: &Self, args: &[Value]) -> Value {
        for arg in args {
            print!("{}", arg.format(&vm.gc));
        }

        Value::Nil
    }

    fn native_println(vm: &Self, args: &[Value]) -> Value {
        for arg in args {
            print!("{}", arg.format(&vm.gc));
        }

        println!();
        Value::Nil
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
