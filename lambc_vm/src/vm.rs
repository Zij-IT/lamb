mod module;

use lambc_parse::{Ident, Import, Script};
use std::{cmp::Ordering, collections::HashMap, convert::identity, ops, path::Path};

use crate::{
    chunk::{Chunk, Op},
    compiler::Compiler,
    gc::{Allocable, GcRef, LambGc},
    value::{Array, Closure, NativeFunction, ResolvedUpvalue, Str, UnresolvedUpvalue, Value},
};

use module::Module;

use self::module::ModuleExport;

pub type RawNative = fn(&Vm, &[Value]) -> Result<Value>;
pub type Result<T> = std::result::Result<T, Error>;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("No global with the name '{0}'")]
    NoSuchGlobal(String),

    #[error("Type Error: Attempt to test a value of type {0} against an array pattern")]
    BadArrayScrutinee(&'static str),

    #[error("Type Error: Attempt to use a value of type {0} as an index")]
    BadIndexType(&'static str),

    #[error("Type Error: Attempt to index into a value of type {0}")]
    BadIndexeeType(&'static str),

    #[error("Callee accepts {0} arguments, but was provided {1}")]
    ArgAmountMismatch(usize, usize),

    #[error("Type Error: Attempt to call a value of type {0}")]
    BadCalleeType(&'static str),

    #[error("Type Error: Expected bool, recieved {0}")]
    CtrlFlowNotBool(&'static str),

    #[error("Type Error: Values of types {1} and {0} can't be compared with {2}")]
    NotComparable(&'static str, &'static str, &'static str),

    #[error("Type Error: The binary op {2} can't be used with values of types {1} and {0}")]
    BinaryTypeMismatch(&'static str, &'static str, &'static str),

    #[error("Type Error: The unary op {1} can't be used with a value of type {0}")]
    UnaryTypeMismatch(&'static str, &'static str),

    #[error("Index {0} is out of bounds (max {1})")]
    IndexOutOfBounds(usize, usize),

    #[error("IoError: {0}")]
    Io(#[from] std::io::Error),

    #[error("Cant convert input to a value of type {0}")]
    InputConv(&'static str),

    #[error("No module with path '{0}' has been loaded.")]
    NoSuchModule(String),

    #[error("Attempt to treat a value of type '{0}' as a module.")]
    NotAModule(&'static str),

    #[error("Module {0} doesn't export an item named {1}")]
    NoExportViaName(String, String),

    #[error("Syntax Errors in import. File: '{0}'")]
    SyntaxErrorInImport(String),
}

macro_rules! num_bin_op {
    (__ONLY_INT, $op:tt, $this:expr) => {{
        let rhs = $this.pop();
        let lhs = $this.pop();

        let val = match (lhs, rhs) {
            (Value::Int(l), Value::Int(r)) => Value::Int(l $op r),
            _ => return $this.error(Error::BinaryTypeMismatch(
                lhs.type_name(),
                rhs.type_name(),
                stringify!($op)
            )),
        };

        $this.push(val);
    }};
    (%, $this:expr) => { num_bin_op!(__ONLY_INT, %, $this) };
    (&, $this:expr) => { num_bin_op!(__ONLY_INT, &, $this) };
    (|, $this:expr) => { num_bin_op!(__ONLY_INT, |, $this) };
    (^, $this:expr) => { num_bin_op!(__ONLY_INT, ^, $this) };
    (<<, $this:expr) => { num_bin_op!(__ONLY_INT, <<, $this) };
    (>>, $this:expr) => { num_bin_op!(__ONLY_INT, >>, $this) };
    ($op:tt, $this:expr) => {{
        let rhs = $this.pop();
        let lhs = $this.pop();

        let val = match (lhs, rhs) {
            (Value::Int(l), Value::Int(r)) => Value::Int(l $op r),
            (Value::Double(l), Value::Double(r)) => Value::Double(l $op r),
            _ => return $this.error(Error::BinaryTypeMismatch(
                lhs.type_name(),
                rhs.type_name(),
                stringify!($op)
            )),
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
            _ => return $this.error(Error::UnaryTypeMismatch(
                rhs.type_name(),
                stringify!($op)
            )),
        };

        $this.push(val);
    }};
}

pub struct Vm {
    gc: LambGc,
    builtins: HashMap<GcRef<Str>, Value>,
    frames: Vec<CallFrame>,
    stack: Vec<Value>,
    saved: Option<Value>,
    modules: HashMap<GcRef<Str>, Module>,
    open_upvalues: Vec<GcRef<ResolvedUpvalue>>,
}

impl Default for Vm {
    fn default() -> Self {
        Self::new()
    }
}

impl Vm {
    pub fn new() -> Self {
        let mut this = Self {
            gc: LambGc::new(),
            builtins: Default::default(),
            modules: Default::default(),
            frames: Vec::with_capacity(64),
            stack: Vec::with_capacity(u8::MAX as usize * 64),
            saved: None,
            open_upvalues: Vec::with_capacity(64),
        };

        this.define_native("print", Self::native_print);
        this.define_native("println", Self::native_println);
        this.define_native("rand", Self::native_rand);
        this.define_native("user_int", Self::native_user_int);
        this.define_native("user_char", Self::native_user_char);
        this
    }

    // TODO:
    // Current behavior: If a module was previously loaded, and we attempt to load
    // a script with the same path, then we keep the module and basically append the
    // changes.
    //
    // Also, figure out how to get good error messages to this place. Perhaps a script
    // shouldn't be all the scripts like this...
    pub fn load_script<P: AsRef<Path>>(&mut self, script: &Script, script_path: P) -> Result<()> {
        let script_path = script_path.as_ref().to_string_lossy();
        let name = self.gc.intern("__LAMB__SCRIPT__");
        let path = self.gc.intern(script_path.as_ref());

        self.modules.entry(path).or_insert(Module::new());
        for import in &script.imports {
            self.load_import(import, Path::new(script_path.as_ref()))?;
        }

        let mut compiler = Compiler::new(name, path);
        compiler.compile(&mut self.gc, script);

        let closure = compiler.finish(&mut self.gc);
        self.stack.push(Value::Closure(closure));
        self.frames.push(CallFrame::new(path, closure, 0));

        Ok(())
    }

    fn load_import<P: AsRef<Path>>(&mut self, import: &Import, script_path: P) -> Result<()> {
        let total_path = script_path
            .as_ref()
            .parent()
            .expect("Can't execute empty file...")
            .join(&import.path)
            .canonicalize()?;

        let module_ref = self.gc.intern(total_path.to_string_lossy());
        // Don't attempt to run an import if there exists an import
        // with the same path
        if self.modules.contains_key(&module_ref) {
            return Ok(());
        }

        let module = std::fs::read_to_string(&total_path)?;
        let script = lambc_parse::script(&module).unwrap();
        self.load_script(&script, &total_path)?;
        self.run()?;

        let export = &script.exports;
        let script = self.gc.intern(script_path.as_ref().to_string_lossy());
        if let Some(Ident(alias)) = &import.alias {
            let alias = self.gc.intern(alias);
            self.modules
                .get_mut(&script)
                .expect("Script must have been added at this point")
                .define_global(alias, Value::ModulePath(module_ref));
        }

        if let Some(exports) = export {
            self.modules
                .get_mut(&module_ref)
                .expect("Module must have been added at this point")
                .build_exports(exports.items.iter().map(|i| ModuleExport {
                    name: self.gc.intern(&i.name.0),
                    alias: i.alias.as_ref().map(|i| self.gc.intern(&i.0)),
                }));
        }

        if let Some(imports) = import.imports.as_ref() {
            for Ident(ref i) in imports {
                let gci = self.gc.intern(i);
                let item = self
                    .modules
                    .get_mut(&module_ref)
                    .expect("Module must have been added at this point")
                    .get_export(gci);

                let item = match item {
                    Some(t) => t,
                    None => return self.error(Error::NoExportViaName(i.clone(), module.clone())),
                };

                self.modules
                    .get_mut(&script)
                    .expect("Script must have been added at this point")
                    .define_global(gci, item);
            }
        }

        Ok(())
    }

    pub fn run(&mut self) -> Result<()> {
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
                        panic!("Compilation Error: DefineGlobal references non-string constant");
                    };

                    let value = self.pop();
                    let module = self.frame().module;
                    let module = self
                        .modules
                        .get_mut(&module)
                        .expect("Module must be defined");

                    module.define_global(name, value);
                }
                Op::GetGlobal(i) => {
                    let Value::String(name) = self.chunk().constants[usize::from(i)] else {
                        panic!("Compilation Error: GetGlobal references non-string constant");
                    };

                    let module = self.frame().module;
                    let module = self.modules.get(&module).expect("Module must be defined");
                    let global = match module.get_global(name) {
                        Some(item) => Some(item),
                        None => self.builtins.get(&name).copied(),
                    };

                    let Some(global) = global else {
                        return self.error(Error::NoSuchGlobal(self.gc.deref(name).0.clone()));
                    };

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
                                return self.error(Error::ArgAmountMismatch(args, func.arity));
                            } else {
                                let frame =
                                    CallFrame::new(func.module, cl, self.stack.len() - 1 - args);
                                self.frames.push(frame);
                            }
                        }
                        Value::Native(native) => {
                            let args = self.stack.len() - args;
                            let result = native.call(self, &self.stack[args..]).map_err(|e| {
                                self.recover();
                                e
                            })?;

                            self.stack.truncate(args - 1);
                            self.push(result);
                        }
                        val => return self.error(Error::BadCalleeType(val.type_name())),
                    }
                }
                Op::Return => {
                    let frame = self.frames.pop().unwrap();
                    let ret_value = self.pop();
                    self.close_upvalues(frame.slot);

                    if self.frames.is_empty() {
                        return Ok(());
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
                    let Value::Function(func_ref) = self.chunk().constants[usize::from(i)] else {
                        panic!("Compilation Error: Op::Closure expects Value::Function");
                    };

                    let func = self.gc.deref(func_ref);
                    let len = func.upvalues.len();
                    let mut closure = Closure::new(func_ref);
                    closure.upvalues.reserve(len);

                    for i in 0..len {
                        let UnresolvedUpvalue { index, is_local } =
                            self.gc.deref(func_ref).upvalues[i];
                        if is_local {
                            let up = self.capture_upvalue(self.frame().slot + index);
                            closure.upvalues.push(up);
                        } else {
                            let curr_closure = self.frame().closure;
                            let curr_closure = self.gc.deref(curr_closure);
                            let up = curr_closure.upvalues[index];
                            closure.upvalues.push(up);
                        }
                    }

                    let closure = self.alloc(closure);
                    self.stack.push(Value::Closure(closure));
                }

                Op::Dup => {
                    let item = self.peek(0);
                    self.push(item);
                }
                Op::Pop(n) => {
                    self.stack.truncate(self.stack.len() - usize::from(n.get()));
                }
                Op::Jump(off) => {
                    self.frame_mut().ip += usize::from(off);
                }
                Op::JumpIfFalse(off) => {
                    let is_true = match self.peek(0) {
                        Value::Bool(is_true) => is_true,
                        val => return self.error(Error::CtrlFlowNotBool(val.type_name())),
                    };

                    if !is_true {
                        self.frame_mut().ip += usize::from(off);
                    }
                }
                Op::JumpIfTrue(off) => {
                    let is_true = match self.peek(0) {
                        Value::Bool(is_true) => is_true,
                        val => return self.error(Error::CtrlFlowNotBool(val.type_name())),
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
                        val => return self.error(Error::BadArrayScrutinee(val.type_name())),
                    };

                    self.push(Value::Int(i64::try_from(len).unwrap()))
                }
                Op::Index => {
                    let idx = match self.pop() {
                        Value::Int(idx) => idx,
                        val => return self.error(Error::BadIndexType(val.type_name())),
                    };

                    let idx = usize::try_from(idx).unwrap();

                    match self.pop() {
                        Value::String(str) => {
                            let str = self.gc.deref(str);
                            let len = str.len();
                            let val = str.get(idx).ok_or_else(|| {
                                self.recover();
                                Error::IndexOutOfBounds(idx, len)
                            })?;

                            self.push(Value::Char(val));
                        }
                        Value::Array(arr) => {
                            let arr = self.gc.deref(arr);
                            let len = arr.len();
                            let val = arr.get(idx).ok_or_else(|| {
                                self.recover();
                                Error::IndexOutOfBounds(idx, len)
                            })?;

                            self.push(val);
                        }
                        val => return self.error(Error::BadIndexeeType(val.type_name())),
                    }
                }
                Op::IndexRev => {
                    let idx = match self.pop() {
                        Value::Int(idx) => idx,
                        val => return self.error(Error::BadIndexType(val.type_name())),
                    };

                    let idx = usize::try_from(idx).unwrap();

                    match self.pop() {
                        Value::String(str) => {
                            let str = self.gc.deref(str);
                            let len = str.len();
                            let idx = len - idx - 1;
                            let val = str.get(idx).ok_or_else(|| {
                                self.recover();
                                Error::IndexOutOfBounds(idx, len)
                            })?;

                            self.push(Value::Char(val));
                        }
                        Value::Array(arr) => {
                            let arr = self.gc.deref(arr);
                            let len = arr.len();
                            let idx = len - idx - 1;
                            let val = arr.get(idx).ok_or_else(|| {
                                self.recover();
                                Error::IndexOutOfBounds(idx, len)
                            })?;

                            self.push(val);
                        }
                        val => return self.error(Error::BadIndexeeType(val.type_name())),
                    }
                }
                Op::Slice(idx) => {
                    let Value::Int(i) = self.chunk().constants[usize::from(idx)] else {
                        panic!("Compilation Error: Slice references non-int constant");
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
                        val => return self.error(Error::BadArrayScrutinee(val.type_name())),
                    }
                }
                Op::MakeArray(n) => {
                    let vec = self.stack.split_off(self.stack.len() - usize::from(n));
                    let arr = Array::from(vec);
                    let arr = self.alloc(arr);
                    self.push(Value::Array(arr));
                }

                Op::NumNeg => num_un_op!(-, self),
                Op::BinNeg => self.num_un_op(ops::Not::not, "~")?,
                Op::LogNeg => self.bool_un_op(ops::Not::not, "!")?,

                Op::Access => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    let (Value::ModulePath(path), Value::ModulePath(item)) = (lhs, rhs) else {
                        return self.error(Error::NotAModule(lhs.type_name()));
                    };

                    let item = self
                        .modules
                        .get(&path)
                        .ok_or_else(|| Error::NoSuchModule(self.gc.deref(path).0.to_string()))
                        .and_then(|module| {
                            module.get_export(item).ok_or_else(|| {
                                Error::NoExportViaName(
                                    self.gc.deref(path).0.to_string(),
                                    self.gc.deref(item).0.to_string(),
                                )
                            })
                        });

                    match item {
                        Ok(v) => self.push(v),
                        Err(e) => return self.error(e),
                    }
                }
                Op::Add => self.add_op()?,
                Op::Sub => num_bin_op!(-, self),
                Op::Div => num_bin_op!(/, self),
                Op::Mod => num_bin_op!(%, self),
                Op::Mul => num_bin_op!(*, self),
                Op::BinAnd => num_bin_op!(&, self),
                Op::BinOr => num_bin_op!(|, self),
                Op::BinXor => num_bin_op!(^, self),
                Op::LShift => num_bin_op!(<<, self),
                Op::RShift => num_bin_op!(>>, self),

                Op::Eq => self.value_eq_op(identity),
                Op::Ne => self.value_eq_op(ops::Not::not),
                Op::Ge => self.value_cmp_op(|o| !matches!(o, Ordering::Less), ">=")?,
                Op::Le => self.value_cmp_op(|o| !matches!(o, Ordering::Greater), "<=")?,
                Op::Gt => self.value_cmp_op(|o| matches!(o, Ordering::Greater), ">")?,
                Op::Lt => self.value_cmp_op(|o| matches!(o, Ordering::Less), "<")?,
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

    fn add_op(&mut self) -> Result<()> {
        let rhs = self.pop();
        let lhs = self.pop();

        match (lhs, rhs) {
            (Value::Int(l), Value::Int(r)) => {
                self.push(Value::Int(l + r));
            }
            (Value::Double(l), Value::Double(r)) => {
                self.push(Value::Double(l + r));
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
            _ => {
                return self.error(Error::BinaryTypeMismatch(
                    lhs.type_name(),
                    rhs.type_name(),
                    "+",
                ));
            }
        }

        Ok(())
    }

    fn bool_un_op<F>(&mut self, f: F, op: &'static str) -> Result<()>
    where
        F: Fn(bool) -> bool,
    {
        let rhs = self.pop();

        let Value::Bool(r) = rhs else {
            return self.error(Error::UnaryTypeMismatch(rhs.type_name(), op));
        };

        self.push(Value::Bool(f(r)));
        Ok(())
    }

    fn num_un_op<F>(&mut self, f: F, op: &'static str) -> Result<()>
    where
        F: Fn(i64) -> i64,
    {
        let rhs = self.pop();

        let Value::Int(r) = rhs else {
            return self.error(Error::UnaryTypeMismatch(rhs.type_name(), op));
        };

        self.push(Value::Int(f(r)));
        Ok(())
    }

    fn value_cmp_op<F>(&mut self, f: F, op: &'static str) -> Result<()>
    where
        F: Fn(Ordering) -> bool,
    {
        let rhs = self.pop();
        let lhs = self.pop();

        let Some(ord) = lhs.compare(&rhs, &self.gc) else {
            return self.error(Error::BinaryTypeMismatch(
                lhs.type_name(),
                rhs.type_name(),
                op,
            ));
        };

        self.push(Value::Bool(f(ord)));
        Ok(())
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

    fn capture_upvalue(&mut self, index: usize) -> GcRef<ResolvedUpvalue> {
        let up_ref = self
            .open_upvalues
            .iter()
            .find(|&&up| self.gc.deref(up).index == index)
            .copied();

        match up_ref {
            Some(up_ref) => up_ref,
            None => {
                let up_ref = self.alloc(ResolvedUpvalue::new(index));
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

        for (k, v) in &self.modules {
            self.gc.mark_object(*k);
            v.mark_items(&mut self.gc);
        }

        for upvalue in &self.open_upvalues {
            self.gc.mark_object(*upvalue)
        }

        for (k, v) in &self.builtins {
            self.gc.mark_object(*k);
            self.gc.mark_value(*v);
        }

        if let Some(val) = self.saved {
            self.gc.mark_value(val);
        }
    }

    fn error(&mut self, err: Error) -> Result<()> {
        self.recover();
        Err(err)
    }

    fn recover(&mut self) {
        self.close_upvalues(0);
        self.stack.clear();
        self.frames.clear();
    }

    fn define_native(&mut self, name: &str, f: RawNative) {
        let name = self.gc.intern(name);
        self.builtins
            .insert(name, Value::Native(NativeFunction::new(f)));
    }

    fn native_print(vm: &Self, args: &[Value]) -> Result<Value> {
        for arg in args {
            print!("{}", arg.format(&vm.gc));
        }

        Ok(Value::Nil)
    }

    fn native_println(vm: &Self, args: &[Value]) -> Result<Value> {
        for arg in args {
            print!("{}", arg.format(&vm.gc));
        }

        println!();
        Ok(Value::Nil)
    }

    fn native_rand(_vm: &Self, args: &[Value]) -> Result<Value> {
        match args.is_empty() {
            true => Ok(Value::Int(rand::random())),
            false => Err(Error::ArgAmountMismatch(0, args.len())),
        }
    }

    fn native_user_int(_vm: &Self, args: &[Value]) -> Result<Value> {
        if !args.is_empty() {
            return Err(Error::ArgAmountMismatch(0, args.len()));
        }

        let mut input = String::new();
        std::io::stdin().read_line(&mut input)?;

        Ok(Value::Int(
            input
                .trim()
                .parse()
                .map_err(|_| Error::InputConv(Value::INT_TYPE_NAME))?,
        ))
    }

    fn native_user_char(_vm: &Self, args: &[Value]) -> Result<Value> {
        if !args.is_empty() {
            return Err(Error::ArgAmountMismatch(0, args.len()));
        }

        let mut input = String::new();
        std::io::stdin().read_line(&mut input)?;

        Ok(Value::Char(input.trim().parse().map_err(|_| {
            Error::InputConv(Value::Char('o').type_name())
        })?))
    }
}

struct CallFrame {
    module: GcRef<Str>,
    closure: GcRef<Closure>,
    ip: usize,
    slot: usize,
}

impl CallFrame {
    fn new(module: GcRef<Str>, closure: GcRef<Closure>, slot: usize) -> Self {
        CallFrame {
            module,
            closure,
            ip: 0,
            slot,
        }
    }
}
