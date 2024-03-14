mod expr;
mod pattern;

use std::num::NonZeroU16;

use lambc_parse::{Define, Expr, Ident, Module, Statement};

use crate::{
    chunk::{Jump, JumpIdx, Op},
    gc::{GcRef, LambGc},
    value::{Closure, Function, Str, UnresolvedUpvalue, Value},
};

// Safety: 1, despite its appearence, is not equal to zero
const NZ_ONE_U16: NonZeroU16 = unsafe { NonZeroU16::new_unchecked(1) };

#[derive(Debug, Default)]
struct Block {
    pub base: usize,
    pub offset: usize,
    pub depth: usize,
}

impl Block {
    fn new_for_func() -> Self {
        Self {
            base: 0,
            offset: 1,
            depth: 0,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Local {
    name: String,
    depth: usize,
    is_captured: bool,
}

impl Local {
    pub fn new(name: String, depth: usize) -> Self {
        Self {
            name,
            depth,
            is_captured: false,
        }
    }

    pub fn mark_captured(&mut self) {
        self.is_captured = true;
    }
}

#[derive(Debug)]
pub struct Lowerer {
    module: GcRef<Str>,
    blocks: Vec<Block>,
    locals: Vec<Local>,
    func: Function,
    enclosing: Option<Box<Self>>,
}

impl Lowerer {
    pub fn new(name: GcRef<Str>, module: GcRef<Str>) -> Self {
        Self {
            module,
            enclosing: None,
            func: Function::new(name, module),
            blocks: vec![Block::new_for_func()],
            // This local refers to the function that is currently being lowered.
            // By setting its depth to zero, we make sure it is unaccessible to
            // the user
            locals: vec![Local::new("".into(), 0)],
        }
    }

    pub fn lower(mut self, gc: &mut LambGc, script: &Module) -> GcRef<Closure> {
        self.lower_script(script, gc);
        self.finish(gc)
    }

    pub fn finish(mut self, gc: &mut LambGc) -> GcRef<Closure> {
        self.write_op(Op::Return);
        let closure = Closure {
            func: gc.alloc(self.func),
            upvalues: Vec::new(),
        };

        gc.alloc(closure)
    }

    fn block(&self) -> &Block {
        self.blocks.last().unwrap()
    }

    fn block_mut(&mut self) -> &mut Block {
        self.blocks.last_mut().unwrap()
    }

    fn start_block(&mut self) {
        let block = self.block();
        let block = Block {
            base: block.base + block.offset,
            offset: 0,
            depth: block.depth + 1,
        };

        self.blocks.push(block);
    }

    fn end_block(&mut self) {
        self.blocks.pop();
        let depth = self.block().depth;

        self.func.chunk.write_op(Op::SaveValue);
        for (idx, loc) in self.locals.iter().enumerate().rev() {
            if loc.depth <= depth {
                self.locals.truncate(idx + 1);
                break;
            }

            let op = if loc.is_captured {
                Op::CloseValue
            } else {
                Op::Pop(NZ_ONE_U16)
            };

            self.func.chunk.write_op(op);
        }
        self.func.chunk.write_op(Op::UnsaveValue);
    }

    fn add_arg(&mut self, gc: &mut LambGc, Ident { raw, .. }: &Ident) {
        let rf = gc.intern(raw);
        self.func.chunk.constants.push(Value::String(rf));
        self.add_local(raw.clone());
        self.func.arity += 1;
        self.block_mut().offset += 1;
    }

    fn add_local(&mut self, name: String) {
        self.locals.push(Local::new(name, self.block().depth));
    }

    fn local_slot(&self, name: &str) -> Option<usize> {
        let idx = self.local_idx(name)?;
        let depth = self.locals[idx].depth;

        // This looks backwards to find the block that the local with a depth
        // of `depth` is found. This local must be within one of the blocks,
        // and as such we must be able to find the associated block.
        let base = self
            .blocks
            .iter()
            .rev()
            .find(|b| b.depth == depth)
            .map(|b| b.base)
            .expect("The local must exist within a block of the same depth");

        // This finds the amount of locals that wee declared in the block
        // before the local `name`
        let predecessors = &self
            .locals
            .iter()
            .take(idx)
            .rev()
            .take_while(|l| l.depth == depth)
            .count();

        Some(base + predecessors)
    }

    fn local_idx(&self, name: &str) -> Option<usize> {
        self.locals.iter().rposition(|l| l.name == name)
    }

    fn upvalue_idx(&mut self, name: &str) -> Option<usize> {
        let parent = self.enclosing.as_deref_mut()?;
        if let Some(idx) = parent.local_idx(name) {
            parent.locals[idx].mark_captured();
            return Some(self.add_upvalue(idx, true));
        }

        parent.upvalue_idx(name).map(|l| self.add_upvalue(l, false))
    }

    fn add_upvalue(&mut self, index: usize, is_local: bool) -> usize {
        let pos = self
            .func
            .upvalues
            .iter()
            .position(|u| u.index == index && u.is_local == is_local);

        match pos {
            Some(p) => p,
            None => {
                self.func
                    .upvalues
                    .push(UnresolvedUpvalue { index, is_local });
                self.func.upvalues.len() - 1
            }
        }
    }

    fn write_closure(&mut self, gc: &mut LambGc, func: Function) {
        self.func
            .chunk
            .constants
            .push(Value::Function(gc.alloc(func)));

        let idx = self.func.chunk.constants.len() - 1;
        self.write_op(Op::Closure(idx.try_into().unwrap()))
    }

    fn write_op(&mut self, op: Op) {
        match op {
            Op::Add
            | Op::Access
            | Op::BinAnd
            | Op::BinOr
            | Op::BinXor
            | Op::CloseValue
            | Op::DefineGlobal(_)
            | Op::Div
            | Op::Eq
            | Op::Ge
            | Op::Gt
            | Op::Index
            | Op::IndexRev
            | Op::LShift
            | Op::Le
            | Op::Lt
            | Op::Mod
            | Op::Mul
            | Op::Ne
            | Op::RShift
            | Op::SaveValue
            | Op::Sub => self.block_mut().offset -= 1,
            Op::Pop(n) => self.block_mut().offset -= usize::from(n.get()),
            Op::Closure(_)
            | Op::Constant(_)
            | Op::Dup
            | Op::GetGlobal(_)
            | Op::GetLocal(_)
            | Op::GetUpvalue(_)
            | Op::Len
            | Op::Slice(_)
            | Op::UnsaveValue => self.block_mut().offset += 1,
            Op::MakeArray(0) => self.block_mut().offset += 1,
            Op::MakeArray(elems) => self.block_mut().offset -= usize::from(elems) - 1,
            Op::Call(off) => self.block_mut().offset -= usize::from(off),
            Op::Return | Op::BinNeg | Op::LogNeg | Op::NumNeg | Op::SetSlot(_) => {
                self.block_mut().offset += 0;
            }
            Op::Jump(_) | Op::JumpIfFalse(_) | Op::JumpIfTrue(_) => {
                panic!("Jump operators must be written with Lowerer::write_jump")
            }
        }

        self.func.chunk.write_op(op);
    }

    fn write_const_op(&mut self, val: Value) {
        self.func.chunk.constants.push(val);
        let idx = self.func.chunk.constants.len() - 1;
        self.write_op(Op::Constant(idx.try_into().unwrap()));
    }

    fn write_jump(&mut self, jmp: Jump) -> JumpIdx {
        self.func.chunk.write_jmp(jmp)
    }

    fn patch_jump(&mut self, jmp: JumpIdx) {
        self.func.chunk.patch_jmp(jmp);
    }

    fn lower_script(&mut self, script: &Module, gc: &mut LambGc) {
        for stat in &script.statements {
            self.lower_stmt(stat, gc);
        }
    }

    fn lower_stmt(&mut self, stat: &Statement, gc: &mut LambGc) {
        match stat {
            Statement::Define(assign) => {
                let Define {
                    ident: ident @ Ident { raw, .. },
                    value,
                    span: _,
                } = assign;

                if let Expr::FnDef(f) = value {
                    if f.recursive {
                        self.lower_rec_func_def(ident, f, gc);
                        return;
                    }
                }

                self.lower_expr(value, gc);

                let ident_obj = gc.intern(raw);
                self.func.chunk.write_val(Value::String(ident_obj));
                if self.block().depth == 0 {
                    let idx = self.func.chunk.constants.len() - 1;
                    self.write_op(Op::DefineGlobal(idx.try_into().unwrap()));
                } else {
                    self.add_local(raw.clone());
                }
            }
            Statement::Expr(e) => {
                self.lower_expr(&e.expr, gc);
                self.write_op(Op::Pop(NZ_ONE_U16));
            }
        }
    }
}
