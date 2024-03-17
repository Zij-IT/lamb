mod expr;
mod info;
mod pattern;

use std::num::NonZeroU16;

use lambc_parse::{Define, Expr, Ident, Module, Statement};

use self::info::{Block, FunctionInfo};
use crate::{
    chunk::{Jump, JumpIdx, Op},
    compiler::State,
    gc::GcRef,
    value::{Closure, Function, Str, Value},
};

// Safety: 1, despite its appearence, is not equal to zero
const NZ_ONE_U16: NonZeroU16 = unsafe { NonZeroU16::new_unchecked(1) };

#[derive(Debug)]
pub struct Lowerer<'a, 'b> {
    state: &'a mut State<'b>,
    module: GcRef<Str>,
    info: FunctionInfo,
}

impl<'a, 'b> Lowerer<'a, 'b> {
    pub fn new(
        state: &'a mut State<'b>,
        name: GcRef<Str>,
        module: GcRef<Str>,
    ) -> Self {
        Self { state, module, info: FunctionInfo::new(name, module) }
    }

    pub fn lower(mut self, script: &Module) -> GcRef<Closure> {
        self.lower_script(script);
        self.finish()
    }

    pub fn finish(mut self) -> GcRef<Closure> {
        self.write_op(Op::Return);
        let closure = Closure {
            func: self.state.gc.alloc(self.info.func),
            upvalues: Vec::new(),
        };

        self.state.gc.alloc(closure)
    }

    fn block(&self) -> &Block {
        self.info.block()
    }

    fn block_mut(&mut self) -> &mut Block {
        self.info.block_mut()
    }

    fn start_block(&mut self) {
        let block = self.block();
        let block = Block {
            base: block.base + block.offset,
            offset: 0,
            depth: block.depth + 1,
        };

        self.info.blocks.push(block);
    }

    fn end_block(&mut self) {
        self.info.blocks.pop();
        let depth = self.block().depth;

        self.info.func.chunk.write_op(Op::SaveValue);
        for (idx, loc) in self.info.locals.iter().enumerate().rev() {
            if loc.depth <= depth {
                self.info.locals.truncate(idx + 1);
                break;
            }

            let op = if loc.is_captured {
                Op::CloseValue
            } else {
                Op::Pop(NZ_ONE_U16)
            };

            self.info.func.chunk.write_op(op);
        }
        self.info.func.chunk.write_op(Op::UnsaveValue);
    }

    fn add_arg(&mut self, Ident { raw, .. }: &Ident) {
        let rf = self.state.gc.intern(raw);
        self.info.func.chunk.constants.push(Value::String(rf));
        self.info.add_local(raw.clone());
        self.info.func.arity += 1;
        self.block_mut().offset += 1;
    }

    fn write_closure(&mut self, func: Function) {
        self.info
            .func
            .chunk
            .constants
            .push(Value::Function(self.state.gc.alloc(func)));

        let idx = self.info.func.chunk.constants.len() - 1;
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
            Op::MakeArray(elems) => {
                self.block_mut().offset -= usize::from(elems) - 1
            }
            Op::Call(off) => self.block_mut().offset -= usize::from(off),
            Op::Return
            | Op::BinNeg
            | Op::LogNeg
            | Op::NumNeg
            | Op::SetSlot(_) => {
                self.block_mut().offset += 0;
            }
            Op::Jump(_) | Op::JumpIfFalse(_) | Op::JumpIfTrue(_) => {
                panic!(
                    "Jump operators must be written with Lowerer::write_jump"
                )
            }
        }

        self.info.func.chunk.write_op(op);
    }

    fn write_const_op(&mut self, val: Value) {
        self.info.func.chunk.constants.push(val);
        let idx = self.info.func.chunk.constants.len() - 1;
        self.write_op(Op::Constant(idx.try_into().unwrap()));
    }

    fn write_jump(&mut self, jmp: Jump) -> JumpIdx {
        self.info.func.chunk.write_jmp(jmp)
    }

    fn patch_jump(&mut self, jmp: JumpIdx) {
        self.info.func.chunk.patch_jmp(jmp);
    }

    fn lower_script(&mut self, script: &Module) {
        for stat in &script.statements {
            self.lower_stmt(stat);
        }
    }

    fn lower_stmt(&mut self, stat: &Statement) {
        match stat {
            Statement::Define(assign) => {
                let Define {
                    ident: ident @ Ident { raw, .. },
                    value,
                    span: _,
                } = assign;

                if let Expr::FnDef(f) = value {
                    if f.recursive {
                        self.lower_rec_func_def(ident, f);
                        return;
                    }
                }

                self.lower_expr(value);

                let ident_obj = self.state.gc.intern(raw);
                self.info.func.chunk.write_val(Value::String(ident_obj));
                if self.block().depth == 0 {
                    let idx = self.info.func.chunk.constants.len() - 1;
                    self.write_op(Op::DefineGlobal(idx.try_into().unwrap()));
                } else {
                    self.info.add_local(raw.clone());
                }
            }
            Statement::Expr(e) => {
                self.lower_expr(&e.expr);
                self.write_op(Op::Pop(NZ_ONE_U16));
            }
        }
    }
}
