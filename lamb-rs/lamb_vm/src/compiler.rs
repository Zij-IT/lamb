use lamb_ast::{
    Assign, Atom, Binary, BinaryOp, Block as LambBlock, Expr, FuncDef, Ident, Index, Script,
    Statement, Unary,
};

use crate::{
    chunk::{Jump, JumpIdx, Op},
    gc::GcRef,
    value::{FuncUpvalue, LambClosure, LambFunc, LambString, Value},
};

#[derive(Default)]
struct Block {
    pub enclosing: Option<Box<Block>>,
    pub base: usize,
    pub offset: usize,
    pub depth: usize,
}

impl Block {
    fn new(enclosing: Option<Box<Block>>) -> Self {
        Self {
            enclosing,
            base: 0,
            offset: 0,
            depth: 0,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum CompilerType {
    Script,
    Function,
}

#[derive(Clone, PartialEq, Eq)]
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

pub struct Compiler {
    block: Block,
    locals: Vec<Local>,
    func: LambFunc,
    type_: CompilerType,
    enclosing: Option<Box<Compiler>>,
}

impl Compiler {
    pub fn for_script(name: GcRef<LambString>) -> Self {
        Self {
            enclosing: None,
            type_: CompilerType::Script,
            func: LambFunc::new(name),
            block: Block::new(None),
            locals: Vec::new(),
        }
    }

    pub fn for_func(name: GcRef<LambString>) -> Self {
        Self {
            enclosing: None,
            type_: CompilerType::Function,
            func: LambFunc::new(name),
            block: Block::new(None),
            locals: Vec::new(),
        }
    }

    pub fn compile_script<'ast>(&mut self, script: &'ast Script) {
        let Script { block } = script;
        self.compile_block(block);
    }

    // TODO: Add arguments to GcRef
    pub fn finish(mut self) -> GcRef<LambClosure> {
        self.func.chunk.write_op(Op::Return);
        let closure = LambClosure {
            func: GcRef::new(/* self.func */),
            upvalues: Vec::new(),
        };

        GcRef::new(/* closure */)
    }

    fn new_enclosed(self, name: GcRef<LambString>) -> Self {
        let mut other = Self::for_func(name);
        other.enclosing = Some(Box::new(self));
        other
    }

    fn add_local(&mut self, name: String) {
        self.locals.push(Local::new(name, self.block.depth));
    }

    fn local_slot(&self, name: &str) -> Option<usize> {
        let idx = self.local_idx(name)?;
        let depth = self.locals[idx].depth;
        let mut base = None;

        let mut block = Some(&self.block);
        while let Some(b) = block {
            if b.depth == depth {
                base = Some(b.base);
                break;
            }

            block = b.enclosing.as_deref();
        }

        let base = base.expect("Block depths are strictly increasing by 1");
        let predecessors = self
            .locals
            .iter()
            .take(idx)
            .rev()
            .take_while(|l| l.depth == depth)
            .count();

        Some(base + predecessors)
    }

    fn local_idx(&self, name: &str) -> Option<usize> {
        self.locals.iter().rev().position(|l| l.name == name)
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
                self.func.upvalues.push(FuncUpvalue { index, is_local });
                self.func.upvalues.len() - 1
            }
        }
    }

    fn start_new_block(&mut self) {
        let mut block = Block {
            enclosing: None,
            base: self.block.base + self.block.offset,
            offset: 0,
            depth: self.block.depth,
        };

        std::mem::swap(&mut self.block, &mut block);
        self.block.enclosing = Some(Box::new(block));
    }

    fn begin_scope(&mut self) {
        self.block.depth += 1;
    }

    fn end_scope(&mut self) {
        let parent = self.block.enclosing.take().map(|b| *b);
        self.func.chunk.write_op(Op::SaveValue);
        if let Some(parent) = parent {
            self.block = parent;
            for (idx, loc) in self.locals.iter().enumerate().rev() {
                if loc.depth <= self.block.depth {
                    self.locals.truncate(idx);
                    break;
                }

                if loc.is_captured {
                    self.func.chunk.write_op(Op::CloseValue);
                } else {
                    self.func.chunk.write_op(Op::Pop);
                }
            }
        } else {
            // "Popping" global scope
            self.block = Block::new(None);

            // Doing all pops and closing of upvalues
            let Self { locals, func, .. } = self;
            for loc in locals.iter().rev() {
                if loc.is_captured {
                    func.chunk.write_op(Op::CloseValue);
                } else {
                    func.chunk.write_op(Op::Pop);
                }
            }
        }
        self.func.chunk.write_op(Op::UnsaveValue);
    }

    fn write_op(&mut self, op: Op) {
        match op {
            Op::Add
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
            | Op::Pop
            | Op::RShift
            | Op::SaveValue
            | Op::Sub => self.block.offset -= 1,
            Op::Closure(_)
            | Op::Constant(_)
            | Op::Dup
            | Op::GetGlobal(_)
            | Op::GetLocal(_)
            | Op::GetUpvalue(_)
            | Op::Len
            | Op::UnsaveValue => self.block.offset += 1,
            Op::Call(off) | Op::MakeArray(off) => self.block.offset -= usize::from(off),
            Op::Jump(_) | Op::JumpIfFalse(_) | Op::JumpIfTrue(_) => {
                panic!("Jump operators must be written with Compiler::write_jump")
            }
            Op::Return | Op::BinNeg | Op::LogNeg | Op::NumNeg | Op::SetSlot(_) | Op::Slice(_) => {
                self.block.offset += 0
            }
        }

        self.func.chunk.write_op(op);
    }

    fn write_val(&mut self, val: Value) {
        self.func.chunk.write_val(val);
        self.block.offset += 1;
    }

    fn write_jump(&mut self, jmp: Jump) -> JumpIdx {
        self.func.chunk.write_jmp(jmp)
    }

    fn patch_jump(&mut self, jmp: JumpIdx) {
        self.func.chunk.patch_jmp(jmp)
    }

    fn compile_block<'ast>(&mut self, block: &'ast LambBlock) {
        let LambBlock { stats, value } = block;
        self.start_new_block();
        self.begin_scope();

        for stat in stats {
            self.compile_stmt(stat);
        }

        if let Some(value) = value {
            self.compile_expr(value);
        } else {
            self.write_val(Value::Nil);
        }

        self.end_scope();

        // This increases the offset of the previous block, so that this block
        // can be placed on the stack
        self.block.offset += 1;
    }

    fn compile_stmt<'ast>(&mut self, stat: &'ast Statement) {
        match stat {
            Statement::Assign(assign) => {
                let Assign {
                    assignee: Ident(ident),
                    value,
                } = assign;

                if let Expr::FuncDef(f) = value {
                    self.compile_rec_func_def(f);
                } else {
                    self.compile_expr(value);
                }

                let ident: GcRef<LambString> = GcRef::new(/* ident */);
                if self.block.depth == 0 {
                    todo!("Define Global")
                } else {
                    todo!("Define Local")
                }
            }
            Statement::Expr(e) => {
                self.compile_expr(e);
                self.write_op(Op::Pop);
            }
            Statement::Return(Some(r)) => {
                self.compile_expr(r);
                self.write_op(Op::Return);
            }
            Statement::Return(None) => {
                self.write_val(Value::Nil);
                self.write_op(Op::Return);
            }
        }
    }

    fn compile_expr<'ast>(&mut self, value: &'ast Expr) {
        match value {
            Expr::Unary(Unary { rhs, op }) => {
                self.compile_expr(rhs);
                self.write_op(Op::from(*op));
            }
            Expr::Binary(Binary { lhs, rhs, op }) => match Op::try_from(*op) {
                Ok(op) => {
                    self.compile_expr(lhs);
                    self.compile_expr(rhs);
                    self.write_op(op);
                }
                Err(BinaryOp::LogAnd) => self.compile_sc_op(lhs, rhs, Jump::IfFalse),
                Err(BinaryOp::LogOr) => self.compile_sc_op(lhs, rhs, Jump::IfTrue),
                Err(BinaryOp::LApply) => self.compile_apply(lhs, rhs),
                Err(BinaryOp::RApply) => self.compile_apply(rhs, lhs),
                Err(BinaryOp::LCompose) => self.compile_compose(lhs, rhs),
                Err(BinaryOp::RCompose) => self.compile_compose(rhs, lhs),
                Err(_) => unreachable!("All cases are actually covered!"),
            },
            Expr::Index(Index { indexee, index }) => {
                self.compile_expr(indexee);
                self.compile_expr(index);
                self.write_op(Op::Index);
            }
            Expr::FuncCall(_) => todo!(),
            Expr::If(_) => todo!(),
            Expr::Case(_) => todo!(),
            Expr::FuncDef(_) => todo!(),
            Expr::Block(block) => self.compile_block(block),
            Expr::Atom(atom) => self.compile_atom(atom),
            Expr::Error => unimplemented!("Attempt to compile Expr::Error"),
        }
    }

    fn compile_rec_func_def<'ast>(&self, def: &'ast FuncDef) {
        todo!()
    }

    fn compile_apply<'ast>(&mut self, lhs: &'ast Expr, rhs: &'ast Expr) {
        todo!()
    }

    fn compile_compose<'ast>(&mut self, lhs: &'ast Expr, rhs: &'ast Expr) {
        todo!()
    }

    fn compile_sc_op<'ast>(&mut self, lhs: &'ast Expr, rhs: &'ast Expr, jump: Jump) {
        self.compile_expr(lhs);
        let idx = self.write_jump(jump);
        self.write_op(Op::Pop);
        self.compile_expr(rhs);
        self.patch_jump(idx);
    }

    fn compile_atom<'ast>(&mut self, atom: &'ast Atom) {
        match atom {
            Atom::Literal(l) => self.compile_literal(l),
            Atom::Ident(i) => self.compile_ident(i),
            Atom::Array(arr) => {
                let len = arr.len();
                for e in arr.iter().rev() {
                    self.compile_expr(e);
                }

                self.write_op(Op::MakeArray(len.try_into().unwrap()));
            }
        }
    }
}
