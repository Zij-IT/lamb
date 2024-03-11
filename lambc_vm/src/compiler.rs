use std::num::NonZeroU16;

use lambc_parse::{
    BinaryOp, Block as LambBlock, BoolLit, Case, CaseArm, CharLit, Define, Else, Expr, F64Lit,
    FnDef, I64Base, I64Lit, Ident, If, IfCond, InnerPattern, List, LiteralPattern, Module, NilLit,
    Pattern, Return, Statement, StrLit,
};

use crate::{
    chunk::{Jump, JumpIdx, Op},
    gc::{GcRef, LambGc},
    value::{Closure, Function, Str, UnresolvedUpvalue, Value},
};

// This is safe because it contains whitespace, which user identifiers
// cannot have.
const SCRUTINEE: &str = " SCRUTINEE";
const COMPOSE_ARG: &str = "COMP ARG";

// Safety: 1, despite its appearence, is not equal to zero
const NZ_ONE_U16: NonZeroU16 = unsafe { NonZeroU16::new_unchecked(1) };

#[derive(Debug, Default)]
struct Block {
    pub enclosing: Option<Box<Block>>,
    pub base: usize,
    pub offset: usize,
    pub depth: usize,
}

impl Block {
    fn new_for_func(enclosing: Option<Box<Block>>) -> Self {
        Self {
            enclosing,
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
pub struct Compiler {
    module: GcRef<Str>,
    block: Block,
    locals: Vec<Local>,
    func: Function,
    enclosing: Option<Box<Compiler>>,
}

impl Compiler {
    pub fn new(name: GcRef<Str>, module: GcRef<Str>) -> Self {
        Self {
            module,
            enclosing: None,
            func: Function::new(name, module),
            block: Block::new_for_func(None),
            // This local refers to the function that is currently being compiled.
            // By setting its depth to zero, we make sure it is unaccessible to
            // the user
            locals: vec![Local::new("".into(), 0)],
        }
    }

    pub fn compile(&mut self, gc: &mut LambGc, script: &Module) {
        self.compile_script(script, gc);
    }

    pub fn finish(mut self, gc: &mut LambGc) -> GcRef<Closure> {
        self.write_op(Op::Return);
        let closure = Closure {
            func: gc.alloc(self.func),
            upvalues: Vec::new(),
        };

        gc.alloc(closure)
    }

    fn start_block(&mut self) {
        let mut block = Block {
            enclosing: None,
            base: self.block.base + self.block.offset,
            offset: 0,
            depth: self.block.depth + 1,
        };

        std::mem::swap(&mut self.block, &mut block);
        self.block.enclosing = Some(Box::new(block));
    }

    fn end_block(&mut self) {
        let parent = self
            .block
            .enclosing
            .take()
            .map(|b| *b)
            .expect("Compiler starts with a block that should never be popped");

        self.func.chunk.write_op(Op::SaveValue);
        self.block = parent;
        for (idx, loc) in self.locals.iter().enumerate().rev() {
            if loc.depth <= self.block.depth {
                self.locals.truncate(idx + 1);
                break;
            }

            if loc.is_captured {
                self.func.chunk.write_op(Op::CloseValue);
            } else {
                self.func
                    .chunk
                    .write_op(Op::Pop(NonZeroU16::new(1).unwrap()));
            }
        }
        self.func.chunk.write_op(Op::UnsaveValue);
    }

    fn add_arg(&mut self, gc: &mut LambGc, Ident { raw: arg, .. }: &Ident) {
        let rf = gc.intern(arg);
        self.func.chunk.constants.push(Value::String(rf));
        self.add_local(arg.clone());
        self.func.arity += 1;
        self.block.offset += 1;
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
        self.locals
            .iter()
            .enumerate()
            .rev()
            .find_map(|(i, l)| if l.name == name { Some(i) } else { None })
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
            | Op::Sub => self.block.offset -= 1,
            Op::Pop(n) => self.block.offset -= usize::from(n.get()),
            Op::Closure(_)
            | Op::Constant(_)
            | Op::Dup
            | Op::GetGlobal(_)
            | Op::GetLocal(_)
            | Op::GetUpvalue(_)
            | Op::Len
            | Op::Slice(_)
            | Op::UnsaveValue => self.block.offset += 1,
            Op::MakeArray(0) => self.block.offset += 1,
            Op::MakeArray(elems) => self.block.offset -= usize::from(elems) - 1,
            Op::Call(off) => self.block.offset -= usize::from(off),
            Op::Return | Op::BinNeg | Op::LogNeg | Op::NumNeg | Op::SetSlot(_) => {
                self.block.offset += 0;
            }
            Op::Jump(_) | Op::JumpIfFalse(_) | Op::JumpIfTrue(_) => {
                panic!("Jump operators must be written with Compiler::write_jump")
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

    fn compile_script(&mut self, script: &Module, gc: &mut LambGc) {
        for stat in &script.statements {
            self.compile_stmt(stat, gc);
        }
    }

    fn compile_block(&mut self, block: &LambBlock, gc: &mut LambGc) {
        let LambBlock {
            value,
            statements: stats,
            ..
        } = block;
        self.start_block();

        for stat in stats {
            self.compile_stmt(stat, gc);
        }

        if let Some(value) = value {
            self.compile_expr(value, gc);
        } else {
            self.write_const_op(Value::Nil);
        }

        self.end_block();

        // Every block leaves 1 extra value on the stack for the block above it.
        // Thus, a value is left on the stack for this block, which increases its
        // offset
        self.block.offset += 1;
    }

    fn compile_stmt(&mut self, stat: &Statement, gc: &mut LambGc) {
        match stat {
            Statement::Define(assign) => {
                let Define {
                    ident: i @ Ident { raw: ident, .. },
                    value,
                    span: _,
                } = assign;

                if let Expr::FnDef(f) = value {
                    if f.recursive {
                        self.compile_rec_func_def(i, f, gc);
                        return;
                    }
                }

                self.compile_expr(value, gc);

                let ident_obj = gc.intern(ident);
                self.func.chunk.write_val(Value::String(ident_obj));
                if self.block.depth == 0 {
                    let idx = self.func.chunk.constants.len() - 1;
                    self.write_op(Op::DefineGlobal(idx.try_into().unwrap()));
                } else {
                    self.add_local(ident.clone());
                }
            }
            Statement::Expr(e) => {
                self.compile_expr(&e.expr, gc);
                self.write_op(Op::Pop(NZ_ONE_U16));
            }
        }
    }

    fn compile_expr(&mut self, value: &Expr, gc: &mut LambGc) {
        match value {
            Expr::Unary(un) => {
                self.compile_expr(&un.rhs, gc);
                self.write_op(Op::from(un.op));
            }
            Expr::Binary(bin) => match Op::try_from(bin.op) {
                Ok(op) => {
                    self.compile_expr(&bin.lhs, gc);
                    self.compile_expr(&bin.rhs, gc);
                    self.write_op(op);
                }
                Err(BinaryOp::Land) => self.compile_sc_op(&bin.lhs, &bin.rhs, Jump::IfFalse, gc),
                Err(BinaryOp::Lor) => self.compile_sc_op(&bin.lhs, &bin.rhs, Jump::IfTrue, gc),
                Err(BinaryOp::Appl) => self.compile_apply(&bin.lhs, &bin.rhs, gc),
                Err(BinaryOp::Appr) => self.compile_apply(&bin.rhs, &bin.lhs, gc),
                Err(BinaryOp::Cpsl) => self.compile_compose(&bin.lhs, &bin.rhs, gc),
                Err(BinaryOp::Cpsr) => self.compile_compose(&bin.rhs, &bin.lhs, gc),
                Err(_) => unreachable!("All cases are actually covered!"),
            },
            Expr::Index(i) => {
                self.compile_expr(&i.lhs, gc);
                self.compile_expr(&i.rhs, gc);
                self.write_op(Op::Index);
            }
            Expr::Call(call) => {
                self.compile_expr(&call.callee, gc);
                for arg in &call.args {
                    self.compile_expr(arg, gc);
                }
                self.write_op(Op::Call(call.args.len().try_into().unwrap()))
            }
            Expr::If(if_) => self.compile_if_expr(if_, gc),
            Expr::Case(c) => self.compile_case(c, gc),
            Expr::FnDef(f) => self.compile_func(f, gc, "Anon Func".into()),
            Expr::Block(block) => self.compile_block(block, gc),
            Expr::Ident(i) => self.compile_ident(i, gc),
            Expr::Char(c) => self.compile_char_literal(c),
            Expr::String(s) => self.compile_str_literal(gc, s),
            Expr::Bool(b) => self.compile_bool_literal(b),
            Expr::Nil(n) => self.compile_nil_literal(n),
            Expr::I64(i) => self.compile_i64_literal(i),
            Expr::F64(f) => self.compile_f64_literal(f),
            Expr::List(list) => self.compile_list(gc, list),
            Expr::Group(gr) => self.compile_expr(&gr.value, gc),
            Expr::Return(ret) => self.compile_return(gc, ret),
            Expr::Path(_) => todo!(),
        }
    }

    fn compile_rec_func_def<'ast>(
        &mut self,
        Ident { raw: inner, .. }: &'ast Ident,
        def: &'ast FnDef,
        gc: &mut LambGc,
    ) {
        let ident = gc.intern(inner);
        self.func.chunk.write_val(Value::String(ident));
        let idx = self.func.chunk.constants.len() - 1;

        self.compile_func(def, gc, inner.clone());

        if self.block.depth == 0 {
            self.write_op(Op::DefineGlobal(idx.try_into().unwrap()))
        } else {
            self.add_local(inner.clone());
        }
    }

    fn compile_apply<'ast>(&mut self, lhs: &'ast Expr, rhs: &'ast Expr, gc: &mut LambGc) {
        self.compile_expr(lhs, gc);
        self.compile_expr(rhs, gc);
        self.write_op(Op::Call(1));
    }

    fn compile_compose<'ast>(&mut self, lhs: &'ast Expr, rhs: &'ast Expr, gc: &mut LambGc) {
        let ident = gc.intern("Anon Func");
        let mut composition = Compiler::new(ident, self.module);
        composition.locals[0].depth = 1;

        // `self` now refers to `composition`. This is done because I
        // can't figure out how to make composition have a reference to self :D
        let orig_self = std::mem::replace(self, composition);

        self.enclosing = Some(Box::new(orig_self));
        self.add_arg(
            gc,
            &Ident {
                raw: COMPOSE_ARG.into(),
                // TODO: Remove this span!
                span: lhs.span(),
            },
        );
        self.start_block();
        self.compile_expr(lhs, gc);
        self.compile_expr(rhs, gc);
        // It's clearly at spot 1... soo.... cheat!
        self.write_op(Op::GetLocal(1));
        self.write_op(Op::Call(1));
        self.write_op(Op::Call(1));
        self.write_op(Op::Return);
        self.end_block();

        // `self` now refers to the original `self`.
        let this = *self.enclosing.take().unwrap();
        let composition = std::mem::replace(self, this);

        self.write_closure(gc, composition.func);
    }

    fn compile_sc_op<'ast>(
        &mut self,
        lhs: &'ast Expr,
        rhs: &'ast Expr,
        jump: Jump,
        gc: &mut LambGc,
    ) {
        self.compile_expr(lhs, gc);
        let idx = self.write_jump(jump);
        self.write_op(Op::Pop(NZ_ONE_U16));
        self.compile_expr(rhs, gc);
        self.patch_jump(idx);
    }

    // fn compile_path<'ast>(&mut self, path: &'ast lambc_parse::Path, gc: &mut LambGc) {
    //     // Module::item1::item2
    //     // ^^^^^^  ^^^^^  ^^^^^
    //     //     |   |      |
    //     //     |   |      +--> ModuleItemRef
    //     //     |   |
    //     //     |   +--> ModuleItemRef
    //     //     |
    //     //     +--> Expr
    //     let (first, rest) = path.segments.split_first().unwrap();
    //     self.compile_ident(first, gc);

    //     let mut iter = rest.chunks_exact(2);
    //     while let Some(&[Ident(ref left), Ident(ref right)]) = iter.next() {
    //         let left = Value::ModulePath(gc.intern(left));
    //         let right = Value::ModulePath(gc.intern(right));
    //         self.write_const_op(left);
    //         self.write_op(Op::Access);
    //         self.write_const_op(right);
    //     }

    //     let path = match iter.remainder() {
    //         &[Ident(ref i)] => Value::ModulePath(gc.intern(i)),
    //         _ => return,
    //     };

    //     self.write_const_op(path);
    //     self.write_op(Op::Access);
    // }

    fn compile_if_expr(&mut self, if_: &If, gc: &mut LambGc) {
        let If {
            cond: IfCond {
                cond, body: block, ..
            },
            elif: elifs,
            els_: els,
            ..
        } = if_;

        // <cond>
        // jmpfalse .cond_false1
        // <block>
        // jmp .past_else
        // .cond_false1:
        // --------- Either Elif
        // <cond>
        // jmpfalse .cond_false2
        // <block>
        // jmp .past_else
        // .cond_false2
        // --------- Or Else
        // <block>
        // .past_else

        // Each `compile_conditional` puts the offset at +1 item however
        // if the previous condition wasn't true, it isn't executed. This
        // means that the next if will run as if it was the first.
        let offset = self.block.offset;
        let mut to_elses = Vec::with_capacity(1 + elifs.len());
        to_elses.push(self.compile_conditional(cond, block, gc));

        // compile elifs
        for IfCond {
            cond, body: block, ..
        } in elifs
        {
            self.write_op(Op::Pop(NZ_ONE_U16));
            self.block.offset = offset;
            to_elses.push(self.compile_conditional(cond, block, gc));
        }

        // compile else
        self.write_op(Op::Pop(NZ_ONE_U16));
        if let Some(Else { body: block, .. }) = els {
            self.compile_block(block, gc);
        } else {
            self.write_const_op(Value::Nil);
        }

        for jmp in to_elses {
            self.patch_jump(jmp);
        }
    }

    fn compile_conditional<'ast>(
        &mut self,
        cond: &'ast Expr,
        block: &'ast LambBlock,
        gc: &mut LambGc,
    ) -> JumpIdx {
        self.compile_expr(cond, gc);
        let cond_false = self.write_jump(Jump::IfFalse);
        self.write_op(Op::Pop(NZ_ONE_U16));
        self.compile_block(block, gc);
        let past_else = self.write_jump(Jump::Always);
        self.patch_jump(cond_false);
        past_else
    }

    fn compile_case(&mut self, c: &Case, gc: &mut LambGc) {
        let Case {
            arms,
            scrutinee: value,
            ..
        } = c;

        self.compile_expr(value, gc);

        gc.intern(SCRUTINEE);
        self.add_local(SCRUTINEE.to_string());

        let mut to_elses = Vec::with_capacity(arms.len());
        for arm in arms {
            let to_else = self.compile_case_arm(arm, gc);
            to_elses.push(to_else);
        }

        // If no match arms are taken, use `Op::Pop`
        self.write_op(Op::Pop(NZ_ONE_U16));
        self.write_const_op(Value::Nil);

        for jmp in to_elses {
            self.patch_jump(jmp);
        }

        assert_eq!(self.locals.pop().unwrap().name, SCRUTINEE);
    }

    fn compile_case_arm(&mut self, c: &CaseArm, gc: &mut LambGc) -> JumpIdx {
        let CaseArm {
            pattern,
            body: on_match,
            ..
        } = c;

        let offset_before_arm = self.block.offset;
        let pre_local_count = self.locals.len();

        let bindings = pattern.binding_names();
        let binding_count = bindings.len();

        for Ident { raw: i, .. } in bindings {
            gc.intern(i);

            self.write_const_op(Value::Nil);
            self.func
                .chunk
                .constants
                .push(Value::String(gc.alloc(Str::new(i))));

            self.add_local(i.clone());
        }

        self.write_op(Op::GetLocal(
            u16::try_from(self.block.base + offset_before_arm - 1).unwrap(),
        ));

        self.compile_pattern(pattern, gc);

        let offset_match = self.block.offset;
        let if_no_match = self.write_jump(Jump::IfFalse);

        // If match ----->
        // Remove True
        self.write_op(Op::Pop(NZ_ONE_U16));
        self.compile_expr(on_match, gc);
        self.write_op(Op::SaveValue);

        // Remove scrutinee dup
        // Remove `binding_count` amount of bindings
        // Remove scrutinee
        self.write_op(Op::Pop(
            NonZeroU16::new(u16::try_from(binding_count).unwrap() + 2).unwrap(),
        ));

        self.write_op(Op::UnsaveValue);

        let past_arms = self.write_jump(Jump::Always);
        // <----- If match

        // Only one of these are run, so we want to set the offset
        // to before the "If match" was executed.
        self.block.offset = offset_match;

        // If no match ----->
        self.patch_jump(if_no_match);

        // Remove `false`
        // Remove scrutinee dup
        // Remove `binding_count` amount of bindings
        self.write_op(Op::Pop(
            NonZeroU16::new(u16::try_from(binding_count).unwrap() + 2).unwrap(),
        ));

        self.block.offset = offset_before_arm;
        self.locals.truncate(pre_local_count);
        // <----- If no match

        past_arms
    }

    // Note:
    //
    //     The scrutinee is assumed to be sitting on the top off the stack
    //     and is *only* to be removed after the case arm is complete.
    //
    //     This means that any sub-patterns must duplicate it.
    fn compile_pattern(&mut self, c: &Pattern, gc: &mut LambGc) {
        let Pattern { inner: pattern, .. } = c;

        let offset = self.block.offset;
        let (first, rest) = pattern.split_first().unwrap();

        let mut jumps = Vec::with_capacity(rest.len() + 1);

        self.compile_pattern_top(first, gc);
        for pat in rest {
            let eop = self.write_jump(Jump::IfTrue);
            jumps.push(eop);
            self.write_op(Op::Pop(NZ_ONE_U16));

            // Just like `if` and `case` all patterns must start at the
            // same stack offset and will leave one expression on the stack
            // which will result in either `true` or `false`
            self.block.offset = offset;
            self.compile_pattern_top(pat, gc);
        }

        for j in jumps {
            self.patch_jump(j);
        }
    }

    fn compile_pattern_top(&mut self, c: &InnerPattern, gc: &mut LambGc) {
        match c {
            InnerPattern::Rest(..) => unimplemented!("This must be handled by the parent pattern"),
            InnerPattern::Literal(lit) => {
                self.write_op(Op::Dup);
                self.compile_literal_pattern(gc, lit);
                self.write_op(Op::Eq);
            }
            InnerPattern::Ident(ref pat) => {
                let i = &pat.ident.raw;
                let pat = &pat.bound;
                // The ident must have been declared by `compile_case`.
                let slot = self.local_slot(i.as_str()).unwrap();
                let _ = gc.intern(i);
                self.write_op(Op::SetSlot(slot.try_into().unwrap()));

                if let Some(pat) = pat.as_deref() {
                    self.compile_pattern_top(pat, gc);
                } else {
                    self.write_const_op(Value::Bool(true));
                }
            }
            InnerPattern::Array(pat) => {
                let (head, tail, dots) = pat.as_parts();

                // Compare the lengths. Array patterns can only be properly be
                // tested if the lengths proper.
                let min_len = head.len() + tail.len();
                self.write_op(Op::Len);
                self.write_const_op(Value::Int(min_len.try_into().unwrap()));
                self.write_op(if dots.is_some() { Op::Ge } else { Op::Eq });

                let mut ends = Vec::with_capacity(min_len);
                for (idx, pat) in head.iter().enumerate() {
                    ends.push(self.write_jump(Jump::IfFalse));
                    self.write_op(Op::Pop(NZ_ONE_U16));
                    self.write_op(Op::Dup);
                    self.write_const_op(Value::Int(idx.try_into().unwrap()));
                    self.write_op(Op::Index);

                    self.compile_pattern(pat, gc);

                    // Stack is:
                    //   <is_match>
                    //   <local>       <---- Need to remove
                    //   <scrutinee>
                    self.write_op(Op::SaveValue);
                    self.write_op(Op::Pop(NZ_ONE_U16));
                    self.write_op(Op::UnsaveValue);
                }

                // Assign dot locals the proper patterns
                if let Some(rest) = dots {
                    let bindings = rest.binding_names();
                    if !bindings.is_empty() {
                        let start = head.len();
                        let dist_from_end = tail.len();

                        let max = usize::try_from(u32::MAX).unwrap();
                        if start > max || dist_from_end > max {
                            panic!("crap");
                        }

                        let start = (start as u64) << u32::BITS;
                        let dist_from_end = dist_from_end as u64;
                        let repr = (start | dist_from_end) as i64;

                        ends.push(self.write_jump(Jump::IfFalse));
                        self.write_op(Op::Pop(NZ_ONE_U16));

                        self.func.chunk.constants.push(Value::Int(repr));
                        self.write_op(Op::Slice(
                            u16::try_from(self.func.chunk.constants.len() - 1).unwrap(),
                        ));

                        for Ident { raw: bind, .. } in bindings {
                            let slot = self.local_slot(&bind).unwrap();
                            self.write_op(Op::SetSlot(u16::try_from(slot).unwrap()))
                        }

                        self.write_op(Op::Pop(NZ_ONE_U16));
                        self.write_const_op(Value::Bool(true));
                    }
                }

                for (idx, pat) in tail.iter().enumerate() {
                    ends.push(self.write_jump(Jump::IfFalse));

                    self.write_op(Op::Pop(NZ_ONE_U16));
                    self.write_op(Op::Dup);
                    self.write_const_op(Value::Int((tail.len() - 1 - idx).try_into().unwrap()));
                    self.write_op(Op::IndexRev);

                    self.compile_pattern(pat, gc);

                    // Stack is:
                    //   <is_match>
                    //   <local>       <---- Need to remove
                    //   <scrutinee>
                    self.write_op(Op::SaveValue);
                    self.write_op(Op::Pop(NZ_ONE_U16));
                    self.write_op(Op::UnsaveValue);
                }

                for j in ends {
                    self.patch_jump(j);
                }
            }
        }
    }

    fn compile_literal_pattern(&mut self, gc: &mut LambGc, lit: &LiteralPattern) {
        match lit {
            LiteralPattern::String(s) => self.compile_str_literal(gc, s),
            LiteralPattern::Bool(b) => self.compile_bool_literal(b),
            LiteralPattern::Char(c) => self.compile_char_literal(c),
            LiteralPattern::I64(i) => self.compile_i64_literal(i),
            LiteralPattern::Nil(n) => self.compile_nil_literal(n),
        }
    }

    fn compile_str_literal(&mut self, gc: &mut LambGc, lit: &StrLit) {
        let inner = lit.text.as_ref().map_or("", |l| l.inner.as_str());
        self.write_const_op(Value::String(gc.intern(inner)))
    }

    fn compile_bool_literal(&mut self, lit: &BoolLit) {
        self.write_const_op(Value::Bool(lit.value));
    }

    fn compile_char_literal(&mut self, lit: &CharLit) {
        let val = lit.text.as_ref().unwrap().inner.parse().unwrap();
        self.write_const_op(Value::Char(val));
    }

    fn compile_i64_literal(&mut self, lit: &I64Lit) {
        let val = match lit.base {
            I64Base::Bin => i64::from_str_radix(&lit.value, 2),
            I64Base::Oct => i64::from_str_radix(&lit.value, 8),
            I64Base::Dec => i64::from_str_radix(&lit.value, 10),
            I64Base::Hex => i64::from_str_radix(&lit.value, 16),
        }
        .unwrap();

        self.write_const_op(Value::Int(val));
    }

    fn compile_f64_literal(&mut self, lit: &F64Lit) {
        let val = lit.value.parse().unwrap();
        self.write_const_op(Value::Double(val))
    }

    fn compile_nil_literal(&mut self, _nil: &NilLit) {
        self.write_const_op(Value::Nil);
    }

    fn compile_list(&mut self, gc: &mut LambGc, list: &List) {
        let len = list.values.len();
        for e in list.values.iter() {
            self.compile_expr(e, gc);
        }

        self.write_op(Op::MakeArray(len.try_into().unwrap()));
    }

    fn compile_func(&mut self, f: &FnDef, gc: &mut LambGc, name: String) {
        let FnDef {
            args,
            body,
            recursive: is_recursive,
            ..
        } = f;

        let func_name = gc.intern(name.clone());
        let mut func_comp = Compiler::new(func_name, self.module);
        if *is_recursive {
            func_comp.locals[0].name = name;
        }

        let orig_self = std::mem::replace(self, func_comp);
        self.enclosing = Some(Box::new(orig_self));

        for arg in args {
            self.add_arg(gc, arg);
        }

        self.compile_expr(body, gc);
        self.write_op(Op::Return);

        let this = *self.enclosing.take().unwrap();
        let composition = std::mem::replace(self, this);

        self.write_closure(gc, composition.func);
    }

    fn compile_ident(&mut self, Ident { raw: i, .. }: &Ident, gc: &mut LambGc) {
        if let Some(slot) = self.local_slot(i) {
            self.write_op(Op::GetLocal(slot.try_into().unwrap()))
        } else if let Some(slot) = self.upvalue_idx(i) {
            self.write_op(Op::GetUpvalue(slot.try_into().unwrap()))
        } else {
            let str = gc.intern(i);
            self.func.chunk.constants.push(Value::String(str));
            let idx = self.func.chunk.constants.len() - 1;
            self.write_op(Op::GetGlobal(idx.try_into().unwrap()))
        }
    }

    fn compile_return(&mut self, gc: &mut LambGc, ret: &Return) {
        if let Some(value) = &ret.value {
            self.compile_expr(value, gc);
            self.write_op(Op::Return);
        } else {
            self.write_const_op(Value::Nil);
            self.write_op(Op::Return);
        }
    }
}
