use std::num::NonZeroU16;

use lambc_parse::{
    BinaryOp, Block as LambBlock, BoolLit, Case, CaseArm, CharLit, Else, Expr,
    F64Lit, FnDef, I64Base, I64Lit, Ident, If, IfCond, List, NilLit, Return,
    StrLit,
};

use crate::{
    bytecode::NZ_ONE_U16,
    chunk::{Jump, JumpIdx, Op},
    gc::LambGc,
    value::{Str, Value},
};

const SCRUTINEE: &str = " SCRUTINEE";
const COMPOSE_ARG: &str = "COMP ARG";

impl super::Lowerer {
    pub fn lower_expr(&mut self, value: &Expr, gc: &mut LambGc) {
        match value {
            Expr::Unary(un) => {
                self.lower_expr(&un.rhs, gc);
                self.write_op(Op::from(un.op));
            }
            Expr::Binary(bin) => match Op::try_from(bin.op) {
                Ok(op) => {
                    self.lower_expr(&bin.lhs, gc);
                    self.lower_expr(&bin.rhs, gc);
                    self.write_op(op);
                }
                Err(BinaryOp::Land) => {
                    self.lower_sc_op(&bin.lhs, &bin.rhs, Jump::IfFalse, gc)
                }
                Err(BinaryOp::Lor) => {
                    self.lower_sc_op(&bin.lhs, &bin.rhs, Jump::IfTrue, gc)
                }
                Err(BinaryOp::Appl) => {
                    self.lower_apply(&bin.lhs, &bin.rhs, gc)
                }
                Err(BinaryOp::Appr) => {
                    self.lower_apply(&bin.rhs, &bin.lhs, gc)
                }
                Err(BinaryOp::Cpsl) => {
                    self.lower_compose(&bin.lhs, &bin.rhs, gc)
                }
                Err(BinaryOp::Cpsr) => {
                    self.lower_compose(&bin.rhs, &bin.lhs, gc)
                }
                Err(_) => unreachable!("All cases are actually covered!"),
            },
            Expr::Index(i) => {
                self.lower_expr(&i.lhs, gc);
                self.lower_expr(&i.rhs, gc);
                self.write_op(Op::Index);
            }
            Expr::Call(call) => {
                self.lower_expr(&call.callee, gc);
                for arg in &call.args {
                    self.lower_expr(arg, gc);
                }
                self.write_op(Op::Call(call.args.len().try_into().unwrap()))
            }
            Expr::If(if_) => self.lower_if_expr(if_, gc),
            Expr::Case(c) => self.lower_case(c, gc),
            Expr::FnDef(f) => self.lower_func(f, gc, "Anon Func".into()),
            Expr::Block(block) => self.lower_block(block, gc),
            Expr::Ident(i) => self.lower_ident(i, gc),
            Expr::Char(c) => self.lower_char_literal(c),
            Expr::String(s) => self.lower_str_literal(gc, s),
            Expr::Bool(b) => self.lower_bool_literal(b),
            Expr::Nil(n) => self.lower_nil_literal(n),
            Expr::I64(i) => self.lower_i64_literal(i),
            Expr::F64(f) => self.lower_f64_literal(f),
            Expr::List(list) => self.lower_list(gc, list),
            Expr::Group(gr) => self.lower_expr(&gr.value, gc),
            Expr::Return(ret) => self.lower_return(gc, ret),
            Expr::Path(path) => self.lower_path(&path, gc),
        }
    }

    pub fn lower_str_literal(&mut self, gc: &mut LambGc, lit: &StrLit) {
        let inner = lit.text.as_ref().map_or("", |l| l.inner.as_str());
        self.write_const_op(Value::String(gc.intern(inner)))
    }

    pub fn lower_bool_literal(&mut self, lit: &BoolLit) {
        self.write_const_op(Value::Bool(lit.value));
    }

    pub fn lower_char_literal(&mut self, lit: &CharLit) {
        let val = lit.text.as_ref().unwrap().inner.parse().unwrap();
        self.write_const_op(Value::Char(val));
    }

    pub fn lower_i64_literal(&mut self, lit: &I64Lit) {
        let val = match lit.base {
            I64Base::Bin => i64::from_str_radix(&lit.value, 2),
            I64Base::Oct => i64::from_str_radix(&lit.value, 8),
            I64Base::Dec => i64::from_str_radix(&lit.value, 10),
            I64Base::Hex => i64::from_str_radix(&lit.value, 16),
        }
        .unwrap();

        self.write_const_op(Value::Int(val));
    }

    pub fn lower_f64_literal(&mut self, lit: &F64Lit) {
        let val = lit.value.parse().unwrap();
        self.write_const_op(Value::Double(val))
    }

    pub fn lower_nil_literal(&mut self, _nil: &NilLit) {
        self.write_const_op(Value::Nil);
    }

    fn lower_block(&mut self, block: &LambBlock, gc: &mut LambGc) {
        let LambBlock { value, statements: stats, .. } = block;
        self.start_block();

        for stat in stats {
            self.lower_stmt(stat, gc);
        }

        if let Some(value) = value {
            self.lower_expr(value, gc);
        } else {
            self.write_const_op(Value::Nil);
        }

        self.end_block();

        // Every block leaves 1 extra value on the stack for the block above it.
        // Thus, a value is left on the stack for this block, which increases its
        // offset
        self.block_mut().offset += 1;
    }

    pub fn lower_rec_func_def<'ast>(
        &mut self,
        Ident { raw, .. }: &'ast Ident,
        def: &'ast FnDef,
        gc: &mut LambGc,
    ) {
        let ident = gc.intern(raw);
        self.func.chunk.write_val(Value::String(ident));
        let idx = self.func.chunk.constants.len() - 1;

        self.lower_func(def, gc, raw.clone());

        if self.block().depth == 0 {
            self.write_op(Op::DefineGlobal(idx.try_into().unwrap()))
        } else {
            self.add_local(raw.clone());
        }
    }

    fn lower_apply<'ast>(
        &mut self,
        lhs: &'ast Expr,
        rhs: &'ast Expr,
        gc: &mut LambGc,
    ) {
        self.lower_expr(lhs, gc);
        self.lower_expr(rhs, gc);
        self.write_op(Op::Call(1));
    }

    fn lower_compose<'ast>(
        &mut self,
        lhs: &'ast Expr,
        rhs: &'ast Expr,
        gc: &mut LambGc,
    ) {
        let ident = gc.intern("Anon Func");
        let mut composition = Self::new(ident, self.module);
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
        self.lower_expr(lhs, gc);
        self.lower_expr(rhs, gc);
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

    fn lower_sc_op<'ast>(
        &mut self,
        lhs: &'ast Expr,
        rhs: &'ast Expr,
        jump: Jump,
        gc: &mut LambGc,
    ) {
        self.lower_expr(lhs, gc);
        let idx = self.write_jump(jump);
        self.write_op(Op::Pop(NZ_ONE_U16));
        self.lower_expr(rhs, gc);
        self.patch_jump(idx);
    }

    fn lower_path<'ast>(
        &mut self,
        path: &'ast lambc_parse::Path,
        gc: &mut LambGc,
    ) {
        // Module::item1::item2
        // ^^^^^^  ^^^^^  ^^^^^
        //     |   |      |
        //     |   |      +--> ModuleItemRef
        //     |   |
        //     |   +--> ModuleItemRef
        //     |
        //     +--> Expr
        let first = &path.head;
        let rest = &path.tail;
        self.lower_ident(first, gc);

        let mut iter = rest.chunks_exact(2);
        while let Some(
            &[Ident { raw: ref left, .. }, Ident { raw: ref right, .. }],
        ) = iter.next()
        {
            let left = Value::ModulePath(gc.intern(left));
            let right = Value::ModulePath(gc.intern(right));
            self.write_const_op(left);
            self.write_op(Op::Access);
            self.write_const_op(right);
        }

        let path = match iter.remainder() {
            &[Ident { ref raw, .. }] => Value::ModulePath(gc.intern(raw)),
            _ => return,
        };

        self.write_const_op(path);
        self.write_op(Op::Access);
    }

    fn lower_if_expr(&mut self, if_: &If, gc: &mut LambGc) {
        let If { cond: IfCond { cond, body, .. }, elif, els_, .. } = if_;

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

        // Each `lower_conditional` puts the offset at +1 item however
        // if the previous condition wasn't true, it isn't executed. This
        // means that the next if will run as if it was the first.
        let offset = self.block().offset;
        let mut to_elses = Vec::with_capacity(1 + elif.len());
        to_elses.push(self.lower_conditional(cond, body, gc));

        // lower elifs
        for IfCond { cond, body, .. } in elif {
            self.write_op(Op::Pop(NZ_ONE_U16));
            self.block_mut().offset = offset;
            to_elses.push(self.lower_conditional(cond, body, gc));
        }

        // lower else
        self.write_op(Op::Pop(NZ_ONE_U16));
        if let Some(Else { body, .. }) = els_ {
            self.lower_block(body, gc);
        } else {
            self.write_const_op(Value::Nil);
        }

        for jmp in to_elses {
            self.patch_jump(jmp);
        }
    }

    fn lower_conditional<'ast>(
        &mut self,
        cond: &'ast Expr,
        block: &'ast LambBlock,
        gc: &mut LambGc,
    ) -> JumpIdx {
        self.lower_expr(cond, gc);
        let cond_false = self.write_jump(Jump::IfFalse);
        self.write_op(Op::Pop(NZ_ONE_U16));
        self.lower_block(block, gc);
        let past_else = self.write_jump(Jump::Always);
        self.patch_jump(cond_false);
        past_else
    }

    fn lower_case(&mut self, c: &Case, gc: &mut LambGc) {
        let Case { arms, scrutinee, .. } = c;

        self.lower_expr(scrutinee, gc);

        gc.intern(SCRUTINEE);
        self.add_local(SCRUTINEE.to_string());

        let after_case = arms
            .iter()
            .map(|arm| self.lower_case_arm(arm, gc))
            .collect::<Vec<_>>();

        // If no match arms are taken, use `Op::Pop`
        self.write_op(Op::Pop(NZ_ONE_U16));
        self.write_const_op(Value::Nil);

        for jmp in after_case {
            self.patch_jump(jmp);
        }

        assert_eq!(self.locals.pop().unwrap().name, SCRUTINEE);
    }

    fn lower_case_arm(&mut self, c: &CaseArm, gc: &mut LambGc) -> JumpIdx {
        let CaseArm { pattern, body, .. } = c;

        let offset_before_arm = self.block().offset;
        let pre_local_count = self.locals.len();

        let bindings = pattern.binding_names();
        let binding_count = bindings.len();

        for Ident { raw, .. } in bindings {
            gc.intern(raw);

            self.write_const_op(Value::Nil);
            self.func
                .chunk
                .constants
                .push(Value::String(gc.alloc(Str::new(raw))));

            self.add_local(raw.clone());
        }

        self.write_op(Op::GetLocal(
            u16::try_from(self.block().base + offset_before_arm - 1).unwrap(),
        ));

        self.lower_pattern(pattern, gc);

        let offset_match = self.block().offset;
        let if_no_match = self.write_jump(Jump::IfFalse);

        // If match ----->
        // Remove True
        self.write_op(Op::Pop(NZ_ONE_U16));
        self.lower_expr(body, gc);
        self.write_op(Op::SaveValue);

        // Remove scrutinee dup
        // Remove `binding_count` amount of bindings
        // Remove scrutinee
        self.write_op(Op::Pop(
            NonZeroU16::new(u16::try_from(binding_count).unwrap() + 2)
                .unwrap(),
        ));

        self.write_op(Op::UnsaveValue);

        let past_arms = self.write_jump(Jump::Always);
        // <----- If match

        // Only one of these are run, so we want to set the offset
        // to before the "If match" was executed.
        self.block_mut().offset = offset_match;

        // If no match ----->
        self.patch_jump(if_no_match);

        // Remove `false`
        // Remove scrutinee dup
        // Remove `binding_count` amount of bindings
        self.write_op(Op::Pop(
            NonZeroU16::new(u16::try_from(binding_count).unwrap() + 2)
                .unwrap(),
        ));

        self.block_mut().offset = offset_before_arm;
        self.locals.truncate(pre_local_count);
        // <----- If no match

        past_arms
    }

    fn lower_list(&mut self, gc: &mut LambGc, list: &List) {
        let len = list.values.len();
        for e in list.values.iter() {
            self.lower_expr(e, gc);
        }

        self.write_op(Op::MakeArray(len.try_into().unwrap()));
    }

    fn lower_func(&mut self, f: &FnDef, gc: &mut LambGc, name: String) {
        let FnDef { args, body, recursive, .. } = f;

        let func_name = gc.intern(name.clone());
        let mut func_comp = Self::new(func_name, self.module);
        if *recursive {
            func_comp.locals[0].name = name;
        }

        let orig_self = std::mem::replace(self, func_comp);
        self.enclosing = Some(Box::new(orig_self));

        for arg in args {
            self.add_arg(gc, arg);
        }

        self.lower_expr(body, gc);
        self.write_op(Op::Return);

        let this = *self.enclosing.take().unwrap();
        let composition = std::mem::replace(self, this);

        self.write_closure(gc, composition.func);
    }

    fn lower_ident(&mut self, Ident { raw, .. }: &Ident, gc: &mut LambGc) {
        if let Some(slot) = self.local_slot(raw) {
            self.write_op(Op::GetLocal(slot.try_into().unwrap()))
        } else if let Some(slot) = self.upvalue_idx(raw) {
            self.write_op(Op::GetUpvalue(slot.try_into().unwrap()))
        } else {
            let str = gc.intern(raw);
            self.func.chunk.constants.push(Value::String(str));
            let idx = self.func.chunk.constants.len() - 1;
            self.write_op(Op::GetGlobal(idx.try_into().unwrap()))
        }
    }

    fn lower_return(&mut self, gc: &mut LambGc, ret: &Return) {
        if let Some(value) = &ret.value {
            self.lower_expr(value, gc);
            self.write_op(Op::Return);
        } else {
            self.write_const_op(Value::Nil);
            self.write_op(Op::Return);
        }
    }
}
