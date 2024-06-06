use std::num::NonZeroU16;

use lambc_parse::{
    BinaryOp, Block as LambBlock, BoolLit, Case, CaseArm, CharLit, Else, Expr,
    F64Lit, FnDef, I64Lit, Ident, If, IfCond, List, NilLit, Return, StrLit,
};

use super::{Error, FunctionInfo};
use crate::{
    bytecode::NZ_ONE_U16,
    chunk::{Jump, JumpIdx, Op},
    value::{Str, Value},
};

const SCRUTINEE: &str = " SCRUTINEE";
const COMPOSE_ARG: &str = "COMP ARG";

impl<'a, 'b> super::Lowerer<'a, 'b> {
    pub fn lower_expr(&mut self, value: &Expr<Ident>) {
        match value {
            Expr::Unary(un) => {
                self.lower_expr(&un.rhs);
                self.write_op(Op::from(un.op));
            }
            Expr::Binary(bin) => match Op::try_from(bin.op) {
                Ok(op) => {
                    self.lower_expr(&bin.lhs);
                    self.lower_expr(&bin.rhs);
                    self.write_op(op);
                }
                Err(BinaryOp::Land) => {
                    self.lower_sc_op(&bin.lhs, &bin.rhs, Jump::IfFalse)
                }
                Err(BinaryOp::Lor) => {
                    self.lower_sc_op(&bin.lhs, &bin.rhs, Jump::IfTrue)
                }
                Err(BinaryOp::Appl) => self.lower_apply(&bin.lhs, &bin.rhs),
                Err(BinaryOp::Appr) => self.lower_apply(&bin.rhs, &bin.lhs),
                Err(BinaryOp::Cpsl) => self.lower_compose(&bin.lhs, &bin.rhs),
                Err(BinaryOp::Cpsr) => self.lower_compose(&bin.rhs, &bin.lhs),
                Err(_) => unreachable!("All cases are actually covered!"),
            },
            Expr::Index(i) => {
                self.lower_expr(&i.lhs);
                self.lower_expr(&i.rhs);
                self.write_op(Op::Index);
            }
            Expr::Call(call) => {
                self.lower_expr(&call.callee);
                for arg in &call.args {
                    self.lower_expr(arg);
                }

                let arg_count = match u16::try_from(call.args.len()) {
                    Ok(len) => len,
                    Err(..) => {
                        // Fix offset to help prevent further issues
                        self.block_mut().offset -= call.args.len();
                        self.add_err(Error::LimitError {
                            items_name: "arguments",
                            limit: u16::MAX as usize,
                            span: call.span,
                        });

                        0
                    }
                };

                self.write_op(Op::Call(arg_count))
            }
            Expr::If(if_) => self.lower_if_expr(if_),
            Expr::Case(c) => self.lower_case(c),
            Expr::FnDef(f) => self.lower_func(f, "Anon Func".into()),
            Expr::Block(block) => self.lower_block(block),
            Expr::Ident(i) => self.lower_ident(i),
            Expr::Char(c) => self.lower_char_literal(c),
            Expr::String(s) => self.lower_str_literal(s),
            Expr::Bool(b) => self.lower_bool_literal(b),
            Expr::Nil(n) => self.lower_nil_literal(n),
            Expr::I64(i) => self.lower_i64_literal(i),
            Expr::F64(f) => self.lower_f64_literal(f),
            Expr::List(list) => self.lower_list(list),
            Expr::Group(gr) => self.lower_expr(&gr.value),
            Expr::Return(ret) => self.lower_return(ret),
            Expr::Path(path) => self.lower_path(&path),
        }
    }

    pub fn lower_str_literal(&mut self, lit: &StrLit) {
        let inner = lit.text.as_ref().map_or("", |l| l.inner.as_str());
        let inner = self.gc.intern(inner);
        self.write_const_op(Value::String(inner))
    }

    pub fn lower_bool_literal(&mut self, lit: &BoolLit) {
        self.write_const_op(Value::Bool(lit.value));
    }

    pub fn lower_char_literal(&mut self, lit: &CharLit) {
        let val = match lit.text.as_ref().map(|c| c.inner.as_str()) {
            Some(txt) => {
                let mut chs = txt.chars();
                let ch = chs
                    .next()
                    .expect("CharText is only parsed if there is text");

                if chs.next().is_some() {
                    self.add_err(Error::InvalidLiteral {
                        type_: "char",
                        reason: "Char literals may only contain one unicode scalar value",
                        help: Some("If this was meant to be a string, use '\"'".into()),
                        span: lit.span,
                    });
                }

                ch
            }
            None => {
                self.add_err(Error::InvalidLiteral {
                    type_: "char",
                    reason:
                        "Char literals must contain one unicode scalar value",
                    help: None,
                    span: lit.span,
                });

                'f'
            }
        };

        self.write_const_op(Value::Char(val));
    }

    pub fn lower_i64_literal(&mut self, lit: &I64Lit) {
        if lit.value.is_empty() {
            self.add_err(Error::InvalidLiteral {
                type_: "i64",
                reason: "prefixed literal cannot be empty",
                help: None,
                span: lit.span,
            });

            self.write_const_op(Value::Int(0));
            return;
        }

        let slice = {
            let mut clone = lit.value.clone();
            clone.retain(|c| c != '_');
            clone
        };

        let val = match i128::from_str_radix(&slice, lit.base.to_base()) {
            Ok(parsed) => match i64::try_from(parsed) {
                Ok(val) => val,
                Err(_) => {
                    self.add_err(Error::InvalidLiteral {
                        type_: "i64",
                        reason: "literal is out of range for an i64",
                        help: Some(format!(
                            "An i64 literal must be between {} and {}",
                            i64::MIN,
                            i64::MAX
                        )),
                        span: lit.span,
                    });

                    0
                }
            },
            Err(e) => unreachable!(
                "Tokenizer produced an invalid token '{slice}': {e}"
            ),
        };

        self.write_const_op(Value::Int(val));
    }

    pub fn lower_f64_literal(&mut self, lit: &F64Lit) {
        let val = match lit.value.parse() {
            Ok(val) => val,
            Err(er) => unreachable!(
                "Tokenizer produced an invalid f64 token '{}': {er}",
                lit.value
            ),
        };

        self.write_const_op(Value::Double(val))
    }

    pub fn lower_nil_literal(&mut self, _nil: &NilLit) {
        self.write_const_op(Value::Nil);
    }

    fn lower_block(&mut self, block: &LambBlock<Ident>) {
        let LambBlock { value, statements: stats, .. } = block;
        self.start_block();

        for stat in stats {
            self.lower_stmt(stat);
        }

        if let Some(value) = value {
            self.lower_expr(value);
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
        def: &'ast FnDef<Ident>,
    ) {
        let ident = self.gc.intern(raw);
        self.info.func.chunk.write_val(Value::String(ident));
        let idx = self.info.func.chunk.constants.len() - 1;

        self.lower_func(def, raw.clone());

        if self.block().depth == 0 {
            self.write_op(Op::DefineGlobal(idx.try_into().unwrap()))
        } else {
            self.info.add_local(raw.clone());
        }
    }

    fn lower_apply<'ast>(
        &mut self,
        lhs: &'ast Expr<Ident>,
        rhs: &'ast Expr<Ident>,
    ) {
        self.lower_expr(lhs);
        self.lower_expr(rhs);
        self.write_op(Op::Call(1));
    }

    fn lower_compose<'ast>(
        &mut self,
        lhs: &'ast Expr<Ident>,
        rhs: &'ast Expr<Ident>,
    ) {
        let ident = self.gc.intern("Anon Func");
        let mut composition = FunctionInfo::new(ident, self.module);
        composition.locals[0].depth = 1;

        // `self` now refers to `composition`. This is done because I
        // can't figure out how to make composition have a reference to self :D
        let orig_self = std::mem::replace(&mut self.info, composition);

        self.info.enclosing = Some(Box::new(orig_self));
        self.add_arg(&Ident {
            raw: COMPOSE_ARG.into(),
            // TODO: Remove this span!
            span: lhs.span(),
        });
        self.start_block();
        self.lower_expr(lhs);
        self.lower_expr(rhs);
        // It's clearly at spot 1... soo.... cheat!
        self.write_op(Op::GetLocal(1));
        self.write_op(Op::Call(1));
        self.write_op(Op::Call(1));
        self.write_op(Op::Return);
        self.end_block();

        // `self` now refers to the original `self`.
        let this = *self.info.enclosing.take().unwrap();
        let composition = std::mem::replace(&mut self.info, this);

        self.write_closure(composition.func);
    }

    fn lower_sc_op<'ast>(
        &mut self,
        lhs: &'ast Expr<Ident>,
        rhs: &'ast Expr<Ident>,
        jump: Jump,
    ) {
        self.lower_expr(lhs);
        let idx = self.write_jump(jump);
        self.write_op(Op::Pop(NZ_ONE_U16));
        self.lower_expr(rhs);
        self.patch_jump(idx);
    }

    fn lower_path<'ast>(&mut self, path: &'ast lambc_parse::Path) {
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
        self.lower_ident(first);

        let mut iter = rest.chunks_exact(2);
        while let Some(
            &[Ident { raw: ref left, .. }, Ident { raw: ref right, .. }],
        ) = iter.next()
        {
            let left = Value::ModulePath(self.gc.intern(left));
            let right = Value::ModulePath(self.gc.intern(right));
            self.write_const_op(left);
            self.write_op(Op::Access);
            self.write_const_op(right);
        }

        let path = match iter.remainder() {
            &[Ident { ref raw, .. }] => Value::ModulePath(self.gc.intern(raw)),
            _ => return,
        };

        self.write_const_op(path);
        self.write_op(Op::Access);
    }

    fn lower_if_expr(&mut self, if_: &If<Ident>) {
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
        to_elses.push(self.lower_conditional(cond, body));

        // lower elifs
        for IfCond { cond, body, .. } in elif {
            self.write_op(Op::Pop(NZ_ONE_U16));
            self.block_mut().offset = offset;
            to_elses.push(self.lower_conditional(cond, body));
        }

        // lower else
        self.write_op(Op::Pop(NZ_ONE_U16));
        if let Some(Else { body, .. }) = els_ {
            self.lower_block(body);
        } else {
            self.write_const_op(Value::Nil);
        }

        for jmp in to_elses {
            self.patch_jump(jmp);
        }
    }

    fn lower_conditional<'ast>(
        &mut self,
        cond: &'ast Expr<Ident>,
        block: &'ast LambBlock<Ident>,
    ) -> JumpIdx {
        self.lower_expr(cond);
        let cond_false = self.write_jump(Jump::IfFalse);
        self.write_op(Op::Pop(NZ_ONE_U16));
        self.lower_block(block);
        let past_else = self.write_jump(Jump::Always);
        self.patch_jump(cond_false);
        past_else
    }

    fn lower_case(&mut self, c: &Case<Ident>) {
        let Case { arms, scrutinee, .. } = c;

        self.lower_expr(scrutinee);

        self.gc.intern(SCRUTINEE);
        self.info.add_local(SCRUTINEE.to_string());

        let after_case = arms
            .iter()
            .map(|arm| self.lower_case_arm(arm))
            .collect::<Vec<_>>();

        // If no match arms are taken, use `Op::Pop`
        self.write_op(Op::Pop(NZ_ONE_U16));
        self.write_const_op(Value::Nil);

        for jmp in after_case {
            self.patch_jump(jmp);
        }

        assert_eq!(self.info.locals.pop().unwrap().name, SCRUTINEE);
    }

    fn lower_case_arm(&mut self, c: &CaseArm<Ident>) -> JumpIdx {
        let CaseArm { pattern, body, .. } = c;

        let offset_before_arm = self.block().offset;
        let pre_local_count = self.info.locals.len();

        let bindings = pattern.binding_names();
        let binding_count = bindings.len();

        for Ident { raw, .. } in bindings {
            self.gc.intern(raw);

            self.write_const_op(Value::Nil);
            self.info
                .func
                .chunk
                .constants
                .push(Value::String(self.gc.alloc(Str::new(raw))));

            self.info.add_local(raw.clone());
        }

        self.write_op(Op::GetLocal(
            u16::try_from(self.block().base + offset_before_arm - 1).unwrap(),
        ));

        self.lower_pattern(pattern);

        let offset_match = self.block().offset;
        let if_no_match = self.write_jump(Jump::IfFalse);

        // If match ----->
        // Remove True
        self.write_op(Op::Pop(NZ_ONE_U16));
        self.lower_expr(body);
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
        self.info.locals.truncate(pre_local_count);
        // <----- If no match

        past_arms
    }

    fn lower_list(&mut self, list: &List<Ident>) {
        for e in list.values.iter() {
            self.lower_expr(e);
        }

        let len = match u16::try_from(list.values.len()) {
            Ok(len) => len,
            Err(..) => {
                // Fix the stack offset and add an error
                self.block_mut().offset -= list.values.len();
                self.add_err(Error::ArrayLimitError {
                    limit: u16::MAX as usize,
                    total: list.values.len(),
                    span: list.span,
                });

                0
            }
        };

        self.write_op(Op::MakeArray(len));
    }

    fn lower_func(&mut self, f: &FnDef<Ident>, name: String) {
        let FnDef { args, body, recursive, .. } = f;

        let func_name = self.gc.intern(name.clone());
        let mut func_comp = FunctionInfo::new(func_name, self.module);
        if *recursive {
            func_comp.locals[0].name = name;
        }

        let orig_self = std::mem::replace(&mut self.info, func_comp);
        self.info.enclosing = Some(Box::new(orig_self));

        for arg in args {
            self.add_arg(arg);
        }

        self.lower_expr(body);
        self.write_op(Op::Return);

        let this = *self.info.enclosing.take().unwrap();
        let composition = std::mem::replace(&mut self.info, this);

        self.write_closure(composition.func);
    }

    fn lower_ident(&mut self, Ident { raw, .. }: &Ident) {
        if let Some(slot) = self.info.local_slot(raw) {
            self.write_op(Op::GetLocal(slot.try_into().unwrap()))
        } else if let Some(slot) = self.info.upvalue_idx(raw) {
            self.write_op(Op::GetUpvalue(slot.try_into().unwrap()))
        } else {
            let str = self.gc.intern(raw);
            self.info.func.chunk.constants.push(Value::String(str));
            let idx = self.info.func.chunk.constants.len() - 1;
            self.write_op(Op::GetGlobal(idx.try_into().unwrap()))
        }
    }

    fn lower_return(&mut self, ret: &Return<Ident>) {
        if let Some(value) = &ret.value {
            self.lower_expr(value);
            self.write_op(Op::Return);
        } else {
            self.write_const_op(Value::Nil);
            self.write_op(Op::Return);
        }
    }
}
