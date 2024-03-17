use lambc_parse::{Ident, InnerPattern, LiteralPattern, Pattern};

use crate::{
    bytecode::NZ_ONE_U16,
    chunk::{Jump, Op},
    value::Value,
};

impl<'a, 'b> super::Lowerer<'a, 'b> {
    // Note:
    //
    //     The scrutinee is assumed to be sitting on the top off the stack
    //     and is *only* to be removed after the case arm is complete.
    //
    //     This means that any sub-patterns must duplicate it.
    pub fn lower_pattern(&mut self, c: &Pattern) {
        let Pattern { inner, .. } = c;

        let offset = self.block().offset;
        let (first, rest) = inner.split_first().unwrap();

        let mut jumps = Vec::with_capacity(rest.len() + 1);

        self.lower_pattern_top(first);
        for pat in rest {
            let eop = self.write_jump(Jump::IfTrue);
            jumps.push(eop);
            self.write_op(Op::Pop(NZ_ONE_U16));

            // Just like `if` and `case` all patterns must start at the
            // same stack offset and will leave one expression on the stack
            // which will result in either `true` or `false`
            self.block_mut().offset = offset;
            self.lower_pattern_top(pat);
        }

        for j in jumps {
            self.patch_jump(j);
        }
    }

    fn lower_pattern_top(&mut self, c: &InnerPattern) {
        match c {
            InnerPattern::Rest(..) => {
                unimplemented!("This must be handled by the parent pattern")
            }
            InnerPattern::Literal(lit) => {
                self.write_op(Op::Dup);
                self.lower_literal_pattern(lit);
                self.write_op(Op::Eq);
            }
            InnerPattern::Ident(ref pat) => {
                let i = &pat.ident.raw;
                let pat = &pat.bound;
                // The ident must have been declared by `lower_case`.
                let slot = self.info.local_slot(i.as_str()).unwrap();
                let _ = self.state.gc.intern(i);
                self.write_op(Op::SetSlot(slot.try_into().unwrap()));

                if let Some(pat) = pat.as_deref() {
                    self.lower_pattern_top(pat);
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

                    self.lower_pattern(pat);

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

                        self.info.func.chunk.constants.push(Value::Int(repr));
                        self.write_op(Op::Slice(
                            u16::try_from(
                                self.info.func.chunk.constants.len() - 1,
                            )
                            .unwrap(),
                        ));

                        for Ident { raw, .. } in bindings {
                            let slot = self.info.local_slot(&raw).unwrap();
                            self.write_op(Op::SetSlot(
                                u16::try_from(slot).unwrap(),
                            ))
                        }

                        self.write_op(Op::Pop(NZ_ONE_U16));
                        self.write_const_op(Value::Bool(true));
                    }
                }

                for (idx, pat) in tail.iter().enumerate() {
                    ends.push(self.write_jump(Jump::IfFalse));

                    self.write_op(Op::Pop(NZ_ONE_U16));
                    self.write_op(Op::Dup);
                    self.write_const_op(Value::Int(
                        (tail.len() - 1 - idx).try_into().unwrap(),
                    ));
                    self.write_op(Op::IndexRev);

                    self.lower_pattern(pat);

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

    fn lower_literal_pattern(&mut self, lit: &LiteralPattern) {
        match lit {
            LiteralPattern::String(s) => self.lower_str_literal(s),
            LiteralPattern::Bool(b) => self.lower_bool_literal(b),
            LiteralPattern::Char(c) => self.lower_char_literal(c),
            LiteralPattern::I64(i) => self.lower_i64_literal(i),
            LiteralPattern::Nil(n) => self.lower_nil_literal(n),
        }
    }
}
