#include <cstdlib>
#include <iostream>

#include "../ast/ast.hpp"
#include "../types.hpp"
#include "ast.hpp"
#include "chunk.hpp"
#include "compiler.hpp"
#include "object.hpp"
#include "value.hpp"

#define ANON_FUNC_NAME "anonymous"
#define STACK_DIFF(compiler, diff) ((compiler)->block->offset += (diff))
#define BUBBLE(x)                                                                                  \
    if ((x) == CarUnsupportedAst) {                                                                \
        return CarUnsupportedAst;                                                                  \
    }

namespace {

// The whitespace is important here as it prevents the user from being able to
// use the name
constexpr auto SCRUTINEE_NAME = " scrutinee";

bool is_stmt(AstNodeType type) {
    switch (type) {
        case AstntReturn:
        case AstntExprStmt:
        case AstntAssignStmt:
            return true;
        default:
            return false;
    }
}

Compiler new_function_compiler(Vm &vm, Compiler *const compiler, Block *const block,
                               char const *name) {
    Compiler func_comp(vm, compiler, block, FtNormal, name, 0);
    func_comp.new_scope();

    return func_comp;
}

void add_arg_to_compiler(Vm &vm, Compiler *const func_comp, AstNode const *arg) {
    AstNode *ident_node = arg->kids[0];
    auto *ident = LambString::from_cstr(vm, ident_node->val.i);
    func_comp->chunk().add_const(vm, Value::from_obj((Object *)ident));
    func_comp->add_local(vm, ident->chars);
    func_comp->function->arity++;
}

void write_as_closure(Vm &vm, Compiler *const func_comp) {
    Compiler *compiler = func_comp->enclosing;
    compiler->write_op(vm, OpClosure);
    compiler->write_op_arg(vm, Value::from_obj((Object *)func_comp->function));

    // TODO: Can this be turned into a range based loop?
    for (i32 i = 0; i < func_comp->function->upvalue_count; i++) {
        // NOLINTBEGIN(cppcoreguidelines-pro-bounds-constant-array-index)
        compiler->write_byte(vm, static_cast<u8>(func_comp->upvalues[i].is_local));
        compiler->write_byte(vm, func_comp->upvalues[i].index);
        // NOLINTEND(cppcoreguidelines-pro-bounds-constant-array-index)
    }

    func_comp->end_scope(vm);
    func_comp->destroy(vm);
    vm.curr_compiler = func_comp->enclosing;
}

// NOLINTNEXTLINE(misc-no-recursion)
CompileAstResult compile_function(Vm &vm, Compiler *compiler, AstNode *node, char const *name) {
    AstNode *func_args = node->kids[0];
    AstNode *func_body = node->kids[1];
    AstNode *is_recursive = node->kids[2];

    Block block = {
        .prev = nullptr,
        .base = 0,
        .offset = 0,
        .depth = 0,
    };

    Compiler func_comp = new_function_compiler(vm, compiler, &block, name);
    vm.curr_compiler = &func_comp;

    // The first local in a function is actually either a nameless value,
    // or the function itself, and in order for it to be accessible the
    // depth must match.
    func_comp.locals[0].depth = func_comp.block->depth;
    block.offset += 1;

    // In a recursive function the starting value must be findable via name
    if (is_recursive->val.b) {
        func_comp.locals[0].name = name;
    }

    for (AstNode *arg = func_args; arg != nullptr; arg = arg->kids[1]) {
        add_arg_to_compiler(vm, &func_comp, arg);
        block.offset += 1;
    }

    BUBBLE(compile(vm, &func_comp, func_body));
    func_comp.write_op(vm, OpReturn);

    if (vm.options.print_fn_chunks) {
        std::cerr << func_comp.chunk().to_string() << '\n';
    }

    write_as_closure(vm, &func_comp);

    return CarOk;
}

// NOLINTNEXTLINE(misc-no-recursion)
CompileAstResult compile_rec_func_def(Vm &vm, Compiler *compiler, AstNode *node) {
    AstNode *ident = node->kids[0];
    AstNode *func_def = node->kids[1];

    auto *rec_func_ident = LambString::from_cstr(vm, ident->val.i);
    compiler->chunk().add_const(vm, Value::from_obj((Object *)rec_func_ident));

    BUBBLE(compile_function(vm, compiler, func_def, rec_func_ident->chars));

    if (compiler->block->depth == 0) {
        compiler->write_op(vm, OpDefineGlobal);
        compiler->write_op_arg(vm, Value::from_obj((Object *)rec_func_ident));
    } else {
        compiler->add_local(vm, rec_func_ident->chars);
    }

    return CarOk;
}

// NOLINTNEXTLINE(misc-no-recursion)
CompileAstResult compile_compose(Vm &vm, Compiler *compiler, AstNode *first, AstNode *second) {
    // Turns: `f1 .> f2` or `f2 <. f1` into:
    // result := fn(x) -> f2(f1(x));
    Block block = {
        .prev = compiler->block,
        .base = 1,
        .offset = 0,
        .depth = 0,
    };

    Compiler func_comp(vm, compiler, &block, FtNormal, ANON_FUNC_NAME, 1);

    // The first local in a function is actually either a nameless value,
    // or the function itself, and in order for it to be accessible the
    // depth must match.
    func_comp.locals[0].depth = 1;
    block.offset += 1;

    func_comp.new_scope();
    func_comp.enclosing = compiler;
    vm.curr_compiler = &func_comp;

    BUBBLE(compile(vm, &func_comp, first));
    BUBBLE(compile(vm, &func_comp, second));

    // This gets the first variable off of the stack. Remember:
    // * The variable at position 0 is the function itself
    // * Position of the first arg is at index 1
    func_comp.write_op(vm, OpGetLocal);
    func_comp.write_const(vm, Value::from_i64(1));

    func_comp.write_vop(vm, OpCall, 1);
    func_comp.write_op_arg(vm, Value::from_i64(1));

    func_comp.write_vop(vm, OpCall, 1);
    func_comp.write_op_arg(vm, Value::from_i64(1));

    func_comp.write_op(vm, OpReturn);

    if (vm.options.print_fn_chunks) {
        std::cerr << func_comp.chunk().to_string() << '\n';
    }

    write_as_closure(vm, &func_comp);

    return CarOk;
}
} // namespace

// NOLINTNEXTLINE(misc-no-recursion,readability-function-cognitive-complexity)
CompileAstResult compile(Vm &vm, Compiler *compiler, AstNode *node) {

#define CONSTANT(val)                                                                              \
    /* NOLINTBEGIN(cppcoreguidelines-avoid-do-while) */                                            \
    do {                                                                                           \
        compiler->write_const(vm, (val));                                                          \
    } while (false) /* NOLINTEND(cppcoreguidelines-avoid-do-while) */

#define UNARY(op)                                                                                  \
    /* NOLINTBEGIN(cppcoreguidelines-avoid-do-while) */                                            \
    do {                                                                                           \
        BUBBLE(compile(vm, compiler, node->kids[0]));                                              \
        compiler->write_op(vm, (op));                                                              \
    } while (false) /* NOLINTEND(cppcoreguidelines-avoid-do-while) */

#define BINARY(op)                                                                                 \
    /* NOLINTBEGIN(cppcoreguidelines-avoid-do-while) */                                            \
    do {                                                                                           \
        BUBBLE(compile(vm, compiler, node->kids[0]));                                              \
        BUBBLE(compile(vm, compiler, node->kids[1]));                                              \
        compiler->write_op(vm, op);                                                                \
    } while (false) /* NOLINTEND(cppcoreguidelines-avoid-do-while) */

    switch (node->type) {
        // Simple Constants
        case AstntNilLit:
            CONSTANT(Value::nil());
            break;
        case AstntNumLit:
            CONSTANT(Value::from_i64(node->val.n));
            break;
        case AstntCharLit:
            CONSTANT(Value::from_char(node->val.c));
            break;
        case AstntBoolLit:
            CONSTANT(Value::from_bool(node->val.b));
            break;

        // Complex Constants
        case AstntIdent: {
            auto *ident = LambString::from_cstr(vm, node->val.i);

            auto local_slot = compiler->local_slot(ident);
            if (local_slot) {
                auto slot = local_slot.value();
                compiler->write_op(vm, OpGetLocal);
                compiler->write_op_arg(vm, Value::from_i64(slot));
            } else {
                auto upvalue_slot = compiler->upvalue_idx(ident);
                if (!upvalue_slot) {
                    compiler->write_op(vm, OpGetGlobal);
                    compiler->write_op_arg(vm, Value::from_obj((Object *)ident));
                } else {
                    compiler->write_op(vm, OpGetUpvalue);
                    compiler->write_op_arg(vm, Value::from_i64(upvalue_slot.value()));
                }
            }

            break;
        }

        // Complex constants
        case AstntStrLit: {
            auto *lit = LambString::from_cstr(vm, node->val.s);
            compiler->write_const(vm, Value::from_obj((Object *)lit));
            break;
        }

        // Simple Unary
        case AstntUnaryNeg:
            UNARY(OpNumNeg);
            break;
        case AstntUnaryLogNot:
            UNARY(OpLogNeg);
            break;
        case AstntUnaryBitNot:
            UNARY(OpBinNeg);
            break;

        // Simple Binary
        case AstntBinaryAdd:
            BINARY(OpAdd);
            break;
        case AstntBinarySub:
            BINARY(OpSub);
            break;
        case AstntBinaryMul:
            BINARY(OpMul);
            break;
        case AstntBinaryDiv:
            BINARY(OpDiv);
            break;
        case AstntBinaryMod:
            BINARY(OpMod);
            break;
        case AstntBinaryEq:
            BINARY(OpEq);
            break;
        case AstntBinaryNe:
            BINARY(OpNe);
            break;
        case AstntBinaryGt:
            BINARY(OpGt);
            break;
        case AstntBinaryGe:
            BINARY(OpGe);
            break;
        case AstntBinaryLt:
            BINARY(OpLt);
            break;
        case AstntBinaryLe:
            BINARY(OpLe);
            break;
        case AstntBinaryOr:
            BINARY(OpBinOr);
            break;
        case AstntBinaryXor:
            BINARY(OpBinXor);
            break;
        case AstntBinaryAnd:
            BINARY(OpBinAnd);
            break;
        case AstntBinaryRShift:
            BINARY(OpRShift);
            break;
        case AstntBinaryLShift:
            BINARY(OpLShift);
            break;

        case AstntBinaryLogAnd: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            i32 if_false = compiler->write_jump(vm, OpJumpIfFalse);
            compiler->write_op(vm, OpPop);
            BUBBLE(compile(vm, compiler, node->kids[1]));
            compiler->patch_jump(if_false);
            break;
        }
        case AstntBinaryLogOr: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            i32 if_true = compiler->write_jump(vm, OpJumpIfTrue);
            compiler->write_op(vm, OpPop);
            BUBBLE(compile(vm, compiler, node->kids[1]));
            compiler->patch_jump(if_true);
            break;
        }
        case AstntBinaryLCompose: {
            // The two items are compiled in a separate block and therefor
            // are not on the stack at this point. The result is that there
            // are that both components are evaluated only when called.
            AstNode *lhs = node->kids[0];
            AstNode *rhs = node->kids[1];
            compile_compose(vm, compiler, lhs, rhs);
            break;
        }
        case AstntBinaryRCompose: {
            // The two items are compiled in a separate block and therefor
            // are not on the stack at this point. The result is that there
            // are that both components are evaluated only when called.
            AstNode *lhs = node->kids[0];
            AstNode *rhs = node->kids[1];
            compile_compose(vm, compiler, rhs, lhs);
            break;
        }
        case AstntBinaryLApply: {
            AstNode *lhs = node->kids[0];
            AstNode *rhs = node->kids[1];

            BUBBLE(compile(vm, compiler, lhs));
            BUBBLE(compile(vm, compiler, rhs));

            compiler->write_vop(vm, OpCall, 1);
            compiler->write_op_arg(vm, Value::from_i64(1));
            break;
        }
        case AstntBinaryRApply: {
            AstNode *lhs = node->kids[0];
            AstNode *rhs = node->kids[1];

            BUBBLE(compile(vm, compiler, rhs));
            BUBBLE(compile(vm, compiler, lhs));

            compiler->write_vop(vm, OpCall, 1);
            compiler->write_op_arg(vm, Value::from_i64(1));
            break;
        }
        case AstntFuncDef: {
            BUBBLE(compile_function(vm, compiler, node, ANON_FUNC_NAME));
            break;
        }
        case AstntFuncCall: {
            AstNode *callee = node->kids[0];
            BUBBLE(compile(vm, compiler, callee));

            i32 arg_count = 0;
            for (AstNode *arg_list = node->kids[1]; arg_list != nullptr;
                 arg_list = arg_list->kids[1]) {
                BUBBLE(compile(vm, compiler, arg_list->kids[0]));
                arg_count += 1;
            }

            compiler->write_vop(vm, OpCall, arg_count);
            compiler->write_op_arg(vm, Value::from_i64((i64)arg_count));
            break;
        }
        case AstntReturn: {
            AstNode *val = node->kids[0];
            if (val == nullptr) {
                compiler->write_const(vm, Value::nil());
            } else {
                BUBBLE(compile(vm, compiler, val));
            }
            compiler->write_op(vm, OpReturn);
            break;
        }
        case AstntArray: {
            // Reverse the linked list
            AstNode *prev = nullptr;
            AstNode *curr = node->kids[0];
            if (curr == nullptr) {
                compiler->write_vop(vm, OpMakeArray, 0);
                compiler->write_const(vm, Value::from_i64(0));
                return CarOk;
            }

            AstNode *next = curr->kids[1];
            while (curr != nullptr) {
                curr->kids[1] = prev;
                prev = curr;
                curr = next;

                if (next != nullptr) {
                    next = next->kids[1];
                }
            }

            node->kids[0] = prev;

            i32 len = 0;
            for (AstNode *expr_list = node->kids[0]; expr_list != nullptr;
                 expr_list = expr_list->kids[1]) {
                BUBBLE(compile(vm, compiler, expr_list->kids[0]));
                len += 1;
            }

            compiler->write_vop(vm, OpMakeArray, len);
            compiler->write_const(vm, Value::from_i64(len));
            break;
        }
        case AstntArrayIndex: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            BUBBLE(compile(vm, compiler, node->kids[1]));
            compiler->write_op(vm, OpIndex);
            break;
        }
        case AstntIf: {
            // During parsing the following transformation happens:
            //
            // if x {} elif y {} elif z {} else {}
            //
            // if x {} else { if y { } else { if z { } else { }}}
            //
            // All nodes in this chain know where the else ends, and can thus jump past
            // it
            //
            //
            // Asm-Lite:
            //
            //  if:
            //    <expr>
            //    jumpiffalse .next_branch_1
            //    <body>
            //    jump .end_of_if
            //  next_branch_1:
            //    <expr_elif_1>
            //    jumpiffalse .next_branch_2
            //    <body_elif_1>
            //
            //  ; repeat elifs
            //
            //  next_branch_n:
            //    <expr_elif_n>
            //    jumpiffalse .else
            //    <body_elif_n>
            //  else:
            //    <body>
            //  end_of_if:
            //    <rest>

            // <expr>
            // jumpiffalse .next_branch_1
            // <body>
            // jump .end_of_if

            i32 offset_before_branch = STACK_DIFF(compiler, 0);
            BUBBLE(compile(vm, compiler, node->kids[0]));

            i32 if_false_jump = compiler->write_jump(vm, OpJumpIfFalse);
            compiler->write_op(vm, OpPop);

            BUBBLE(compile(vm, compiler, node->kids[1]));

            i32 past_else = compiler->write_jump(vm, OpJump);
            compiler->patch_jump(if_false_jump);
            compiler->write_op(vm, OpPop);

            compiler->block->offset = offset_before_branch;
            if (node->kids[2] != nullptr) {
                BUBBLE(compile(vm, compiler, node->kids[2]));
            } else {
                compiler->write_const(vm, Value::nil());
            }

            compiler->patch_jump(past_else);
            break;
        }
        case AstntCase: {
            BUBBLE(compile(vm, compiler, node->kids[0]));

            // Add case scrutinee as a local in the compiler so that all locals
            // created by patterns are correctly offset.
            // Whitespace is important because users can't have identifiers with
            // whitespace, but we can ;)
            i32 pre_case_local_count = compiler->locals.len();

            auto *ident = LambString::from_cstr(vm, SCRUTINEE_NAME);
            compiler->add_local(vm, ident->chars);

            if (node->kids[1] != nullptr) {
                BUBBLE(compile(vm, compiler, node->kids[1]));
            } else {
                compiler->write_op(vm, OpPop);
                compiler->write_const(vm, Value::nil());
            }

            // Very complicated way off poppping " scrutinee"
            compiler->locals.truncate(pre_case_local_count);
            break;
        }
        case AstntCaseArm: {
            AstNode *pattern = node->kids[0];
            AstNode *body = node->kids[1];
            AstNode *next_arm = node->kids[2];

            i32 offset_before_arm = STACK_DIFF(compiler, 0);
            i32 local_count_pre_pattern = compiler->locals.len();

            i32 binding_count = 0;
            for (auto *names = pattern->kids[1]; names != nullptr; names = names->kids[1]) {
                auto *ident_ = names->kids[0]->val.i;
                auto *ident = LambString::from_cstr(vm, ident_);

                // Provide a default Value nil, which will be overriden by binding
                // patterns
                compiler->write_const(vm, Value::nil());
                compiler->chunk().add_const(vm, Value::from_obj((Object *)ident));
                compiler->add_local(vm, ident->chars);

                binding_count++;
            }

            const auto *scrutinee = LambString::from_cstr(vm, SCRUTINEE_NAME);
            auto scrutinee_slot = compiler->local_slot(scrutinee).value();
            compiler->write_op(vm, OpGetLocal);
            compiler->write_op_arg(vm, Value::from_i64(scrutinee_slot));

            // Compare the value of the arm with a duplicate of the case value
            BUBBLE(compile(vm, compiler, pattern));

            i32 if_neq = compiler->write_jump(vm, OpJumpIfFalse);

            // If equal -------->
            // Pop the case 'true' off of the stack
            compiler->write_op(vm, OpPop);

            // Run through the body of the arm
            BUBBLE(compile(vm, compiler, body));

            // Pop off `scrutinee` after going through the branch
            compiler->write_op(vm, OpSaveValue);
            compiler->write_op(vm, OpPop);
            for (int i = 0; i < binding_count; ++i) {
                compiler->write_op(vm, OpPop);
            }
            compiler->write_op(vm, OpPop);
            compiler->write_op(vm, OpUnsaveValue);

            // Jump over the other arms of the case expression
            i32 past_else = compiler->write_jump(vm, OpJump);
            // If equal <-------

            // If not equal -------->
            compiler->patch_jump(if_neq);
            // Pop the 'false' from the previous EQ check off of the stack
            compiler->write_op(vm, OpPop);

            // Pop duplicated compare off of the stack
            compiler->write_op(vm, OpPop);

            for (int i = 0; i < binding_count; ++i) {
                compiler->write_op(vm, OpPop);
            }

            // When attempting another arm, the offset for this arm should
            // be the same as the offset for the arm before, as testing an
            // arm shouldn't effect the stack difference.
            compiler->block->offset = offset_before_arm;

            compiler->locals.truncate(local_count_pre_pattern);
            if (next_arm != nullptr) {
                // Attempt the next arm
                BUBBLE(compile(vm, compiler, next_arm));
            } else {
                // Pop the compare value off of the stack
                compiler->write_op(vm, OpPop);

                // We can't check for exhaustivity at compile time, so we write
                // a default nil in the event none of arms matched successfully
                compiler->write_const(vm, Value::nil());
            }
            // If not equal <-------

            // If successfull, we can jump over all of the arms thanks to the
            // recursive nature of the case arms representation
            compiler->patch_jump(past_else);
            break;
        }
        case AstntPattern: {
            // Note:
            //
            //     The scrutinee is assumed to be sitting on the top off the stack
            //     and is *only* to be removed after the case arm is complete.
            //
            //     This means that any sub-patterns must duplicate it.
            //
            // ASM-lite:
            //
            //     pattern:
            //        test <sub_pat_0> <scrutinee>

            //        jumpiftrue .end_of_pattern
            //        pop                         ; pops false off the stack
            //        test <sub_pat_1> <scrutinee>
            //        ..
            //        jumpiftrue .end_of_pattern
            //        pop                         ; pops false off the stack
            //        test <sub_pat_N> <scrutinee>
            //
            //    end_of_pattern:
            //        ; Nothing
            //
            auto *pattern_list = node->kids[0];
            auto *next = pattern_list->kids[1];

            i32 offset_before_pattern = STACK_DIFF(compiler, 0);
            BUBBLE(compile(vm, compiler, pattern_list->kids[0]));

            std::vector<i32> jmps{};

            while (next != nullptr && next->kids[0] != nullptr) {
                i32 eop = compiler->write_jump(vm, OpJumpIfTrue);
                jmps.push_back(eop);

                // pop `false` off the stack
                compiler->write_op(vm, OpPop);

                // Just like `if` and `case` all patterns must start at the
                // same stack offset and will leave one expression on the stack
                // which will result in either `true` or `false`
                compiler->block->offset = offset_before_pattern;
                BUBBLE(compile(vm, compiler, next->kids[0]));

                next = next->kids[1];
            }

            for (auto j : jmps) {
                compiler->patch_jump(j);
            }

            break;
        }
        case AstntPatternTopLit: {
            // Note:
            //
            //     The scrutinee is assumed to be sitting on the top off the stack
            //     and is *only* to be removed after the case arm is complete.
            //
            // ASM-lite:
            //
            //     dup       ; Duplicates top of stack
            //     <lit>
            //     eq
            //
            auto *lit = node->kids[0];
            compiler->write_op(vm, OpDup);
            BUBBLE(compile(vm, compiler, lit));
            compiler->write_op(vm, OpEq);
            break;
        }
        case AstntPatternTopIdent: {
            auto *ident_ = node->kids[0]->val.i;
            auto *ident = LambString::from_cstr(vm, ident_);
            auto *pattern = node->kids[1];

            // Set the pattern value
            i32 slot = compiler->local_slot(ident).value();

            // +1 is because there is a
            compiler->write_op(vm, OpSetSlot);
            compiler->write_op_arg(vm, Value::from_i64(slot));

            if (pattern == nullptr) {
                compiler->write_const(vm, Value::from_bool(true));
            } else {
                BUBBLE(compile(vm, compiler, pattern));
            }

            break;
        }
        case AstntPatternTopArray: {
            // Note:
            //
            //     The scrutinee is assumed to be sitting on the top off the stack
            //     and is *NOT* to be consumed by this pattern.
            //
            //     This pattern is also usable for strings because it doesn't type-check
            //     the value itself, but only the inner elements. This does mean that
            //     doing a nested array check will fail because `OpLen` checks that the
            //     value is either an array or a string.
            //
            // ASM-lite:
            //   arr_start:
            //     len                          ; push length of arr onto stack
            //     push min_len
            //     if has_dots { OpGe } else { OpEq }
            //
            //     jumpiffalse .cleanup
            //     pop                          ; pop `false` off the stack
            //     test <left_0> <arr_0>
            //     ..
            //     jumpiffalse .cleanup
            //     pop                          ; pop `false` off the stack
            //     test <left_n> <arr_n>
            //
            //     jumpiffalse .cleanup
            //     pop                          ; pop `false` off the stack
            //     test <right_0> <arr_(len - 1)>
            //     ..
            //     jumpiffalse .cleanup
            //     pop                         ; pop `false` off the stack
            //     test <right_0 + r_len> <arr_(len - 1 - r_len)>
            //
            //     jumpiftrue .arr_end
            //
            //   cleanup:
            //     save                        ; save result
            //     pop                         ; pop scrutinee off the stack
            //     unsave                      ; unsave result
            //
            //   arr_end:
            //     <rest_of_program>
            auto *head_list = node->kids[0];
            auto *tail_list = node->kids[1];

            auto *ext = node->kids[2];
            auto head_len = ext->kids[0]->val.n;
            auto tail_len = ext->kids[1]->val.n;
            auto has_dots = ext->kids[2]->val.b;

            auto min_len = head_len + tail_len;

            // Verify the length matches:
            compiler->write_op(vm, OpLen);
            compiler->write_const(vm, Value::from_i64(min_len));
            compiler->write_op(vm, has_dots ? OpGe : OpEq);

            std::vector<i32> ends{};

            for (i32 i = 0; i < head_len; ++i) {
                // Get the next head pattern
                ends.push_back(compiler->write_jump(vm, OpJumpIfFalse));
                compiler->write_op(vm, OpPop);

                // Index into scrutinee at position `i`
                compiler->write_op(vm, OpDup);
                compiler->write_const(vm, Value::from_i64(i));
                compiler->write_op(vm, OpIndex);

                auto *pattern = head_list->kids[0];
                head_list = head_list->kids[1];

                // Test pattern against item at position `i`
                BUBBLE(compile(vm, compiler, pattern));

                // Currently we have:
                // <scrutinee> <local> <is_match> and we need <scutinee> <is_match>
                compiler->write_op(vm, OpSaveValue);
                compiler->write_op(vm, OpPop);
                compiler->write_op(vm, OpUnsaveValue);
            }

            for (i32 i = 0; i < tail_len; ++i) {
                // Get the next tail pattern
                ends.push_back(compiler->write_jump(vm, OpJumpIfFalse));
                compiler->write_op(vm, OpPop);

                // Index into scrutinee at position `length - 1 - i`
                compiler->write_op(vm, OpDup);
                compiler->write_const(vm, Value::from_i64(tail_len - 1 - i));
                compiler->write_op(vm, OpIndexRev);

                auto *pattern = tail_list->kids[0];
                tail_list = tail_list->kids[1];

                // Test pattern against item at position `length - 1 - i`
                BUBBLE(compile(vm, compiler, pattern));

                // Currently we have:
                // <scrutinee> <item i> <is_match> and we need <scutinee> <is_match>
                compiler->write_op(vm, OpSaveValue);
                compiler->write_op(vm, OpPop);
                compiler->write_op(vm, OpUnsaveValue);
            }

            for (auto jmp : ends) {
                compiler->patch_jump(jmp);
            }

            break;
        }
        case AstntBlock: {
            Block *prev = compiler->block;
            Block block = {
                .prev = prev,
                .base = prev->base + prev->offset,
                .offset = 0,
                .depth = compiler->block->depth,
            };
            compiler->block = &block;

            compiler->new_scope();
            AstNode *stmt = node->kids[0];
            for (; stmt != nullptr && stmt->type == AstntNodeList && is_stmt(stmt->kids[0]->type);
                 stmt = stmt->kids[1]) {
                BUBBLE(compile(vm, compiler, stmt->kids[0]));
            }

            if (stmt != nullptr) {
                BUBBLE(compile(vm, compiler, stmt));
            } else {
                compiler->write_const(vm, Value::nil());
            }

            compiler->end_scope(vm);

            // This is needed so that the block, in which this block resides
            // get's it's stack difference updated. The final expression of
            // the block means the block itself adds 1 to the outer block's
            // offset
            STACK_DIFF(compiler, 1);
            break;
        }
        case AstntExprStmt: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            compiler->write_op(vm, OpPop);
            break;
        }
        case AstntAssignStmt: {
            AstNode *ident_node = node->kids[0];
            AstNode *value_node = node->kids[1];

            if (value_node->type == AstntFuncDef && value_node->kids[2]->val.b) {
                BUBBLE(compile_rec_func_def(vm, compiler, node));
                break;
            }

            BUBBLE(compile(vm, compiler, value_node));

            auto *ident = LambString::from_cstr(vm, ident_node->val.i);

            if (compiler->block->depth == 0) {
                compiler->write_op(vm, OpDefineGlobal);
                compiler->write_op_arg(vm, Value::from_obj((Object *)ident));
            } else {
                compiler->chunk().add_const(vm, Value::from_obj((Object *)ident));
                compiler->add_local(vm, ident->chars);
            }

            break;
        }
        case AstntNodeList: {
            for (AstNode *stmt = node; stmt != nullptr; stmt = stmt->kids[1]) {
                if (stmt->type != AstntNodeList) {
                    std::cerr << "[Lamb] Internal compiler error: AstNodeList->kids[1] is not of "
                                 "type AstntNodeList"
                              << '\n';
                    exit(EXIT_FAILURE);
                }

                BUBBLE(compile(vm, compiler, stmt->kids[0]));
            }
            break;
        }
        case AstntPatternArrayExt:
        default:
            std::cerr << "[Lamb] Internal compiler error: "
                      << "Unable to compile AstNode of kind: " << node->type << '\n';
            return CarUnsupportedAst;
    }

    return CarOk;
#undef BINARY
#undef UNARY
#undef CONSTANT
}

#undef BUBBLE
#undef STACK_DIFF
#undef ANON_FUNC_NAME
