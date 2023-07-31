#include <stdlib.h>
#include <string.h>
#include <iostream>

#include "ast.hpp"
#include "chunk.hpp"
#include "object.hpp"

#define ANON_FUNC_NAME "anonymous"
#define STACK_DIFF(compiler, diff) ((compiler)->block->offset += (diff))
#define BUBBLE(x)                                                                                  \
    if ((x) == CarUnsupportedAst) {                                                                \
        return CarUnsupportedAst;                                                                  \
    }

static bool is_stmt(AstNodeType type) {
    switch (type) {
        case AstntReturn:
            return true;
        case AstntExprStmt:
            return true;
        case AstntAssignStmt:
            return true;
        default:
            return false;
    }
}

static Compiler new_function_compiler(Vm& vm, Compiler *const compiler, Block *const block, char const* name) {
    Compiler func_comp(vm, compiler, block, FtNormal, name, 0);
    func_comp.new_scope();

    return func_comp;
}

static void add_arg_to_compiler(Vm& vm, Compiler *const func_comp, AstNode const *arg) {
    AstNode *ident_node = arg->kids[0];
    auto ident = LambString::from_cstr(vm, ident_node->val.i);
    func_comp->chunk().add_const(vm, Value::from_obj((Object *)ident));
    func_comp->add_local(vm, ident->chars);
    func_comp->function->arity++;
}

static void write_as_closure(Vm& vm, Compiler *const func_comp) {
    Compiler *compiler = func_comp->enclosing;
    compiler->chunk().write(vm, OpClosure);
    compiler->chunk().write_const(vm, Value::from_obj((Object *)func_comp->function));
    for (i32 i = 0; i < func_comp->function->upvalue_count; i++) {
         compiler->chunk().write(vm, func_comp->upvalues[i].is_local);
         compiler->chunk().write(vm, func_comp->upvalues[i].index);
    }

    func_comp->end_scope(vm);
    func_comp->destroy(vm);
    vm.curr_compiler = func_comp->enclosing;
}

static CompileAstResult compile_function(Vm& vm, Compiler *compiler, AstNode *node, char const* name) {
    AstNode *func_args = node->kids[0];
    AstNode *func_body = node->kids[1];
    AstNode *is_recursive = node->kids[2];

    Block block = {
        .prev = NULL,
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

    for (AstNode *arg = func_args; arg != NULL; arg = arg->kids[1]) {
        add_arg_to_compiler(vm, &func_comp, arg);
        block.offset += 1;
    }

    BUBBLE(compile(vm, &func_comp, func_body));
    func_comp.chunk().write(vm, OpReturn);

    if (vm.options.print_fn_chunks) {
        std::cerr << func_comp.chunk().to_string() << std::endl;
    }

    write_as_closure(vm, &func_comp);

    return CarOk;
}

static CompileAstResult compile_rec_func_def(Vm& vm, Compiler *compiler, AstNode *node) {
    AstNode *ident = node->kids[0];
    AstNode *func_def = node->kids[1];

    auto rec_func_ident = LambString::from_cstr(vm, ident->val.i);
    compiler->chunk().add_const(vm, Value::from_obj((Object *)rec_func_ident));

    BUBBLE(compile_function(vm, compiler, func_def, rec_func_ident->chars));
    STACK_DIFF(compiler, 1);

    if (compiler->block->depth == 0) {
        compiler->chunk().write(vm, OpDefineGlobal);
        compiler->chunk().write_const(vm, Value::from_obj((Object *)rec_func_ident));
        STACK_DIFF(compiler, -1);
    } else {
        compiler->add_local(vm, rec_func_ident->chars);
        STACK_DIFF(compiler, 0);
    }

    return CarOk;
}

static CompileAstResult compile_compose(Vm& vm, Compiler *compiler, AstNode *first, AstNode *second) {
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
    func_comp.chunk().write(vm, OpGetLocal);
    func_comp.chunk().write_const(vm,  Value::from_i64(1));

    func_comp.chunk().write(vm, OpCall);
    func_comp.chunk().write_const(vm,  Value::from_i64(1));

    func_comp.chunk().write(vm, OpCall);
    func_comp.chunk().write_const(vm,  Value::from_i64(1));

    func_comp.chunk().write(vm, OpReturn);

    if (vm.options.print_fn_chunks) {
        std::cerr << func_comp.chunk().to_string() << std::endl;
    }

    write_as_closure(vm, &func_comp);

    return CarOk;
}

CompileAstResult compile(Vm& vm, Compiler *compiler, AstNode *node) {
    #define CONSTANT(val) \
        do { \
            compiler->chunk().write_const(vm, (val)); \
            STACK_DIFF(compiler, 1); \
        } while(false)
    
    #define UNARY(op) \
        do { \
            BUBBLE(compile(vm, compiler, node->kids[0])); \
            compiler->chunk().write(vm, (op)); \
            STACK_DIFF(compiler, 0); \
        } while(false)
    
    #define BINARY(op) \
        do { \
            BUBBLE(compile(vm, compiler, node->kids[0])); \
            BUBBLE(compile(vm, compiler, node->kids[1])); \
            compiler->chunk().write(vm, op); \
            STACK_DIFF(compiler, -1); \
        } while(false)
    
    switch (node->type) {
        // Simple Constants
        case AstntNilLit:  CONSTANT(Value::nil());                  break;
        case AstntNumLit:  CONSTANT(Value::from_i64(node->val.n));  break;
        case AstntCharLit: CONSTANT(Value::from_char(node->val.c)); break;
        case AstntBoolLit: CONSTANT(Value::from_bool(node->val.b)); break;

        // Complex Constants
        case AstntIdent: {
            auto ident = LambString::from_cstr(vm, node->val.i);

            auto local_slot = compiler->local_slot(ident);
            if (local_slot) {
                auto slot = local_slot.value();
                compiler->chunk().write(vm, OpGetLocal);
                compiler->chunk().write_const(vm, Value::from_i64(slot));
            } else {
                auto upvalue_slot = compiler->upvalue_idx(ident);
                if (!upvalue_slot) {
                    compiler->chunk().write(vm, OpGetGlobal);
                    compiler->chunk().write_const(vm, Value::from_obj((Object *)ident));
                } else {
                    compiler->chunk().write(vm, OpGetUpvalue);
                    compiler->chunk().write_const(vm, Value::from_i64(upvalue_slot.value()));
                }
            }

            STACK_DIFF(compiler, 1);
            break;
        }

        // Complex constants
        case AstntStrLit: {
            auto lit = LambString::from_cstr(vm, node->val.s);
            compiler->chunk().write_const(vm, Value::from_obj((Object *)lit));
            STACK_DIFF(compiler, 1);
            break;
        }

        // Simple Unary
        case AstntUnaryNeg:     UNARY(OpNumNeg); break;
        case AstntUnaryLogNot:  UNARY(OpLogNeg); break;
        case AstntUnaryBitNot:  UNARY(OpBinNeg); break;

        // Simple Binary
        case AstntBinaryAdd:    BINARY(OpAdd); break;
        case AstntBinarySub:    BINARY(OpSub); break;
        case AstntBinaryMul:    BINARY(OpMul); break;
        case AstntBinaryDiv:    BINARY(OpDiv); break;
        case AstntBinaryMod:    BINARY(OpMod); break;
        case AstntBinaryEq:     BINARY(OpEq);  break;
        case AstntBinaryNe:     BINARY(OpNe);  break;
        case AstntBinaryGt:     BINARY(OpGt);  break;
        case AstntBinaryGe:     BINARY(OpGe);  break;
        case AstntBinaryLt:     BINARY(OpLt);  break;
        case AstntBinaryLe:     BINARY(OpLe);  break;
        case AstntBinaryOr:     BINARY(OpBinOr);  break;
        case AstntBinaryXor:    BINARY(OpBinXor); break;
        case AstntBinaryAnd:    BINARY(OpBinAnd); break;
        case AstntBinaryRShift: BINARY(OpRShift); break;
        case AstntBinaryLShift: BINARY(OpLShift); break;

        case AstntBinaryLogAnd: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            i32 if_false = compiler->chunk().write_jump(vm, OpJumpIfFalse);
            compiler->chunk().write(vm, OpPop);
            BUBBLE(compile(vm, compiler, node->kids[1]));
            compiler->chunk().patch_jump(if_false);
            STACK_DIFF(compiler, -1);
            break;
        }
        case AstntBinaryLogOr: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            i32 if_false = compiler->chunk().write_jump(vm, OpJumpIfFalse);
            i32 skip_right = compiler->chunk().write_jump(vm, OpJump);
            compiler->chunk().patch_jump(if_false);
            compiler->chunk().write(vm, OpPop);
            BUBBLE(compile(vm, compiler, node->kids[1]));
            compiler->chunk().patch_jump(skip_right);
            STACK_DIFF(compiler, -1);
            break;
        }
        case AstntBinaryLCompose: {
            AstNode *lhs = node->kids[0];
            AstNode *rhs = node->kids[1];
            compile_compose(vm, compiler, lhs, rhs);
            STACK_DIFF(compiler, -1);
            break;
        }
        case AstntBinaryRCompose: {
            AstNode *lhs = node->kids[0];
            AstNode *rhs = node->kids[1];
            compile_compose(vm, compiler, rhs, lhs);
            STACK_DIFF(compiler, -1);
            break;
        }
        case AstntBinaryLApply: {
            AstNode *lhs = node->kids[0];
            AstNode *rhs = node->kids[1];

            BUBBLE(compile(vm, compiler, lhs));
            BUBBLE(compile(vm, compiler, rhs));

            compiler->chunk().write(vm, OpCall);
            compiler->chunk().write_const(vm, Value::from_i64(1));
            STACK_DIFF(compiler, -1);
            break;
        }
        case AstntBinaryRApply: {
            AstNode *lhs = node->kids[0];
            AstNode *rhs = node->kids[1];

            BUBBLE(compile(vm, compiler, rhs));
            BUBBLE(compile(vm, compiler, lhs));

            compiler->chunk().write(vm, OpCall);
            compiler->chunk().write_const(vm, Value::from_i64(1));
            STACK_DIFF(compiler, -1);
            break;
        }
        case AstntFuncDef: {
            BUBBLE(compile_function(vm, compiler, node, ANON_FUNC_NAME));
            STACK_DIFF(compiler, 1);
            break;
        }
        case AstntFuncCall: {
            AstNode *callee = node->kids[0];
            BUBBLE(compile(vm, compiler, callee));

            u32 arg_count = 0;
            for (AstNode *arg_list = node->kids[1]; arg_list != NULL;
                 arg_list = arg_list->kids[1]) {
                BUBBLE(compile(vm, compiler, arg_list->kids[0]));
                arg_count += 1;
            }

            compiler->chunk().write(vm, OpCall);
            compiler->chunk().write_const(vm, Value::from_i64((i64)arg_count));
            STACK_DIFF(compiler, -arg_count);
            break;
        }
        case AstntReturn: {
            AstNode *val = node->kids[0];
            if (val == NULL) {
                compiler->chunk().write_const(vm, Value::nil());
            } else {
                BUBBLE(compile(vm, compiler, val));
            }
            compiler->chunk().write(vm, OpReturn);
            STACK_DIFF(compiler, -1);
            break;
        }
        case AstntArray: {
            // Reverse the linked list
            AstNode *prev = NULL;
            AstNode *curr = node->kids[0];
            if (curr == NULL) {
                compiler->chunk().write_const(vm, Value::from_i64(0));
                STACK_DIFF(compiler, 1);

                compiler->chunk().write(vm, OpMakeArray);
                STACK_DIFF(compiler, 0);
                return CarOk;
            }

            AstNode *next = curr->kids[1];
            while (curr != NULL) {
                curr->kids[1] = prev;
                prev = curr;
                curr = next;

                if (next != NULL) {
                    next = next->kids[1];
                }
            }

            node->kids[0] = prev;

            i32 len = 0;
            for (AstNode *expr_list = node->kids[0]; expr_list != NULL;
                 expr_list = expr_list->kids[1]) {
                BUBBLE(compile(vm, compiler, expr_list->kids[0]));
                len += 1;
            }

            compiler->chunk().write_const(vm, Value::from_i64(len));
            STACK_DIFF(compiler, 1);

            compiler->chunk().write(vm, OpMakeArray);
            STACK_DIFF(compiler, -len);
            break;
        }
        case AstntArrayIndex: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            BUBBLE(compile(vm, compiler, node->kids[1]));
            compiler->chunk().write(vm, OpIndexArray);
            STACK_DIFF(compiler, -1);
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
            BUBBLE(compile(vm, compiler, node->kids[0]));
            i32 if_false_jump = compiler->chunk().write_jump(vm, OpJumpIfFalse);

            compiler->chunk().write(vm, OpPop);
            BUBBLE(compile(vm, compiler, node->kids[1]));
            i32 past_else = compiler->chunk().write_jump(vm, OpJump);

            compiler->chunk().patch_jump(if_false_jump);
            compiler->chunk().write(vm, OpPop);

            if (node->kids[2] != NULL) {
                BUBBLE(compile(vm, compiler, node->kids[2]));
            } else {
                compiler->chunk().write_const(vm, Value::nil());
            }

            compiler->chunk().patch_jump(past_else);

            STACK_DIFF(compiler, 0);
            break;
        }
        case AstntCase: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            BUBBLE(compile(vm, compiler, node->kids[1]));
            STACK_DIFF(compiler, 0);
            break;
        }
        case AstntCaseArm: {
            AstNode *value = node->kids[0];
            AstNode *branch = node->kids[1];
            AstNode *next_arm = node->kids[2];

            // Compare the value of the arm with a duplicate of the case value
            compiler->chunk().write(vm, OpDup);
            BUBBLE(compile(vm, compiler, value));
            compiler->chunk().write(vm, OpEq);

            i32 if_neq = compiler->chunk().write_jump(vm, OpJumpIfFalse);

            // If equal -------->
            // Pop the case 'true' off of the stack
            compiler->chunk().write(vm, OpPop);

            // Pop the case value off of the stack
            compiler->chunk().write(vm, OpPop);

            // Run through the body of the arm
            BUBBLE(compile(vm, compiler, branch));

            // Jump over the other arms of the case expression
            i32 past_else = compiler->chunk().write_jump(vm, OpJump);
            // If equal <-------

            // If not equal -------->
            compiler->chunk().patch_jump(if_neq);
            // Pop the 'false' from the previous EQ check off of the stack
            compiler->chunk().write(vm, OpPop);

            if (next_arm != NULL) {
                // Attempt the next arm
                BUBBLE(compile(vm, compiler, next_arm));
            } else {
                // Pop the compare value off of the stack
                compiler->chunk().write(vm, OpPop);

                // We can't check for exhaustivity at compile time, so we write
                // a default nil in the event none of arms matched successfully
                compiler->chunk().write_const(vm, Value::nil());
            }
            // If not equal <-------

            // If successfull, we can jump over all of the arms thanks to the
            // recursive nature of the case arms representation
            compiler->chunk().patch_jump(past_else);
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
            for (; stmt != NULL && stmt->type == AstntNodeList && is_stmt(stmt->kids[0]->type);
                 stmt = stmt->kids[1]) {
                BUBBLE(compile(vm, compiler, stmt->kids[0]));
            }

            if (stmt != NULL) {
                BUBBLE(compile(vm, compiler, stmt));
            } else {
                compiler->chunk().write_const(vm, Value::nil());
            }

            compiler->end_scope(vm);
            STACK_DIFF(compiler, 1);
            break;
        }
        case AstntExprStmt: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            compiler->chunk().write(vm, OpPop);
            STACK_DIFF(compiler, -1);
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

            auto ident = LambString::from_cstr(vm, ident_node->val.i);

            if (compiler->block->depth == 0) {
                compiler->chunk().write(vm, OpDefineGlobal);
                compiler->chunk().write_const(vm, Value::from_obj((Object *)ident));
                STACK_DIFF(compiler, -1);
            } else {
                compiler->chunk().add_const(vm, Value::from_obj((Object *)ident));
                compiler->add_local(vm, ident->chars);
                STACK_DIFF(compiler, 0);
            }

            break;
        }
        case AstntNodeList: {
            for (AstNode *stmt = node; stmt != NULL; stmt = stmt->kids[1]) {
                if (stmt->type != AstntNodeList) {
                    std::cerr << "[Lamb] Internal compiler error: AstNodeList->kids[1] is not of type AstntNodeList"
                              << std::endl;
                    exit(EXIT_FAILURE);
                }

                BUBBLE(compile(vm, compiler, stmt->kids[0]));
            }
            break;
        }
        default:
            std::cerr << "[Lamb] Internal compiler error: "
                      << "Unable to compile AstNode of kind: " << node->type
                      << std::endl;
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
