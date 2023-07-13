#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../debug/debug.hpp"
#include "ast.hpp"
#include "chunk.hpp"
#include "misc.hpp"
#include "object.hpp"

#define LOCAL_NOT_FOUND -1
#define UPVALUE_NOT_FOUND -1
#define ANON_FUNC_NAME "anonymous"

#define STACK_DIFF(compiler, diff) ((compiler)->block->offset += (diff))

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

static i32 add_upvalue(Compiler *const compiler, i32 index, bool is_local) {
    int count = compiler->function->upvalue_count;

    if (index > 255) {
        // TODO: Figure out how to get a dynamic amount of upvalues
        printf("COMPILER_ERR: Too many upvalues. Max 255...");
        exit(1);
    }

    for (i32 i = 0; i < count; i++) {
        Upvalue *upvalue = &compiler->upvalues[i];
        if (upvalue->index == index && upvalue->is_local == is_local) {
            return i;
        }
    }

    compiler->upvalues[count].is_local = is_local;
    compiler->upvalues[count].index = index;

    return compiler->function->upvalue_count++;
}

static i32 resolve_local(Compiler *const compiler, LambString *const name) {
    for (i32 i = compiler->locals.len() - 1; i >= 0; i--) {
        Local *local = &compiler->locals[i];
        if (local->name == name->chars) {
            return i;
        }
    }

    return LOCAL_NOT_FOUND;
}

static i32 resolve_upvalue(Compiler *const compiler, LambString *const name) {
    if (compiler->enclosing == NULL) {
        return UPVALUE_NOT_FOUND;
    }

    i32 local = resolve_local(compiler->enclosing, name);
    if (local != LOCAL_NOT_FOUND) {
        compiler->enclosing->locals[local].is_captured = true;
        return add_upvalue(compiler, local, true);
    }

    i32 upvalue = resolve_upvalue(compiler->enclosing, name);
    if (upvalue != UPVALUE_NOT_FOUND) {
        return add_upvalue(compiler, upvalue, false);
    }

    return UPVALUE_NOT_FOUND;
}

#define BUBBLE(x)                                                                                  \
    if ((x) == CarUnsupportedAst) {                                                                \
        return CarUnsupportedAst;                                                                  \
    }

static Compiler new_function_compiler(Vm& vm, Compiler *const compiler, Block *const block, char const* name) {
    Compiler func_comp(vm, compiler, block, FtNormal, name, 0);
    func_comp.new_scope();
    vm.curr_compiler = &func_comp;

    return func_comp;
}

static void add_arg_to_compiler(Vm& vm, Compiler *const func_comp, AstNode const *arg) {
    AstNode *ident_node = arg->kids[0];
    LambString *ident = cstr_to_lambstring(vm, ident_node->val.i);
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
}

static CompileAstResult compile_function(Vm& vm, Compiler *compiler, AstNode *node, char const* name) {
    AstNode *func_args = node->kids[0];
    AstNode *func_body = node->kids[1];
    AstNode *is_recursive = node->kids[2];

    Block block = {
        .base = 0,
        .offset = 0,
        .depth = 0,
        .prev = NULL,
    };

    Compiler func_comp = new_function_compiler(vm, compiler, &block, name);

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
        chunk_debug(func_comp.chunk(), "Function Chunk");
    }

    write_as_closure(vm, &func_comp);

    func_comp.end_scope(vm);
    func_comp.destroy(vm);
    vm.curr_compiler = func_comp.enclosing;

    return CarOk;
}

static CompileAstResult compile_rec_func_def(Vm& vm, Compiler *compiler, AstNode *node) {
    AstNode *ident = node->kids[0];
    AstNode *func_def = node->kids[1];

    LambString *rec_func_ident = cstr_to_lambstring(vm, ident->val.i);
    compiler->chunk().add_const(vm, Value::from_obj((Object *)rec_func_ident));

    BUBBLE(compile_function(vm, compiler, func_def, rec_func_ident->chars));

    if (compiler->block->depth == 0) {
        compiler->chunk().write(vm, OpDefineGlobal);
        compiler->chunk().write_const(vm, Value::from_obj((Object *)rec_func_ident));
    } else {
        compiler->add_local(vm, rec_func_ident->chars);
    }

    return CarOk;
}

static CompileAstResult compile_compose(Vm& vm, Compiler *compiler, AstNode *first, AstNode *second,
                                        char const* name) {
    // Turns: `f1 .> f2` or `f2 <. f1` into:
    // result := fn(x) -> f2(f1(x));
    Block block = {
        .base = 1,
        .offset = 0,
        .depth = 0,
        .prev = compiler->block,
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

    compiler->chunk().write(vm, OpClosure);
    compiler->chunk().write_const(vm, Value::from_obj((Object *)func_comp.function));

    if (vm.options.print_fn_chunks) {
        chunk_debug(func_comp.chunk(), name);
    }

    for (i32 i = 0; i < func_comp.function->upvalue_count; i++) {
       compiler->chunk().write(vm, func_comp.upvalues[i].is_local ? 1 : 0);
       compiler->chunk().write(vm, func_comp.upvalues[i].index);
    }

    func_comp.end_scope(vm);
    func_comp.destroy(vm);
    vm.curr_compiler = func_comp.enclosing;

    return CarOk;
}

CompileAstResult compile(Vm& vm, Compiler *compiler, AstNode *node) {
    switch (node->type) {
        case AstntStrLit: {
            LambString *lit = cstr_to_lambstring(vm, node->val.s);
            compiler->chunk().write_const(vm, Value::from_obj((Object *)lit));
            STACK_DIFF(compiler, 1);
            break;
        }
        case AstntIdent: {
            LambString *ident = cstr_to_lambstring(vm, node->val.i);

            i32 local_slot = resolve_local(compiler, ident);
            if (local_slot != LOCAL_NOT_FOUND) {
                compiler->chunk().write(vm, OpGetLocal);

                i32 depth = compiler->locals[local_slot].depth;
                i32 base = -1;
                for (Block *bl = compiler->block; bl != NULL; bl = bl->prev) {
                    if (bl->depth == depth) {
                        base = bl->base;
                        break;
                    }
                }

                i32 local_idx = 0;
                for (i32 idx = local_slot - 1;
                     idx >= 0 && compiler->locals[idx].depth == depth; idx--) {
                    local_idx++;
                }

                i32 local_slot = base + local_idx;
                compiler->chunk().write_const(vm, Value::from_i64(local_slot));
            } else {
                i32 upvalue_slot = resolve_upvalue(compiler, ident);
                if (upvalue_slot == UPVALUE_NOT_FOUND) {
                    compiler->chunk().write(vm, OpGetGlobal);
                    compiler->chunk().write_const(vm, Value::from_obj((Object *)ident));
                } else {
                    compiler->chunk().write(vm, OpGetUpvalue);
                    compiler->chunk().write_const(vm, Value::from_i64(upvalue_slot));
                }
            }

            STACK_DIFF(compiler, 1);
            break;
        }
        case AstntNilLit: {
            compiler->chunk().write_const(vm, Value::nil());
            STACK_DIFF(compiler, 1);
            break;
        }
        case AstntNumLit: {
            compiler->chunk().write_const(vm, Value::from_i64(node->val.n));
            STACK_DIFF(compiler, 1);
            break;
        }
        case AstntCharLit: {
            compiler->chunk().write_const(vm, Value::from_char(node->val.c));
            STACK_DIFF(compiler, 1);
            break;
        }
        case AstntBoolLit: {
            compiler->chunk().write_const(vm, Value::from_bool(node->val.b));
            STACK_DIFF(compiler, 1);
            break;
        }
        case AstntUnaryNeg: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            compiler->chunk().write(vm, OpNumNeg);
            STACK_DIFF(compiler, 0);
            break;
        }
        case AstntUnaryLogNot: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            compiler->chunk().write(vm, OpLogNeg);
            STACK_DIFF(compiler, 0);
            break;
        }
        case AstntUnaryBitNot: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            compiler->chunk().write(vm, OpBinNeg);
            STACK_DIFF(compiler, 0);
            break;
        }
        case AstntBinaryAdd: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            BUBBLE(compile(vm, compiler, node->kids[1]));
            compiler->chunk().write(vm, OpAdd);
            STACK_DIFF(compiler, -1);
            break;
        }
        case AstntBinarySub: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            BUBBLE(compile(vm, compiler, node->kids[1]));
            compiler->chunk().write(vm, OpSub);
            STACK_DIFF(compiler, -1);
            break;
        }
        case AstntBinaryMul: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            BUBBLE(compile(vm, compiler, node->kids[1]));
            compiler->chunk().write(vm, OpMul);
            STACK_DIFF(compiler, -1);
            break;
        }
        case AstntBinaryDiv: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            BUBBLE(compile(vm, compiler, node->kids[1]));
            compiler->chunk().write(vm, OpDiv);
            STACK_DIFF(compiler, -1);
            break;
        }
        case AstntBinaryMod: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            BUBBLE(compile(vm, compiler, node->kids[1]));
            compiler->chunk().write(vm, OpMod);
            STACK_DIFF(compiler, -1);
            break;
        }
        case AstntBinaryEq: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            BUBBLE(compile(vm, compiler, node->kids[1]));
            compiler->chunk().write(vm, OpEq);
            STACK_DIFF(compiler, -1);
            break;
        }
        case AstntBinaryNe: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            BUBBLE(compile(vm, compiler, node->kids[1]));
            compiler->chunk().write(vm, OpNe);
            STACK_DIFF(compiler, -1);
            break;
        }
        case AstntBinaryGt: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            BUBBLE(compile(vm, compiler, node->kids[1]));
            compiler->chunk().write(vm, OpGt);
            STACK_DIFF(compiler, -1);
            break;
        }
        case AstntBinaryGe: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            BUBBLE(compile(vm, compiler, node->kids[1]));
            compiler->chunk().write(vm, OpGe);
            STACK_DIFF(compiler, -1);
            break;
        }
        case AstntBinaryLt: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            BUBBLE(compile(vm, compiler, node->kids[1]));
            compiler->chunk().write(vm, OpLt);
            STACK_DIFF(compiler, -1);
            break;
        }
        case AstntBinaryLe: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            BUBBLE(compile(vm, compiler, node->kids[1]));
            compiler->chunk().write(vm, OpLe);
            STACK_DIFF(compiler, -1);
            break;
        }
        case AstntBinaryOr: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            BUBBLE(compile(vm, compiler, node->kids[1]));
            compiler->chunk().write(vm, OpBinOr);
            STACK_DIFF(compiler, -1);
            break;
        }
        case AstntBinaryXor: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            BUBBLE(compile(vm, compiler, node->kids[1]));
            compiler->chunk().write(vm, OpBinXor);
            STACK_DIFF(compiler, -1);
            break;
        }
        case AstntBinaryAnd: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            BUBBLE(compile(vm, compiler, node->kids[1]));
            compiler->chunk().write(vm, OpBinAnd);
            STACK_DIFF(compiler, -1);
            break;
        }
        case AstntBinaryRShift: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            BUBBLE(compile(vm, compiler, node->kids[1]));
            compiler->chunk().write(vm, OpRShift);
            STACK_DIFF(compiler, -1);
            break;
        }
        case AstntBinaryLShift: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            BUBBLE(compile(vm, compiler, node->kids[1]));
            compiler->chunk().write(vm, OpLShift);
            STACK_DIFF(compiler, -1);
            break;
        }
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

            compile_compose(vm, compiler, lhs, rhs, "BinaryLCompose");
            STACK_DIFF(compiler, -1);
            break;
        }
        case AstntBinaryRCompose: {
            AstNode *lhs = node->kids[0];
            AstNode *rhs = node->kids[1];
            compile_compose(vm, compiler, rhs, lhs, "BinaryRCompose");
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
                compiler->chunk().write(vm, OpMakeArray);
                STACK_DIFF(compiler, 1);
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
            compiler->chunk().write(vm, OpMakeArray);
            STACK_DIFF(compiler, -len + 1);
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
            i32 prev_offset = prev->offset;
            Block block = {.base = prev->base + prev->offset,
                           .offset = 0,
                           .depth = compiler->block->depth,
                           .prev = prev};
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
            compiler->block->offset = prev_offset + 1;
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
                STACK_DIFF(compiler, 1);
                break;
            }

            BUBBLE(compile(vm, compiler, value_node));

            LambString *ident = cstr_to_lambstring(vm, ident_node->val.i);

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
                    fprintf(stderr,
                            "[Lamb] ICE: AstntNodeList->kids[1] is not of type AstntNodeList");
                    exit(EXIT_FAILURE);
                }

                BUBBLE(compile(vm, compiler, stmt->kids[0]));
            }
            break;
        }
        default:
            fprintf(stderr, "Unable to compile AstNode of kind: (%d)", node->type);
            fprintf(stderr, "Default branch reached while matching on node->type in %s on line %d",
                    __FILE__, __LINE__);
            return CarUnsupportedAst;
    }

    return CarOk;
}

#undef BUBBLE
#undef STACK_DIFF
