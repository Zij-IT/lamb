#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../debug/debug.h"
#include "ast.h"
#include "misc.h"
#include "object.h"

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
    for (i32 i = compiler->locals.len - 1; i >= 0; i--) {
        Local *local = &compiler->locals.values[i];
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
        compiler->enclosing->locals.values[local].is_captured = true;
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

static Chunk *compiler_chunk(Compiler *compiler) { return &compiler->function->chunk; }

static Compiler new_function_compiler(Vm *const vm, Compiler *const compiler, str name) {
    Compiler func_comp;
    compiler_init(vm, &func_comp, FtNormal);
    compiler_new_scope(&func_comp);
    func_comp.function = (LambFunc *)alloc_obj(vm, OtFunc);
    func_comp.function->name = name;
    func_comp.enclosing = compiler;
    vm->curr_compiler = &func_comp;

    return func_comp;
}

static void add_arg_to_compiler(Vm *const vm, Compiler *const func_comp, AstNode const *arg) {
    AstNode *ident_node = arg->kids[0];
    LambString *ident = cstr_to_lambstring(vm, ident_node->val.i);
    chunk_add_constant(vm, compiler_chunk(func_comp), new_object((Object *)ident));
    Local loc = {.depth = func_comp->scope_depth, .name = ident->chars, .is_captured = false};
    local_arr_write(vm, &func_comp->locals, loc);
    func_comp->function->arity++;
}

static void write_as_closure(Vm *const vm, Compiler *const func_comp) {
    Compiler *compiler = func_comp->enclosing;
    chunk_write(vm, compiler_chunk(compiler), OpClosure);
    chunk_write_constant(vm, compiler_chunk(compiler), new_object((Object *)func_comp->function));
    for (i32 i = 0; i < func_comp->function->upvalue_count; i++) {
        chunk_write(vm, compiler_chunk(compiler), func_comp->upvalues[i].is_local);
        chunk_write(vm, compiler_chunk(compiler), func_comp->upvalues[i].index);
    }
}

static CompileAstResult compile_function(Vm *vm, Compiler *compiler, AstNode *node, str name) {
    AstNode *func_args = node->kids[0];
    AstNode *func_body = node->kids[1];
    AstNode *is_recursive = node->kids[2];

    Compiler func_comp = new_function_compiler(vm, compiler, name);
    Block block = {
        .base = 0,
        .offset = 0,
        .depth = func_comp.scope_depth,
        .prev = NULL,
    };

    func_comp.block = &block;

    // The first local in a function is actually either a nameless value,
    // or the function itself, and in order for it to be accessible the
    // depth must match.
    func_comp.locals.values[0].depth = 1;
    block.offset += 1;

    if (is_recursive->val.b) {
        func_comp.locals.values[0].name = name;
    }

    for (AstNode *arg = func_args; arg != NULL; arg = arg->kids[1]) {
        add_arg_to_compiler(vm, &func_comp, arg);
        block.offset += 1;
    }

    BUBBLE(compile(vm, &func_comp, func_body));
    chunk_write(vm, compiler_chunk(&func_comp), OpReturn);

    if (vm->options.print_fn_chunks) {
        chunk_debug(&func_comp.function->chunk, "Function Chunk");
    }

    write_as_closure(vm, &func_comp);

    compiler_end_scope(vm, &func_comp);
    compiler_free(vm, &func_comp);
    vm->curr_compiler = func_comp.enclosing;

    return CarOk;
}

static CompileAstResult compile_rec_func_def(Vm *vm, Compiler *compiler, AstNode *node) {
    AstNode *ident = node->kids[0];
    AstNode *func_def = node->kids[1];

    LambString *rec_func_ident = cstr_to_lambstring(vm, ident->val.i);
    chunk_add_constant(vm, compiler_chunk(compiler), new_object((Object *)rec_func_ident));

    BUBBLE(compile_function(vm, compiler, func_def, rec_func_ident->chars));

    if (compiler->scope_depth == 0) {
        chunk_write(vm, compiler_chunk(compiler), OpDefineGlobal);
        chunk_write_constant(vm, compiler_chunk(compiler), new_object((Object *)rec_func_ident));
    } else {
        Local loc = {
            .depth = compiler->scope_depth, .name = rec_func_ident->chars, .is_captured = false};
        local_arr_write(vm, &compiler->locals, loc);
    }

    return CarOk;
}

static CompileAstResult compile_compose(Vm *vm, Compiler *compiler, AstNode *first, AstNode *second,
                                        str name) {
    Compiler func_comp;
    compiler_init(vm, &func_comp, FtNormal);
    compiler_new_scope(&func_comp);
    func_comp.function = (LambFunc *)alloc_obj(vm, OtFunc);
    func_comp.function->name = ANON_FUNC_NAME;
    func_comp.function->arity = 1;
    func_comp.enclosing = compiler;
    vm->curr_compiler = &func_comp;

    BUBBLE(compile(vm, &func_comp, first));
    BUBBLE(compile(vm, &func_comp, second));

    chunk_write(vm, compiler_chunk(&func_comp), OpGetLocal);
    chunk_write_constant(vm, compiler_chunk(&func_comp), new_int(1));

    chunk_write(vm, compiler_chunk(&func_comp), OpCall);
    chunk_write_constant(vm, compiler_chunk(&func_comp), new_int(1));

    chunk_write(vm, compiler_chunk(&func_comp), OpCall);
    chunk_write_constant(vm, compiler_chunk(&func_comp), new_int(1));

    chunk_write(vm, compiler_chunk(&func_comp), OpReturn);

    chunk_write(vm, compiler_chunk(compiler), OpClosure);
    chunk_write_constant(vm, compiler_chunk(compiler), new_object((Object *)func_comp.function));

    if (vm->options.print_fn_chunks) {
        chunk_debug(compiler_chunk(&func_comp), name);
    }

    for (i32 i = 0; i < func_comp.function->upvalue_count; i++) {
        chunk_write(vm, compiler_chunk(compiler), func_comp.upvalues[i].is_local ? 1 : 0);
        chunk_write(vm, compiler_chunk(compiler), func_comp.upvalues[i].index);
    }

    compiler_end_scope(vm, &func_comp);
    compiler_free(vm, &func_comp);
    vm->curr_compiler = func_comp.enclosing;

    return CarOk;
}

CompileAstResult compile(Vm *vm, Compiler *compiler, AstNode *node) {
    compiler->block->depth = compiler->scope_depth;
    switch (node->type) {
        case AstntStrLit: {
            LambString *lit = cstr_to_lambstring(vm, node->val.s);
            chunk_write_constant(vm, compiler_chunk(compiler), new_object((Object *)lit));
            STACK_DIFF(compiler, 1);
            break;
        }
        case AstntIdent: {
            LambString *ident = cstr_to_lambstring(vm, node->val.i);

            i32 local_slot = resolve_local(compiler, ident);
            if (local_slot != LOCAL_NOT_FOUND) {
                chunk_write(vm, compiler_chunk(compiler), OpGetLocal);

                i32 depth = compiler->locals.values[local_slot].depth; 
                i32 base = -1;
                for (Block* bl = compiler->block; bl != NULL; bl = bl->prev) {
                    if (bl->depth == depth) {
                        base = bl->base;
                        break;
                    }
                }

                i32 local_idx = 0;
                for (i32 idx = local_slot - 1; idx >= 0 && compiler->locals.values[idx].depth == depth; idx--) {
                    local_idx++;
                }

                i32 local_slot = base + local_idx;
                chunk_write_constant(vm, compiler_chunk(compiler), new_int(local_slot));
            } else {
                i32 upvalue_slot = resolve_upvalue(compiler, ident);
                if (upvalue_slot == UPVALUE_NOT_FOUND) {
                    chunk_write(vm, compiler_chunk(compiler), OpGetGlobal);
                    chunk_write_constant(vm, compiler_chunk(compiler), new_object((Object *)ident));
                } else {
                    chunk_write(vm, compiler_chunk(compiler), OpGetUpvalue);
                    chunk_write_constant(vm, compiler_chunk(compiler), new_int(upvalue_slot));
                }
            }

            STACK_DIFF(compiler, 1);
            break;
        }
        case AstntNilLit: {
            chunk_write_constant(vm, compiler_chunk(compiler), new_nil());
            STACK_DIFF(compiler, 1);
            break;
        }
        case AstntNumLit: {
            chunk_write_constant(vm, compiler_chunk(compiler), new_int(node->val.n));
            STACK_DIFF(compiler, 1);
            break;
        }
        case AstntCharLit: {
            chunk_write_constant(vm, compiler_chunk(compiler), new_char(node->val.c));
            STACK_DIFF(compiler, 1);
            break;
        }
        case AstntBoolLit: {
            chunk_write_constant(vm, compiler_chunk(compiler), new_boolean(node->val.b));
            STACK_DIFF(compiler, 1);
            break;
        }
        case AstntUnaryNeg: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            chunk_write(vm, compiler_chunk(compiler), OpNumNeg);
            STACK_DIFF(compiler, 0);
            break;
        }
        case AstntUnaryLogNot: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            chunk_write(vm, compiler_chunk(compiler), OpLogNeg);
            STACK_DIFF(compiler, 0);
            break;
        }
        case AstntUnaryBitNot: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            chunk_write(vm, compiler_chunk(compiler), OpBinNeg);
            STACK_DIFF(compiler, 0);
            break;
        }
        case AstntBinaryAdd: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            BUBBLE(compile(vm, compiler, node->kids[1]));
            chunk_write(vm, compiler_chunk(compiler), OpAdd);
            STACK_DIFF(compiler, -1);
            break;
        }
        case AstntBinarySub: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            BUBBLE(compile(vm, compiler, node->kids[1]));
            chunk_write(vm, compiler_chunk(compiler), OpSub);
            STACK_DIFF(compiler, -1);
            break;
        }
        case AstntBinaryMul: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            BUBBLE(compile(vm, compiler, node->kids[1]));
            chunk_write(vm, compiler_chunk(compiler), OpMul);
            STACK_DIFF(compiler, -1);
            break;
        }
        case AstntBinaryDiv: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            BUBBLE(compile(vm, compiler, node->kids[1]));
            chunk_write(vm, compiler_chunk(compiler), OpDiv);
            STACK_DIFF(compiler, -1);
            break;
        }
        case AstntBinaryMod: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            BUBBLE(compile(vm, compiler, node->kids[1]));
            chunk_write(vm, compiler_chunk(compiler), OpMod);
            STACK_DIFF(compiler, -1);
            break;
        }
        case AstntBinaryEq: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            BUBBLE(compile(vm, compiler, node->kids[1]));
            chunk_write(vm, compiler_chunk(compiler), OpEq);
            STACK_DIFF(compiler, -1);
            break;
        }
        case AstntBinaryNe: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            BUBBLE(compile(vm, compiler, node->kids[1]));
            chunk_write(vm, compiler_chunk(compiler), OpNe);
            STACK_DIFF(compiler, -1);
            break;
        }
        case AstntBinaryGt: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            BUBBLE(compile(vm, compiler, node->kids[1]));
            chunk_write(vm, compiler_chunk(compiler), OpGt);
            STACK_DIFF(compiler, -1);
            break;
        }
        case AstntBinaryGe: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            BUBBLE(compile(vm, compiler, node->kids[1]));
            chunk_write(vm, compiler_chunk(compiler), OpGe);
            STACK_DIFF(compiler, -1);
            break;
        }
        case AstntBinaryLt: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            BUBBLE(compile(vm, compiler, node->kids[1]));
            chunk_write(vm, compiler_chunk(compiler), OpLt);
            STACK_DIFF(compiler, -1);
            break;
        }
        case AstntBinaryLe: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            BUBBLE(compile(vm, compiler, node->kids[1]));
            chunk_write(vm, compiler_chunk(compiler), OpLe);
            STACK_DIFF(compiler, -1);
            break;
        }
        case AstntBinaryOr: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            BUBBLE(compile(vm, compiler, node->kids[1]));
            chunk_write(vm, compiler_chunk(compiler), OpBinOr);
            STACK_DIFF(compiler, -1);
            break;
        }
        case AstntBinaryXor: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            BUBBLE(compile(vm, compiler, node->kids[1]));
            chunk_write(vm, compiler_chunk(compiler), OpBinXor);
            STACK_DIFF(compiler, -1);
            break;
        }
        case AstntBinaryAnd: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            BUBBLE(compile(vm, compiler, node->kids[1]));
            chunk_write(vm, compiler_chunk(compiler), OpBinAnd);
            STACK_DIFF(compiler, -1);
            break;
        }
        case AstntBinaryRShift: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            BUBBLE(compile(vm, compiler, node->kids[1]));
            chunk_write(vm, compiler_chunk(compiler), OpRShift);
            STACK_DIFF(compiler, -1);
            break;
        }
        case AstntBinaryLShift: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            BUBBLE(compile(vm, compiler, node->kids[1]));
            chunk_write(vm, compiler_chunk(compiler), OpLShift);
            STACK_DIFF(compiler, -1);
            break;
        }
        case AstntBinaryLogAnd: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            i32 if_false = chunk_write_jump(vm, compiler_chunk(compiler), OpJumpIfFalse);
            chunk_write(vm, compiler_chunk(compiler), OpPop);
            BUBBLE(compile(vm, compiler, node->kids[1]));
            chunk_patch_jump(compiler_chunk(compiler), if_false);
            STACK_DIFF(compiler, -1);
            break;
        }
        case AstntBinaryLogOr: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            i32 if_false = chunk_write_jump(vm, compiler_chunk(compiler), OpJumpIfFalse);
            i32 skip_right = chunk_write_jump(vm, compiler_chunk(compiler), OpJump);
            chunk_patch_jump(compiler_chunk(compiler), if_false);
            chunk_write(vm, compiler_chunk(compiler), OpPop);
            BUBBLE(compile(vm, compiler, node->kids[1]));
            chunk_patch_jump(compiler_chunk(compiler), skip_right);
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

            chunk_write(vm, compiler_chunk(compiler), OpCall);
            chunk_write_constant(vm, compiler_chunk(compiler), new_int(1));
            STACK_DIFF(compiler, -1);
            break;
        }
        case AstntBinaryRApply: {
            AstNode *lhs = node->kids[0];
            AstNode *rhs = node->kids[1];

            BUBBLE(compile(vm, compiler, rhs));
            BUBBLE(compile(vm, compiler, lhs));

            chunk_write(vm, compiler_chunk(compiler), OpCall);
            chunk_write_constant(vm, compiler_chunk(compiler), new_int(1));
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

            chunk_write(vm, compiler_chunk(compiler), OpCall);
            chunk_write_constant(vm, compiler_chunk(compiler), new_int((i64)arg_count));
            STACK_DIFF(compiler, -arg_count);
            break;
        }
    case AstntReturn: {
            AstNode *val = node->kids[0];
            if (val == NULL) {
                chunk_write_constant(vm, compiler_chunk(compiler), new_nil());
            } else {
                BUBBLE(compile(vm, compiler, val));
            }
            chunk_write(vm, compiler_chunk(compiler), OpReturn);
            STACK_DIFF(compiler, -1);
            break;
        }
        case AstntArray: {
            // Reverse the linked list
            AstNode *prev = NULL;
            AstNode *curr = node->kids[0];
            if (curr == NULL) {
                chunk_write_constant(vm, compiler_chunk(compiler), new_int(0));
                chunk_write(vm, compiler_chunk(compiler), OpMakeArray);
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
            chunk_write_constant(vm, compiler_chunk(compiler), new_int(len));
            chunk_write(vm, compiler_chunk(compiler), OpMakeArray);
            STACK_DIFF(compiler, -len + 1);
            break;
        }
        case AstntArrayIndex: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            BUBBLE(compile(vm, compiler, node->kids[1]));
            chunk_write(vm, compiler_chunk(compiler), OpIndexArray);
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
            i32 if_false_jump = chunk_write_jump(vm, compiler_chunk(compiler), OpJumpIfFalse);

            chunk_write(vm, compiler_chunk(compiler), OpPop);
            BUBBLE(compile(vm, compiler, node->kids[1]));
            i32 past_else = chunk_write_jump(vm, compiler_chunk(compiler), OpJump);

            chunk_patch_jump(compiler_chunk(compiler), if_false_jump);
            chunk_write(vm, compiler_chunk(compiler), OpPop);

            if (node->kids[2] != NULL) {
                BUBBLE(compile(vm, compiler, node->kids[2]));
            } else {
                chunk_write_constant(vm, compiler_chunk(compiler), new_nil());
            }

            chunk_patch_jump(compiler_chunk(compiler), past_else);

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

            chunk_write(vm, compiler_chunk(compiler), OpDup);
            BUBBLE(compile(vm, compiler, value));
            chunk_write(vm, compiler_chunk(compiler), OpEq);

            i32 if_neq = chunk_write_jump(vm, compiler_chunk(compiler), OpJumpIfFalse);

            chunk_write(vm, compiler_chunk(compiler), OpPop);
            chunk_write(vm, compiler_chunk(compiler), OpPop);
            BUBBLE(compile(vm, compiler, branch));
            i32 past_else = chunk_write_jump(vm, compiler_chunk(compiler), OpJump);

            chunk_patch_jump(compiler_chunk(compiler), if_neq);
            chunk_write(vm, compiler_chunk(compiler), OpPop);

            if (next_arm != NULL) {
                BUBBLE(compile(vm, compiler, next_arm));
            } else {
                chunk_write(vm, compiler_chunk(compiler), OpPop);
                chunk_write_constant(vm, compiler_chunk(compiler), new_nil());
            }

            chunk_patch_jump(compiler_chunk(compiler), past_else);
            break;
        }
        case AstntBlock: {
            Block *prev = compiler->block;
            i32 prev_offset = prev->offset;
            Block block = { .base = prev->base + prev->offset, .offset = 0, .depth = compiler->scope_depth, .prev = prev };
            compiler->block = &block;

            compiler_new_scope(compiler);
            AstNode *stmt = node->kids[0];
            for (; stmt != NULL && stmt->type == AstntNodeList && is_stmt(stmt->kids[0]->type);
                 stmt = stmt->kids[1]) {
                BUBBLE(compile(vm, compiler, stmt->kids[0]));
            }

            if (stmt != NULL) {
                BUBBLE(compile(vm, compiler, stmt));
            } else {
                chunk_write_constant(vm, compiler_chunk(compiler), new_nil());
            }

            compiler_end_scope(vm, compiler);
            compiler->block = prev;
            compiler->block->offset = prev_offset + 1;
            break;
        }
        case AstntExprStmt: {
            BUBBLE(compile(vm, compiler, node->kids[0]));
            chunk_write(vm, compiler_chunk(compiler), OpPop);
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

            LambString *ident = cstr_to_lambstring(vm, ident_node->val.i);
            i32 idx = chunk_add_constant(vm, compiler_chunk(compiler), new_object((Object *)ident));

            if (compiler->scope_depth == 0) {
                chunk_write(vm, compiler_chunk(compiler), OpDefineGlobal);
                if (idx >= 256) {
                    u8 hi = (idx >> 16) & 0xFF;
                    u8 mi = (idx >> 8) & 0xFF;
                    u8 lo = (idx >> 0) & 0xFF;

                    chunk_write(vm, compiler_chunk(compiler), OpLongConstant);
                    chunk_write(vm, compiler_chunk(compiler), hi);
                    chunk_write(vm, compiler_chunk(compiler), mi);
                    chunk_write(vm, compiler_chunk(compiler), lo);
                } else {
                    chunk_write(vm, compiler_chunk(compiler), OpConstant);
                    chunk_write(vm, compiler_chunk(compiler), (u8)idx);
                }
                STACK_DIFF(compiler, -1);
            } else {
                Local loc = {
                    .depth = compiler->scope_depth, .name = ident->chars, .is_captured = false};
                local_arr_write(vm, &compiler->locals, loc);
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
