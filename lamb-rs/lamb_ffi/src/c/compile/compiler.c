#include "compiler.h"
#include "chunk.h"
#include "memory.h"
#include "table.h"
#include <stdlib.h>

void compiler_init(Vm *vm, Compiler *compiler, FuncType type) {
    compiler->enclosing = NULL;
    compiler->function = NULL;
    compiler->block = NULL;
    compiler->scope_depth = 0;
    compiler->type = type;
    local_arr_init(&compiler->locals);

    Local loc = {.name = "", .depth = 0, .is_captured = false};
    local_arr_write(vm, &compiler->locals, loc);
}

void compiler_free(Vm *vm, Compiler *compiler) {
    local_arr_free(vm, &compiler->locals);
    local_arr_init(&compiler->locals);
}

void compiler_new_scope(Compiler *compiler) {
    compiler->scope_depth++;
    compiler->block->depth++;
}

void compiler_end_scope(Vm *vm, Compiler *compiler) {
    compiler->scope_depth--;
    chunk_write(vm, &compiler->function->chunk, OpSaveValue);

    while (compiler->locals.len > 0 &&
           compiler->locals.values[compiler->locals.len - 1].depth > compiler->scope_depth) {
        if (compiler->locals.values[compiler->locals.len - 1].is_captured) {
            chunk_write(vm, &compiler->function->chunk, OpCloseValue);
        } else {
            chunk_write(vm, &compiler->function->chunk, OpPop);
        }
        compiler->locals.len--;
    }
    chunk_write(vm, &compiler->function->chunk, OpUnsaveValue);
}

void local_arr_init(LocalArray *arr) {
    arr->len = 0;
    arr->capacity = 0;
    arr->values = NULL;
}

void local_arr_write(Vm *vm, LocalArray *arr, Local val) {
    if (arr->capacity < arr->len + 1) {
        i32 old_cap = arr->capacity;
        arr->capacity = GROW_CAPACITY(old_cap);
        arr->values = GROW_ARRAY(vm, Local, arr->values, old_cap, arr->capacity);
    }

    arr->values[arr->len] = val;
    arr->len += 1;
}

void local_arr_free(Vm *vm, LocalArray *arr) {
    FREE_ARRAY(vm, Value, arr->values, arr->capacity);
    local_arr_init(arr);
}
