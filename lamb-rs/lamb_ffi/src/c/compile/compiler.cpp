#include "compiler.hpp"
#include "chunk.hpp"
#include "memory.hpp"
#include "table.hpp"
#include <stdlib.h>

Compiler::Compiler(Vm* vm, Compiler* enclosing, Block* block, FuncType type, char const* name, i32 arity) {
    this->block = block;
    this->type = type;
    this->enclosing = enclosing;

    this->function = (LambFunc *)alloc_obj(vm, OtFunc);
    this->function->name = name;
    this->function->arity = arity;

    local_arr_init(&this->locals);

    Local loc = {.name = "", .depth = 0, .is_captured = false};
    local_arr_write(vm, &this->locals, loc);
}

void Compiler::new_scope() {
    this->block->depth++;
}

void Compiler::end_scope(Vm* vm) {
    this->block = this->block->prev;
    i32 depth = this->block == NULL ? -1 : this->block->depth;

    chunk_write(vm, &this->function->chunk, OpSaveValue);

    while (this->locals.len > 0 &&
           this->locals.values[this->locals.len - 1].depth > depth) {
        if (this->locals.values[this->locals.len - 1].is_captured) {
            chunk_write(vm, &this->function->chunk, OpCloseValue);
        } else {
            chunk_write(vm, &this->function->chunk, OpPop);
        }
        this->locals.len--;
    }

    chunk_write(vm, &this->function->chunk, OpUnsaveValue);
}

void Compiler::destroy(Vm* vm) {
    local_arr_free(vm, &this->locals);
    local_arr_init(&this->locals);
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
