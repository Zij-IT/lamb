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

    this->locals = GcVec<Local>();

    Local loc = {.name = "", .depth = 0, .is_captured = false};
    this->locals.push(vm, loc);
}

void Compiler::add_local(Vm* vm, char const* name) {
    Local loc = {
        .name = name, .depth = this->block->depth, .is_captured = false};
    this->locals.push(vm, loc);
}

void Compiler::new_scope() {
    this->block->depth++;
}

void Compiler::end_scope(Vm* vm) {
    this->block = this->block->prev;
    i32 depth = this->block == NULL ? -1 : this->block->depth;

    this->function->chunk.write(vm, OpSaveValue);

    for(auto x = this->locals.len() - 1; x >= 0; x--) {
        auto local = this->locals[x];
        if (local.depth <= depth) { 
            this->locals.truncate(x + 1);
            break; 
        }

        if (local.is_captured) {
            this->function->chunk.write(vm, OpCloseValue);
        } else {
            this->function->chunk.write(vm, OpPop);
        }
    }
    
    this->function->chunk.write(vm, OpUnsaveValue);
}

Chunk* Compiler::chunk() {
    return &this->function->chunk;
}

void Compiler::destroy(Vm* vm) {
    this->locals.destroy(vm);
}
