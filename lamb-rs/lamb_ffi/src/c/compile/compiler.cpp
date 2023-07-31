#include "compiler.hpp"
#include "chunk.hpp"
#include "table.hpp"
#include <stdlib.h>
#include <iostream>

Compiler::Compiler(Vm& vm, Compiler* enclosing, Block* block, FuncType type, char const* name, i32 arity) {
    this->block = block;
    this->type = type;
    this->enclosing = enclosing;
    this->function = LambFunc::alloc(vm, name, arity);
    this->locals = GcVec<Local>();

    Local loc = {.name = "", .depth = 0, .is_captured = false};
    this->locals.push(vm, loc);
}

void Compiler::add_local(Vm& vm, char const* name) {
    Local loc = {
        .name = name, .depth = this->block->depth, .is_captured = false};
    this->locals.push(vm, loc);
}

std::optional<i32> Compiler::local_idx(LambString *name) {
    for (i32 i = this->locals.len() - 1; i >= 0; i--) {
        Local *local = &this->locals[i];
        if (local->name == name->chars) {
            return i;
        }
    }

    return std::nullopt;
}

std::optional<i32> Compiler::local_slot(Vm& vm, LambString *name) {
    auto idx_ = this->local_idx(name);    
    if (!idx_) {
        return std::nullopt;
    }

    auto idx = idx_.value();

    i32 depth = this->locals[idx].depth;
    i32 base = -1;
    for (Block *bl = this->block; bl != NULL; bl = bl->prev) {
        if (bl->depth == depth) {
            base = bl->base;
            break;
        }
    }

    if (base == -1) {
        // TODO: You should probably include more debug info to help yourself in the
        //       future
        std::cerr << "[Lamb] Internal Compiler Error: No block found for variable '"
                  << name << "' at depth " << depth
                  << std::endl;

        std::exit(EXIT_FAILURE);
    }

    i32 local_idx = 0;
    for (i32 idx = idx - 1; idx >= 0 && this->locals[idx].depth == depth; idx--) {
        local_idx++;
    }

    return base + local_idx;
}

std::optional<i32> Compiler::upvalue_idx(LambString *name) {
    if (this->enclosing == NULL) {
        return std::nullopt;
    }

    auto idx = this->enclosing->local_idx(name);
    if (idx) {
        i32 idx_ = idx.value();
        this->enclosing->locals[idx_].is_captured = true;
        return this->add_upvalue(idx_, true);
    }

    auto upvalue = this->enclosing->upvalue_idx(name);
    if (upvalue) {
        return this->add_upvalue(upvalue.value(), false);
    }

    return std::nullopt;
}

std::optional<i32> Compiler::add_upvalue(i32 idx, bool is_local) {
        int count = this->function->upvalue_count;

    if (idx > 255) {
        // TODO: Figure out how to get a dynamic amount of upvalues
        std::cerr << "[Lamb] There exist too many upvalues. Max is 255." << std::endl;
        exit(EXIT_FAILURE);
    }

    for (i32 i = 0; i < count; i++) {
        Upvalue *upvalue = &this->upvalues[i];
        if (upvalue->index == idx && upvalue->is_local == is_local) {
            return i;
        }
    }

    this->upvalues[count].is_local = is_local;
    this->upvalues[count].index = idx;

    return this->function->upvalue_count++;
}

void Compiler::end_scope(Vm& vm) {
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

void Compiler::destroy(Vm& vm) {
    this->locals.destroy(vm);
}
