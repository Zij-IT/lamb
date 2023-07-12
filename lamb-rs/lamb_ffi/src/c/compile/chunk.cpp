#include <stdio.h>
#include <stdlib.h>

#include "../vm/vm.hpp"
#include "./chunk.hpp"
#include "./value.hpp"
#include "memory.hpp"

Chunk::Chunk() {
    this->len = 0;
    this->capacity = 0;
    this->bytes = NULL;
    value_arr_init(&this->constants);
}

void Chunk::destroy(Vm* vm) {
    FREE_ARRAY(vm, u8, this->bytes, this->capacity);
    value_arr_free(vm, &this->constants);

    this->len = 0;
    this->capacity = 0;
    this->bytes = NULL;
}

void Chunk::write(Vm* vm, u8 byte) {
    if (this->capacity < this->len + 1) {
        i32 old_cap = this->capacity;
        this->capacity = GROW_CAPACITY(old_cap);
        this->bytes = GROW_ARRAY(vm, u8, this->bytes, old_cap, this->capacity);
    }

    this->bytes[this->len] = byte;
    this->len += 1;
}

i32 Chunk::write_jump(Vm* vm, u8 op) {
    this->write(vm, op);
    this->write(vm, 0xff);
    this->write(vm, 0xff);

    return this->len - 2;
}

void Chunk::patch_jump(i32 offset) {
    i32 jump = this->len - offset - 2;
    if (jump > UINT16_MAX) {
        fprintf(stderr, "COMPILE_ERR: jump exceeds maximal bytes of %d", UINT16_MAX);
        exit(1);
    }

    this->bytes[offset] = (jump >> 8) & 0xff;
    this->bytes[offset + 1] = jump & 0xff;
}

void Chunk::write_const(Vm* vm, Value val) {
    i32 idx = this->add_const(vm, val);
    u8 hi = (idx >> 8) & 0xFF;
    u8 lo = idx & 0xFF;

    this->write(vm, OpConstant);
    this->write(vm, hi);
    this->write(vm, lo);
}

i32 Chunk::add_const(Vm* vm, Value val) {
    vm_push_stack(vm, val);
    value_arr_write(vm, &this->constants, val);
    vm_pop_stack(vm);
    return this->constants.len - 1;
}
