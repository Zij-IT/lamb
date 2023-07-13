#include <stdio.h>
#include <stdlib.h>

#include "../vm/vm.hpp"
#include "./chunk.hpp"
#include "./value.hpp"
#include "memory.hpp"

Chunk::Chunk() {
    this->bytes = GcVec<u8>();
    this->constants = GcVec<Value>();
}

void Chunk::destroy(Vm& vm) {
    this->bytes.destroy(vm);
    this->constants.destroy(vm);
}

void Chunk::write(Vm& vm, u8 byte) {
    this->bytes.push(vm, byte);
}

i32 Chunk::write_jump(Vm& vm, u8 op) {
    this->write(vm, op);
    this->write(vm, 0xff);
    this->write(vm, 0xff);

    return this->bytes.len() - 2;
}

void Chunk::patch_jump(i32 offset) {
    i32 jump = this->bytes.len() - offset - 2;
    if (jump > UINT16_MAX) {
        fprintf(stderr, "COMPILE_ERR: jump exceeds maximal bytes of %d", UINT16_MAX);
        exit(1);
    }

    this->bytes[offset] = (jump >> 8) & 0xff;
    this->bytes[offset + 1] = jump & 0xff;
}

void Chunk::write_const(Vm& vm, Value val) {
    i32 idx = this->add_const(vm, val);
    u8 hi = (idx >> 8) & 0xFF;
    u8 lo = idx & 0xFF;

    this->write(vm, OpConstant);
    this->write(vm, hi);
    this->write(vm, lo);
}

i32 Chunk::add_const(Vm& vm, Value val) {
    vm_push_stack(vm, val);
    this->constants.push(vm, val);
    vm_pop_stack(vm);
    return this->constants.len() - 1;
}
