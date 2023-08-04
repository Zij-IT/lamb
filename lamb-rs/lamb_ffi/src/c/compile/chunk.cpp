#include <algorithm>
#include <cstdint>
#include <cstdlib>
#include <iomanip>
#include <iostream>
#include <optional>
#include <ostream>
#include <sstream>
#include <string>
#include <tuple>
#include <vector>

#include "../types.hpp"
#include "../vm/vm.hpp"
#include "chunk.hpp"
#include "gcvec.hpp"
#include "object.hpp"
#include "value.hpp"

constexpr u8 byte = 8;

Chunk::Chunk() {
    this->bytes = GcVec<u8>();
    this->constants = GcVec<Value>();
}

void Chunk::destroy(Vm &vm) {
    this->bytes.destroy(vm);
    this->constants.destroy(vm);
}

void Chunk::write(Vm &vm, u8 byte) { this->bytes.push(vm, byte); }

i32 Chunk::write_jump(Vm &vm, u8 op) {
    constexpr auto dummy = 0;

    this->write(vm, op);
    this->write(vm, dummy);
    this->write(vm, dummy);

    return this->bytes.len() - 2;
}

void Chunk::patch_jump(i32 offset) const {
    i32 jump = this->bytes.len() - offset - 2;
    if (jump > UINT16_MAX) {
        std::cerr << "COMPILE_ERR: jump exceeds maximal bytes of " << UINT16_MAX << '\n';
        exit(1);
    }

    this->bytes[offset] = (jump >> byte);
    this->bytes[offset + 1] = jump;
}

void Chunk::write_const(Vm &vm, Value val) {
    i32 idx = this->add_const(vm, val);
    u8 hi = (idx >> byte);
    u8 lo = idx;

    this->write(vm, OpConstant);
    this->write(vm, hi);
    this->write(vm, lo);
}

i32 Chunk::add_const(Vm &vm, Value val) {
    vm.push_stack(val);
    this->constants.push(vm, val);
    vm.pop_stack();
    return this->constants.len() - 1;
}

// NOLINTBEGIN(cppcoreguidelines-avoid-magic-numbers,readability-magic-numbers)
std::string Chunk::to_string() {
    std::vector<std::tuple<std::string, std::string, std::string>> vec;

    i32 offset = 0;
    i32 max_left = 10;
    i32 max_right = 10;
    i32 max_digit = 5;
    while (offset < this->bytes.len()) {
        auto [left, rhs, inst_off] = this->format_instruction(offset);
        auto right = rhs.value_or("");
        auto str_offset = std::to_string(offset);
        vec.emplace_back(left, right, str_offset);

        max_left = std::max(max_left, (i32)left.length());
        max_right = std::max(max_right, (i32)right.length());
        max_digit = std::max(max_digit, (i32)str_offset.length());
        offset += inst_off;
    }

    max_left = std::min(max_left, 20);
    max_right = std::min(max_right, 20);
    max_digit = std::min(max_digit, 20);

    std::ostringstream out;
    out << std::setw(max_digit) << std::left << "Byte"
        << " | " << std::setw(max_left) << std::left << "OpCode"
        << " | " << std::setw(max_right) << std::left << "Extra" << '\n'
        << std::string(max_digit, '=') << "=#=" << std::string(max_left, '=')
        << "=#=" << std::string(max_right, '=') << '\n';

    for (auto const &[left, right, offset] : vec) {
        out << std::setw(max_digit) << std::left << offset << " | " << std::setw(max_left)
            << std::left << left << " | " << std::setw(max_right) << std::left << right << '\n';
    }

    return out.str();
}
// NOLINTEND(cppcoreguidelines-avoid-magic-numbers,readability-magic-numbers)

namespace {
std::string format_jump(Chunk const &chunk, i32 offset) {
    u16 jump = (((u16)chunk.bytes[offset + 1]) << byte) | chunk.bytes[offset + 2];

    std::ostringstream out;
    out << offset << " -> " << offset + 3 + jump;

    return out.str();
}
} // namespace

// std::tuple<std::string, std::optional<std::string>, u32>
// SIMPLE(name) std::make_tuple(name, std::nullopt, 1)

// NOLINTNEXTLINE(misc-no-recursion)
std::tuple<std::string, std::optional<std::string>, u32>
Chunk::format_instruction(u32 offset) const {
#define SIMPLE(name) (std::make_tuple(name, std::nullopt, 1))
#define CONSTANT(name)                                                                             \
    (std::make_tuple(                                                                              \
        (name),                                                                                    \
        this->constants[(this->bytes[(offset) + 1] << 8) | this->bytes[(offset) + 2]].to_string(), \
        3))
#define JUMP(name) (std::make_tuple((name), format_jump(*this, offset), 3))

    switch ((OpCode)this->bytes[offset]) {
        case OpConstant:
            return CONSTANT("OpConstant");
        case OpDefineGlobal:
            return SIMPLE("OpDefineGlobal");
        case OpGetGlobal:
            return SIMPLE("OpGetGlobal");
        case OpGetLocal:
            return SIMPLE("OpGetLocal");
        case OpJump:
            return JUMP("OpJump");
        case OpJumpIfTrue:
            return JUMP("OpJumpIfTrue");
        case OpJumpIfFalse:
            return JUMP("OpJumpIfFalse");
        case OpNumNeg:
            return SIMPLE("OpNumNeg");
        case OpBinNeg:
            return SIMPLE("OpBinNeg");
        case OpLogNeg:
            return SIMPLE("OpLogNeg");
        case OpAdd:
            return SIMPLE("OpAdd");
        case OpSub:
            return SIMPLE("OpSub");
        case OpMul:
            return SIMPLE("OpMul");
        case OpMod:
            return SIMPLE("OpMod");
        case OpDiv:
            return SIMPLE("OpDiv");
        case OpBinAnd:
            return SIMPLE("OpBinAnd");
        case OpBinOr:
            return SIMPLE("OpBinOr");
        case OpBinXor:
            return SIMPLE("OpBinXor");
        case OpEq:
            return SIMPLE("OpEq");
        case OpNe:
            return SIMPLE("OpNe");
        case OpGt:
            return SIMPLE("OpGt");
        case OpGe:
            return SIMPLE("OpGe");
        case OpLt:
            return SIMPLE("OpLt");
        case OpLe:
            return SIMPLE("OpLe");
        case OpRShift:
            return SIMPLE("OpRShift");
        case OpLShift:
            return SIMPLE("OpLShift");
        case OpReturn:
            return SIMPLE("OpReturn");
        case OpLen:
            return SIMPLE("OpLen");
        case OpMakeArray:
            return SIMPLE("OpMakeArray");
        case OpIndex:
            return SIMPLE("OpIndex");
        case OpIndexRev:
            return SIMPLE("OpIndexRev");
        case OpPop:
            return SIMPLE("OpPop");
        case OpDup:
            return SIMPLE("OpDup");
        case OpCall:
            return SIMPLE("OpCall");
        case OpSaveValue:
            return SIMPLE("OpSaveValue");
        case OpUnsaveValue:
            return SIMPLE("OpUnsaveValue");
        case OpCloseValue:
            return SIMPLE("OpCloseValue");
        case OpGetUpvalue:
            return SIMPLE("OpGetUpvalue");
        case OpClosure: {
            // Explanation:
            // * 1 to jump over OpConstant
            // * 1 to jump over high byte
            // * 1 to jump over low  byte
            // * 2 to jump over each upvalue (is_local & index)
            i32 idx = (this->bytes[offset + 2] << byte) | this->bytes[offset + 3];
            auto *func = (LambFunc *)this->constants[idx].as.obj;

            std::string lhs = "OpClosure";
            i32 offset = 1 + 1 + 2 + 2 * func->upvalue_count;

            return std::make_tuple("OpClosure", func->obj.to_string(), offset);
        }
    }

    std::cerr << "[Lamb] Internal compilerer error: Missing switch branch for " << offset << '\n';
    exit(EXIT_FAILURE);

#undef JUMP
#undef CONSTANT
#undef SIMPLE
}
