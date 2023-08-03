#include <algorithm>
#include <cstdlib>
#include <iostream>
#include <iterator>
#include <optional>
#include <ranges>

#include "../types.hpp"
#include "../vm/vm.hpp"
#include "chunk.hpp"
#include "compiler.hpp"
#include "gcvec.hpp"
#include "object.hpp"
#include "value.hpp"

Compiler::Compiler(Vm &vm, Compiler *enclosing, Block *block, FuncType type, char const *name,
                   i32 arity)
    : function(LambFunc::alloc(vm, name, arity)), enclosing(enclosing), block(block), type(type) {
    ;
    this->locals = GcVec<Local>();
    this->add_local(vm, "");
}

void Compiler::add_local(Vm &vm, char const *name) {
    Local loc = {.name = name, .depth = this->block->depth, .is_captured = false};
    this->locals.push(vm, loc);
}

std::optional<i32> Compiler::local_idx(LambString *name) {
    auto it = std::find_if(this->locals.rbegin(), this->locals.rend(),
                           [name](Local &loc) { return loc.name == name->chars; });

    // Ends of iterators point 1 past the actual end, and therefore we need `-1`
    auto dist = std::distance(it, this->locals.rend());
    return dist == 0 ? std::nullopt : std::optional{dist - 1};
}

std::optional<i32> Compiler::local_slot(LambString *name) {
    auto idx_ = this->local_idx(name);
    if (!idx_) {
        return std::nullopt;
    }

    auto idx = idx_.value();

    i32 depth = this->locals[idx].depth;
    i32 base = -1;
    for (Block *bl = this->block; bl != nullptr; bl = bl->prev) {
        if (bl->depth == depth) {
            base = bl->base;
            break;
        }
    }

    if (base == -1) {
        // TODO: You should probably include more debug info to help yourself in the
        //       future
        std::cerr << "[Lamb] Internal Compiler Error: No block found for variable '" << name
                  << "' at depth " << depth << '\n';

        exit(EXIT_FAILURE);
    }

    auto block_locals =
        this->locals | std::views::take(idx) | std::views::reverse |
        std::views::take_while([depth](auto const &local) { return local.depth == depth; });

    return base + std::ranges::distance(block_locals);
}

// NOLINTNEXTLINE(misc-no-recursion)
std::optional<i32> Compiler::upvalue_idx(LambString *name) {
    if (this->enclosing == nullptr) {
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
    if (idx > 255) {
        // TODO: Figure out how to get a dynamic amount of upvalues
        std::cerr << "[Lamb] There exist too many upvalues. Max is 255." << '\n';
        exit(EXIT_FAILURE);
    }

    auto count = this->function->upvalue_count;
    for (auto const &upvalue : std::views::counted(this->upvalues, count)) {
        if (upvalue.index == idx && upvalue.is_local == is_local) {
            return &upvalue - this->upvalues;
        }
    }

    this->upvalues[count].is_local = is_local;
    this->upvalues[count].index = idx;

    return this->function->upvalue_count++;
}

void Compiler::end_scope(Vm &vm) {
    this->block = this->block->prev;
    i32 depth = this->block == nullptr ? -1 : this->block->depth;

    this->function->chunk.write(vm, OpSaveValue);

    for (auto const &local : this->locals | std::views::reverse) {
        if (local.depth <= depth) {
            auto dist = &local - this->locals.as_raw();
            this->locals.truncate(dist + 1);
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

void Compiler::destroy(Vm &vm) { this->locals.destroy(vm); }

#define STACK_DIFF(diff) (this->block->offset += (diff))

void Compiler::write_op(Vm &vm, OpCode op) const {
    switch (op) {
        case OpAdd:
        case OpBinAnd:
        case OpBinOr:
        case OpBinXor:
        case OpCloseValue:
        case OpDefineGlobal:
        case OpDiv:
        case OpEq:
        case OpGe:
        case OpGt:
        case OpIndexArray:
        case OpLShift:
        case OpLe:
        case OpLt:
        case OpMod:
        case OpMul:
        case OpNe:
        case OpPop:
        case OpRShift:
        case OpSaveValue:
        case OpSub:
            STACK_DIFF(-1);
            break;
        case OpBinNeg:
        case OpLogNeg:
        case OpNumNeg:
            STACK_DIFF(0);
            break;
        case OpClosure:
        case OpConstant:
        case OpDup:
        case OpGetGlobal:
        case OpGetLocal:
        case OpGetUpvalue:
        case OpUnsaveValue:
            STACK_DIFF(1);
            break;
        case OpReturn:
            // Nothing after this is actually run, so techincally it doesn't
            // matter, but still I should fix this.
            STACK_DIFF(0);
            break;
        case OpCall:
        case OpJump:
        case OpJumpIfFalse:
        case OpJumpIfTrue:
        case OpMakeArray:
            // Invalid op... err and terminate
            std::cerr << "Invalid Op for Compiler::wirte_op: " << op << "\n";
            exit(EXIT_FAILURE);
            break;
    }

    this->chunk().write(vm, op);
}

void Compiler::write_op_arg(Vm &vm, Value val) const { this->chunk().write_const(vm, val); }

void Compiler::write_vop(Vm &vm, OpCode op, i32 args) const {
    switch (op) {
        case OpAdd:
        case OpBinAnd:
        case OpBinNeg:
        case OpBinOr:
        case OpBinXor:
        case OpCloseValue:
        case OpClosure:
        case OpConstant:
        case OpDefineGlobal:
        case OpDiv:
        case OpDup:
        case OpEq:
        case OpGe:
        case OpGetGlobal:
        case OpGetLocal:
        case OpGetUpvalue:
        case OpGt:
        case OpIndexArray:
        case OpJump:
        case OpJumpIfFalse:
        case OpJumpIfTrue:
        case OpLShift:
        case OpLe:
        case OpLogNeg:
        case OpLt:
        case OpMod:
        case OpMul:
        case OpNe:
        case OpNumNeg:
        case OpPop:
        case OpRShift:
        case OpReturn:
        case OpSaveValue:
        case OpSub:
        case OpUnsaveValue:
            // Invalid op... err and terminate
            std::cerr << "Invalid Op for Compiler::wirte_vop: " << op << "\n";
            exit(EXIT_FAILURE);
            break;
        case OpCall:
        case OpMakeArray:
            // Invalid op... err and terminate
            STACK_DIFF(-args);
            break;
    }

    this->chunk().write(vm, op);
}

void Compiler::write_const(Vm &vm, Value val) const {
    this->chunk().write_const(vm, val);
    STACK_DIFF(1);
}

void Compiler::write_byte(Vm &vm, u8 byte) { this->chunk().write(vm, byte); }

i32 Compiler::write_jump(Vm &vm, OpCode op) { return this->chunk().write_jump(vm, op); }

void Compiler::patch_jump(i32 jump_marker) { this->chunk().patch_jump(jump_marker); }

#undef STACK_DIFF
