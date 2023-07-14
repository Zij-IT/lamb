#include "gc.hpp"
#include "../vm/vm.hpp"
#include "compiler.hpp"
#include <iostream>

#define GC_HEAP_GROWTH_FACTOR 2
#define KB 1024

void* MarkAndSweep::alloc(Vm& vm, size_t t_size, size_t count) {
    auto size = count * t_size;
    vm.bytes_allocated += size;

    if (vm.bytes_allocated > vm.next_collection) {
        vm.next_collection = vm.bytes_allocated * GC_HEAP_GROWTH_FACTOR;
        this->collect(vm);
    }

    auto result = malloc(size);
    if (result == NULL) {
        fprintf(stderr, "[Lamb]: Gc: Allocation failed... exiting...");
        exit(1);
    }

    return result;
}

void* MarkAndSweep::grow_array(Vm& vm, void* ptr, size_t t_size, size_t old_count, size_t new_count) {
    if (new_count < old_count) {
        return ptr;
    }
    
    auto size = new_count * t_size;
    vm.bytes_allocated += size - old_count * t_size;

    if (vm.bytes_allocated > vm.next_collection) {
        vm.next_collection = vm.bytes_allocated * GC_HEAP_GROWTH_FACTOR;
        this->collect(vm);
    }

    auto result = realloc(ptr, size);
    if (result == NULL) {
        fprintf(stderr, "[Lamb]: Gc: Allocation failed... exiting...");
        exit(1);
    }

    return result;
}

void MarkAndSweep::free(Vm& vm, void* ptr, size_t t_size) {
    vm.bytes_allocated -= t_size;
   ::free(ptr);
}

void MarkAndSweep::free_array(Vm& vm, void* ptr, size_t t_size, size_t len) {
    vm.bytes_allocated -= t_size * len;
    ::free(ptr);
}

void MarkAndSweep::collect(Vm& vm) {
    this->mark_roots(vm);

    this->trace_refs(vm);

    vm.strings.remove_marked();

    this->sweep_unused(vm);
} 

void MarkAndSweep::mark_roots(Vm& vm) {
    this->mark_value(vm, &vm.saved_value);

    for (Value *slot = vm.stack; slot < vm.stack_top; slot++) {
        this->mark_value(vm, slot);
    }

    for (i32 i = 0; i < vm.frame_count; i++) {
        this->mark_object(vm, (Object *)vm.frames[i].closure);
    }

    for (LambUpvalue *upvalue = vm.open_upvalues; upvalue != NULL; upvalue = upvalue->next) {
        this->mark_object(vm, (Object *)upvalue);
    }

    this->mark_table(vm, vm.globals);

    // mark current object in compilation as compilation can trigger GC
    // This means that the vm does have to have some link to the current compiler
    this->mark_compiler(vm, vm.curr_compiler);
}

void MarkAndSweep::mark_compiler(Vm& vm, Compiler* compiler) {
    while (compiler != NULL) {
        mark_object(vm, (Object *)compiler->function);
        compiler = compiler->enclosing;
    }
}

void MarkAndSweep::mark_table(Vm& vm, Table& table) {
    for (i32 i = 0; i < table.capacity; i++) {
        Entry *entry = &table.entries[i];
        mark_object(vm, (Object *)entry->key);
        mark_value(vm, &entry->val);
    }
}

void MarkAndSweep::trace_refs(Vm& vm) {
    while (vm.gray_stack.size() > 0) {
        this->mark_object(vm, vm.gray_stack.back());
        vm.gray_stack.pop_back();
    }
}

void MarkAndSweep::sweep_unused(Vm& vm) {
    Object *prev = NULL;
    Object *curr = vm.objects;

    while (curr != NULL) {
        if (curr->is_marked) {
            curr->is_marked = false;
            prev = curr;
            curr = curr->next;
        } else {
            Object *unreached = curr;
            curr = curr->next;

            if (prev == NULL) {
                vm.objects = curr;
            } else {
                prev->next = curr;
            }

            object_free(vm, unreached);
        }
    }
}

void MarkAndSweep::mark_value(Vm& vm, Value *value) {
    if (value->kind == VkObj) {
        mark_object(vm, (Object *)value->as.obj);
    }
}

void MarkAndSweep::mark_object(Vm& vm, Object *object) {
    if (object == NULL || object->is_marked) {
        return;
    }

    object->is_marked = true;
    vm.gray_stack.push_back(object);
}
