#include "gc.hpp"
#include "../vm/vm.hpp"
#include <iostream>

#define GC_HEAP_GROWTH_FACTOR 2
#define KB 1024

void* MarkAndSweep::reallocate(Vm& vm, void *ptr, size_t old_size, size_t new_size) {
    if (new_size > old_size) {
        vm.bytes_allocated += new_size - old_size;
    } else {
        vm.bytes_allocated -= old_size - new_size;
    }

    if (new_size == 0) {
        free(ptr);
        return NULL;
    }

    if (vm.bytes_allocated > vm.next_collection) {
        vm.next_collection = vm.bytes_allocated * GC_HEAP_GROWTH_FACTOR;
        this->collect(vm);
    }

    void *result = realloc(ptr, new_size);
    if (result == NULL) {
        fprintf(stderr, "[Lamb]: Gc: Reallocation failed... exiting...");
        exit(1);
    }

    return result;
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

template<typename T>
void MarkAndSweep::mark_gcvec(Vm& vm, GcVec<T>& vec) {
    for (i32 i = 0; i < vec.len(); i++) {
        this->mark_value(vm, vec);
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
