#include "gc.hpp"
#include "../vm/vm.hpp"
#include "compiler.hpp"
#include <iostream>

#define GC_HEAP_GROWTH_FACTOR 2
#define KB 1024

void* MarkAndSweep::alloc(Vm& vm, size_t t_size, size_t count) {
    auto size = count * t_size;
    this->bytes_allocated += size;

    if (this->bytes_allocated > this->next_collection) {
        this->next_collection = this->bytes_allocated * GC_HEAP_GROWTH_FACTOR;
        this->collect(vm);
    }

    auto result = malloc(size);
    if (result == NULL) {
        fprintf(stderr, "[Lamb]: Gc: Allocation failed... exiting...");
        exit(1);
    }

    return result;
}

void MarkAndSweep::add_object(Object* obj) {
    obj->next = this->objects;
    this->objects = obj;
}

void* MarkAndSweep::grow_array(Vm& vm, void* ptr, size_t t_size, size_t old_count, size_t new_count) {
    if (new_count < old_count) {
        return ptr;
    }
    
    auto size = new_count * t_size;
    this->bytes_allocated += size - old_count * t_size;

    if (this->bytes_allocated > this->next_collection) {
        this->next_collection = this->bytes_allocated * GC_HEAP_GROWTH_FACTOR;
        this->collect(vm);
    }

    auto result = realloc(ptr, size);
    if (result == NULL) {
        fprintf(stderr, "[Lamb]: Gc: Allocation failed... exiting...");
        exit(1);
    }

    return result;
}

void MarkAndSweep::free(void* ptr, size_t t_size) {
    this->bytes_allocated -= t_size;
   ::free(ptr);
}

void MarkAndSweep::free_array(void* ptr, size_t t_size, size_t len) {
    this->bytes_allocated -= t_size * len;
    ::free(ptr);
}

void MarkAndSweep::destroy(Vm& vm) {
     Object *obj = this->objects;
     while (obj != NULL) {
         Object *next = obj->next;
         object_free(vm, obj);
         obj = next;
     }
}

void MarkAndSweep::collect(Vm& vm) {
    this->mark_roots(vm);

    this->trace_refs();

    vm.strings.remove_marked();

    this->sweep_unused(vm);
} 

void MarkAndSweep::mark_roots(Vm& vm) {
    this->mark_value(&vm.saved_value);

    for (Value *slot = vm.stack; slot < vm.stack_top; slot++) {
        this->mark_value(slot);
    }

    for (i32 i = 0; i < vm.frame_count; i++) {
        this->mark_object((Object *)vm.frames[i].closure);
    }

    for (LambUpvalue *upvalue = vm.open_upvalues; upvalue != NULL; upvalue = upvalue->next) {
        this->mark_object((Object *)upvalue);
    }

    this->mark_table(vm.globals);

    // mark current object in compilation as compilation can trigger GC
    // This means that the vm does have to have some link to the current compiler
    this->mark_compiler(vm.curr_compiler);
}

void MarkAndSweep::mark_compiler(Compiler* compiler) {
    while (compiler != NULL) {
        this->mark_object((Object *)compiler->function);
        compiler = compiler->enclosing;
    }
}

void MarkAndSweep::mark_table(Table& table) {
    for (i32 i = 0; i < table.capacity; i++) {
        Entry *entry = &table.entries[i];
        mark_object((Object *)entry->key);
        mark_value(&entry->val);
    }
}

void MarkAndSweep::trace_refs() {
    while (this->gray_stack.size() > 0) {
        this->mark_object(this->gray_stack.back());
        this->gray_stack.pop_back();
    }
}

void MarkAndSweep::sweep_unused(Vm& vm) {
    Object *prev = NULL;
    Object *curr = this->objects;

    while (curr != NULL) {
        if (curr->is_marked) {
            curr->is_marked = false;
            prev = curr;
            curr = curr->next;
        } else {
            Object *unreached = curr;
            curr = curr->next;

            if (prev == NULL) {
                this->objects = curr;
            } else {
                prev->next = curr;
            }

            object_free(vm, unreached);
        }
    }
}

void MarkAndSweep::mark_value(Value *value) {
    if (value->kind == VkObj) {
        mark_object((Object *)value->as.obj);
    }
}

void MarkAndSweep::mark_object(Object *object) {
    if (object == NULL || object->is_marked) {
        return;
    }

    object->is_marked = true;
    this->gray_stack.push_back(object);
}
