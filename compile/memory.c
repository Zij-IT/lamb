#include "memory.h"
#include "vm.h"
#include "value.h"

#include <stdio.h>

#define GC_HEAP_GROWTH_FACTOR 2

// NOTE: Function requires access to Vm
// TODO: Add Vm parameter to function: mark_array(Vm *vm, ValueArray *arr)
static void mark_array(Vm* vm, ValueArray* arr) {
  for (i32 i = 0; i < arr->len; i++) {
    mark_value(vm, &arr->values[i]);
  }
}

// NOTE: Function requires access to Vm
// TODO: Add Vm parameter to function: blacken_object(Vm *vm, Object *obj)
static void blacken_object(Vm* vm, Object* obj) {
  #ifdef DEBUG_LOG_GC
  printf("Blackening %p with value: ", (void *)obj);
  print_value(new_object(obj));
  printf("\n");
  #endif
  
  switch(obj->type) {
  case OtNative:
  case OtString:
    break;
  case OtArray: {
    LambArray *arr = (LambArray *)obj;
    mark_array(vm, &arr->items);
    break;
    }
  case OtFunc: {
    LambFunc* func = (LambFunc*)obj;
    mark_object(vm, (Object*)func->name);
    mark_array(vm, &func->chunk.constants);
    break;
    }
  case OtClosure: {
    LambClosure* closure = (LambClosure*)obj;
    mark_object(vm, (Object*)closure->function);
    for (i32 i = 0; i < closure->upvalue_count; i++) {
      mark_object(vm, (Object*)closure->upvalues[i]);   
    }
    break;
    }
  case OtUpvalue: {
    mark_value(vm, &((LambUpvalue*)obj)->closed);
    break;
    }
  }
}

static void mark_roots(Vm* vm) {
  for (Value* slot = vm->stack; slot < vm->stack_top; slot++) {
    mark_value(vm, slot);
  }
  
  for (i32 i = 0; i < vm->frame_count; i++) {
    mark_object(vm, (Object*)vm->frames[i].closure);
  }
  
  for (LambUpvalue* upvalue = vm->open_upvalues; upvalue != NULL; upvalue = upvalue->next) {
    mark_object(vm, (Object*)upvalue);
  }
  
  mark_table(vm, &vm->globals);
  
  // mark current object in compilation as compilation can trigger GC
  // This means that the vm does have to have some link to the current compiler
  // mark_compiler_roots(vm->compiler);
}

static void trace_refs(Vm* vm) {
  while(vm->gray_stack.len > 0) {
    Object* obj = vm->gray_stack.values[--vm->gray_stack.len];
    blacken_object(vm, obj);
  }
}

static void sweep_unused(Vm* vm) {
  Object* prev = NULL;
  Object* curr = vm->poor_mans_gc;
  
  while (curr != NULL) {
    if (curr->is_marked) {
      prev = curr;
      curr = curr->next;
      curr->is_marked = false;
    } else {
      Object* unreached = curr;
      curr = curr->next;
      
      if (prev == NULL) {
        vm->poor_mans_gc = curr;
      } else {
        prev->next = curr;
      }
      
      object_free(vm, unreached);
    }
  }
}


// Work as specified in the table:
//
//  | old_size ----|---- new_size ----|---- operation ----|
//  |--------------|------------------|-------------------|
//  | 0            | non-zero         | allocate new block|
//  |---------------------------------|-------------------|
//  | non-zero     | smaller than old | shrink existing   |
//  |--------------|------------------|-------------------|
//  | non-zero     | larger than old  | grow exisitng     |
//  -------------------------------------------------------
void* reallocate(Vm* vm, void* ptr, size_t old_size, size_t new_size) {
  if (new_size > old_size) {
    vm->bytes_allocated += new_size - old_size;
  } else {
    vm->bytes_allocated -= old_size - new_size;
  }
  
  if(new_size == 0) {
    free(ptr);
    return NULL;
  }
  
  if (vm->bytes_allocated > vm->next_collection) {
    collect_garbage(vm);
    vm->next_collection = vm->bytes_allocated * GC_HEAP_GROWTH_FACTOR;
  }
  
  void* result = realloc(ptr, new_size);
  if (result == NULL) {
    fprintf(stderr, "LambCompiler: Ran out of memory... sorry :(");
    exit(1);
  }

  return result;
}

void mark_object(Vm* vm, Object* object) {
  if (object == NULL || object->is_marked) {
    return;
  }
  
  #ifdef DEBUG_LOG_GC
  printf("Marking %p with value: ", (void*)object);
  print_value(new_object(object));
  printf("\n");
  #endif

  object->is_marked = true;
  objectptr_array_write(vm, &vm->gray_stack, object);
}

void mark_value(Vm* vm, Value* value) {
  if (value->kind == VkObj) {
    mark_object(vm, (Object*)value->as.obj);
  }
}

void mark_table(Vm* vm, Table* table) {
  for (i32 i = 0; i < table->capacity; i++) {
    Entry* entry = &table->entries[i];
    mark_object(vm, (Object*)entry->key);
    mark_value(vm, &entry->val);
  }
}

void collect_garbage(Vm* vm) {
  #ifdef DEBUG_LOG_GC
  u64 before = vm->bytes_allocated;
  printf("====== GC Begin ======\n");
  #endif

  mark_roots(vm);

  #ifdef DEBUG_LOG_GC
  #endif

  trace_refs(vm);
  
  table_remove_white(&vm->strings);
  
  sweep_unused(vm);

  #ifdef DEBUG_LOG_GC
  printf("Collected %lu bytes (from %lu to %lu)\n", before - vm->bytes_allocated, before, vm->bytes_allocated);
  printf("Next collection at %lu bytes\n", vm->next_collection);
  printf("====== GC End ======\n");
  #endif
}
