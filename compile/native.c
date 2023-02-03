#include <stdio.h>
#include <string.h>
#include "debug.h"
#include "native.h"
#include "compiler.h"
#include "misc.h"

static NativeFunc* new_native(Vm* vm, CFunc cfunc) {
  NativeFunc* native_func = (NativeFunc*)alloc_obj(vm, OtNative);
  native_func->func = cfunc;
  return native_func;
}

static void define_native(Vm* vm, str fn_name, CFunc function) {
  u64 len = strlen(fn_name);
  u32 hash = hash_string(fn_name);
  LambString* interned = table_find_string(&vm->strings, fn_name, len, hash);
  if (interned == NULL) {
    interned = (LambString*)alloc_obj(vm, OtString);
    interned->chars = strdup(fn_name);
    interned->hash = hash;
    interned->len = len;
    
    table_insert(&vm->strings, interned, new_boolean(false)); 
  }

  vm_push_stack(vm, new_object((Object*)interned));
  vm_push_stack(vm, new_object((Object*)new_native(vm, function)));
  table_insert(&vm->globals, (LambString*)(vm->stack[0].as.obj), vm->stack[1]);
  
  vm_pop_stack(vm);
  vm_pop_stack(vm);
}

static Value print(i32 arg_count, Value* args) {
  print_value(*args);
  return new_nil();
}

void set_natives(Vm* vm) {
  define_native(vm, "print", print);
}
