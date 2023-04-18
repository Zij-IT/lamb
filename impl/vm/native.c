#include "native.h"
#include "../compile/compiler.h"
#include "../compile/debug.h"
#include "../compile/misc.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static NativeFunc *new_native(Vm *vm, CFunc cfunc) {
  NativeFunc *native_func = (NativeFunc *)alloc_obj(vm, OtNative);
  native_func->func = cfunc;
  return native_func;
}

static void define_native(Vm *vm, str fn_name, CFunc function) {
  LambString *interned = cstr_to_lambstring(vm, fn_name);
  vm_push_stack(vm, new_object((Object *)interned));
  vm_push_stack(vm, new_object((Object *)new_native(vm, function)));
  table_insert(vm, &vm->globals, (LambString *)(vm->stack[0].as.obj), vm->stack[1]);

  vm_pop_stack(vm);
  vm_pop_stack(vm);
}

static Value lamb_print(i32 arg_count, Value *args) {
  if (arg_count != 0) {
    print_value(*args);
  }

  return new_nil();
}

static Value lamb_println(i32 arg_count, Value *args) {
  if (arg_count != 0) {
    print_value(*args);
  }
  printf("\n");
  return new_nil();
}

static Value lamb_user_int(i32 arg_count, Value *args) {
  char x_buffer[80];
  if (fgets(x_buffer, 80, stdin) == NULL) {
    return new_nil();
  } else {
    return new_int(atoi(x_buffer));
  }
}

static Value lamb_user_char(i32 arg_count, Value *args) {
  return new_char(fgetc(stdin));
}

static Value lamb_rand(i32 arg_count, Value *args) {
  if (arg_count == 1 && is_integer(*args)) {
    return new_int(rand() % args->as.intn);
  }

  return new_int(rand());
}

static Value lamb_len(i32 arg_count, Value *args) {
  if (arg_count == 1 && args->kind == VkObj) {
    switch (args->as.obj->type) {
    case OtArray:
      return new_int(((LambArray *)args->as.obj)->items.len);
    case OtString:
      return new_int(((LambString *)args->as.obj)->len);
    default:
      return new_nil();
    }
  }

  return new_nil();
}

void set_natives(Vm *vm) {
  define_native(vm, "print", lamb_print);
  define_native(vm, "println", lamb_println);
  define_native(vm, "user_int", lamb_user_int);
  define_native(vm, "user_char", lamb_user_char);
  define_native(vm, "rand", lamb_rand);
  define_native(vm, "len", lamb_len);
}
