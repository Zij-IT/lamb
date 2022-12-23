#include <stdlib.h>

#include "object.h"
#include "vm.h"
#include "../memory.h"

Object* alloc_obj(Vm* vm, ObjectType type) {
  switch(type) {
    case OtString: {
      LambString* st = malloc(sizeof(LambString));
      Object obj = { .type = type, .next = vm->poor_mans_gc, };
      st->obj = obj;
      st->chars = NULL;
      st->len = 0;
      vm->poor_mans_gc = (Object*)st;
      return vm->poor_mans_gc;
    }
  }
  
  return NULL;
}

void object_free(Object* obj) {
  switch(obj->type) {
    case OtString: {
      LambString* st = (LambString*)obj;
      FREE_ARRAY(char, st->chars, st->len + 1);
      FREE(LambString, st);
      break;
    }
  }
}

bool is_of_type(Object* obj, ObjectType type) {
  return obj->type == type;
}

