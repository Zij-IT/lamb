#include "object.h"
#include "vm.h"
#include <stdlib.h>

Object* alloc_obj(Vm* vm, ObjectType type) {
  switch(type) {
    case OtString: {
      LambString* st = malloc(sizeof(LambString));
      Object obj = { .type = type, };
      st->obj = obj;
      st->chars = NULL;
      st->len = 0;
      return (Object*)st;
    }
  }
  
  return NULL;
}

bool is_of_type(Object* obj, ObjectType type) {
  return obj->type == type;
}

