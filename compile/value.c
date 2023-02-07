#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../memory.h"
#include "./object.h"
#include "./value.h"

Value new_nil() {
  Value val = {
      .kind = VkNil,
      .as.boolean = false,
  };
  return val;
}

Value new_boolean(bool b) {
  Value val = {.kind = VkBool, .as.boolean = b};
  return val;
}

Value new_int(i64 num) {
  Value val = {.kind = VkInt, .as.intn = num};
  return val;
}

Value new_double(double num) {
  Value val = {.kind = VkDouble, .as.doubn = num};
  return val;
}

Value new_char(char c) {
  Value val = {.kind = VkChar, .as.ch = c};
  return val;
}

Value new_object(Object *obj) {
  Value val = {.kind = VkObj, .as.obj = obj};
  return val;
}

bool is_nil(Value val) { return val.kind == VkNil; }

bool is_bool(Value val) { return val.kind == VkBool; }

bool is_double(Value val) { return val.kind == VkDouble; }

bool is_integer(Value val) { return val.kind == VkInt; }

bool is_char(Value val) { return val.kind == VkChar; }

bool is_object(Value val) { return val.kind == VkObj; }

void print_kind(Value val) {
  switch (val.kind) {
  case VkNil:
    printf("nil");
    break;
  case VkBool:
    printf("bool");
    break;
  case VkInt:
    printf("int");
    break;
  case VkDouble:
    printf("double");
    break;
  case VkChar:
    printf("char");
    break;
  case VkObj: {
    switch (val.as.obj->type) {
    case OtString:
      printf("string");
      break;
    case OtArray:
      printf("array");
      break;
    case OtFunc:
      printf("fn");
      break;
    case OtNative:
      printf("native fn");
      break;
    case OtClosure:
      printf("closure fn");
      break;
    case OtUpvalue:
      printf("upvalue");
      break;
    }
    break;
  }
  }
}

Order value_compare(Value *lhs, Value *rhs) {
  switch (rhs->kind) {
  case VkNil:
    return OrderEqual;
  case VkBool:
    if (lhs->as.boolean == rhs->as.boolean) {
      return OrderEqual;
    } else if (lhs->as.boolean) {
      return OrderGreater;
    } else {
      return OrderLess;
    }
  case VkInt:
    if (lhs->as.intn == rhs->as.intn) {
      return OrderEqual;
    } else if (lhs->as.intn > rhs->as.intn) {
      return OrderGreater;
    } else {
      return OrderLess;
    }
  case VkDouble:
    if (lhs->as.doubn == rhs->as.doubn) {
      return OrderEqual;
    } else if (lhs->as.doubn > rhs->as.doubn) {
      return OrderGreater;
    } else {
      return OrderLess;
    }
  case VkChar:
    if (lhs->as.ch == rhs->as.ch) {
      return OrderEqual;
    } else if (lhs->as.ch > rhs->as.ch) {
      return OrderGreater;
    } else {
      return OrderLess;
    }
  case VkObj:
    switch (rhs->as.obj->type) {
    case OtArray: {
      LambArray *larr = (LambArray *)lhs->as.obj;
      LambArray *rarr = (LambArray *)rhs->as.obj;

      i32 llen = larr->items.len;
      i32 rlen = rarr->items.len;
      i32 min_len = llen < rlen ? llen : rlen;

      Order element_order = OrderEqual;
      for (i32 i = 0; i < min_len; i++) {
        if ((element_order = value_compare(&larr->items.values[i],
                                           &rarr->items.values[i])) !=
            OrderEqual) {
          return element_order;
        }
      }

      if (llen == rlen) {
        return OrderEqual;
      } else if (llen > rlen) {
        return OrderGreater;
      } else {
        return OrderLess;
      }
    }
    case OtString: {
      LambString *larr = (LambString *)lhs->as.obj;
      LambString *rarr = (LambString *)rhs->as.obj;

      if (larr == rarr) {
        return OrderEqual;
      }

      i32 llen = larr->len;
      i32 rlen = rarr->len;
      i32 min_len = llen < rlen ? llen : rlen;

      for (i32 i = 0; i < min_len; i++) {
        if (larr->chars[i] > rarr->chars[i]) {
          return OrderGreater;
        } else if (larr->chars[i] < rarr->chars[i]) {
          return OrderLess;
        }
      }

      return llen < rlen ? OrderLess : OrderGreater;
    }
    case OtFunc:
    case OtNative:
    case OtClosure:
    case OtUpvalue:
      if (lhs->as.obj == rhs->as.obj) {
        return OrderEqual;
      } else if (lhs->as.obj > rhs->as.obj) {
        return OrderGreater;
      } else {
        return OrderLess;
      }
    }
  }

  fprintf(stderr, "Fallthrough found in switch in %s at %s", __FILE__,
          __FUNCTION__);
  return OrderLess;
}

void value_arr_init(ValueArray *arr) {
  arr->len = 0;
  arr->capacity = 0;
  arr->values = NULL;
}

void value_arr_write(ValueArray *arr, Value val) {
  if (arr->capacity < arr->len + 1) {
    i32 old_cap = arr->capacity;
    arr->capacity = GROW_CAPACITY(old_cap);
    arr->values = GROW_ARRAY(Value, arr->values, old_cap, arr->capacity);
  }

  arr->values[arr->len] = val;
  arr->len += 1;
}

void value_arr_free(ValueArray *arr) {
  FREE_ARRAY(Value, arr->values, arr->capacity);
  value_arr_init(arr);
}
