#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "./object.hpp"
#include "./value.hpp"

Order Value::cmp(Value const& rhs) const {
    switch (rhs.kind) {
        case VkNil:
            return OrderEqual;
        case VkBool:
            if (this->as.boolean == rhs.as.boolean) {
                return OrderEqual;
            } else if (this->as.boolean) {
                return OrderGreater;
            } else {
                return OrderLess;
            }
        case VkInt:
            if (this->as.intn == rhs.as.intn) {
                return OrderEqual;
            } else if (this->as.intn > rhs.as.intn) {
                return OrderGreater;
            } else {
                return OrderLess;
            }
        case VkDouble:
            if (this->as.doubn == rhs.as.doubn) {
                return OrderEqual;
            } else if (this->as.doubn > rhs.as.doubn) {
                return OrderGreater;
            } else {
                return OrderLess;
            }
        case VkChar:
            if (this->as.ch == rhs.as.ch) {
                return OrderEqual;
            } else if (this->as.ch > rhs.as.ch) {
                return OrderGreater;
            } else {
                return OrderLess;
            }
        case VkObj:
            switch (rhs.as.obj->type) {
                case OtArray: {
                    LambArray *larr = (LambArray *)this->as.obj;
                    LambArray *rarr = (LambArray *)rhs.as.obj;

                    i32 llen = larr->items.len();
                    i32 rlen = rarr->items.len();
                    i32 min_len = llen < rlen ? llen : rlen;

                    Order element_order = OrderEqual;
                    for (i32 i = 0; i < min_len; i++) {
                        if ((element_order = larr->items[i].cmp(rarr->items[i])) != OrderEqual) {
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
                    LambString *larr = (LambString *)this->as.obj;
                    LambString *rarr = (LambString *)rhs.as.obj;

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
                    if (this->as.obj == rhs.as.obj) {
                        return OrderEqual;
                    } else if (this->as.obj > rhs.as.obj) {
                        return OrderGreater;
                    } else {
                        return OrderLess;
                    }
            }
    }

    fprintf(stderr, "Fallthrough found in switch in %s at %s", __FILE__, __FUNCTION__);
    return OrderLess;
}

constexpr char const* Value::kind_as_cstr() const {
    switch (this->kind) {
        case VkNil:
            return "nil";
        case VkBool:
            return "bool";
        case VkInt:
            return "int";
        case VkDouble:
            return "double";
        case VkChar:
            return "char";
        case VkObj: {
            switch (this->as.obj->type) {
                case OtString:
                    return "string";
                case OtArray:
                    return "array";
                case OtFunc:
                    return "fn";
                case OtNative:
                    return "native fn";
                case OtClosure:
                    return "closure fn";
                case OtUpvalue:
                    return "upvalue";
            }
            break;
        }
    }

    fprintf(stderr, "Unknown kind value: %d", this->kind);
    return "unknown";
}
