#include "native.hpp"
#include "../compile/compiler.hpp"
#include "../compile/misc.hpp"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <iostream>

static void define_native(Vm& vm, char const* fn_name, CFunc function) {
    auto interned = LambString::from_cstr(vm, fn_name);
    vm.push_stack(Value::from_obj((Object *)interned));
    vm.push_stack(Value::from_obj((Object *)NativeFunc::alloc(vm, function)));
    vm.globals.insert(vm, (LambString *)(vm.stack[0].as.obj), vm.stack[1]);

    vm.pop_stack();
    vm.pop_stack();
}
static Value lamb_print(i32 arg_count, Value *args) {
    if (arg_count != 0) {
        auto st = args->to_string();
        printf("%s", st.c_str());
    }

    return Value::nil();
}

static Value lamb_println(i32 arg_count, Value *args) {
    if (arg_count != 0) {
        auto st = args->to_string();
        printf("%s", st.c_str());
    }
    printf("\n");
    return Value::nil();
}

static Value lamb_NATIVE_assert(i32 arg_count, Value *args) {
    if (arg_count == 2) {
        bool assertion_failed = args[0].is_bool() && !args[0].as.boolean;
        bool assertion_explanation = args[1].is_object() && args[1].as.obj->is(OtString);

        if (assertion_failed && assertion_explanation) {
            LambString *str = (LambString *)args[1].as.obj;
            fprintf(stderr, "Assertion failed: %s", str->chars);
        } else if (assertion_failed) {
            fprintf(stderr, "Assertion failed.");
        }
    }

    return Value::nil();
}

static Value lamb_NATIVE_assert_eq(i32 arg_count, Value *args) {
    if (arg_count == 2) {
        switch (args->cmp(args[1])) {
            case OrderEqual:
                return Value::nil();
            case OrderGreater:
            case OrderLess:
                std::cerr << "Equality assertion failed:\n"
                          << "lhs: " << args[0].to_string()
                          << "rhs: " << args[1].to_string()
                          << std::endl;
                break;
        }
    }

    return Value::nil();
}

static Value lamb_user_int(__attribute__((unused)) i32 arg_count,
                           __attribute__((unused)) Value *args) {
    char x_buffer[80];
    if (fgets(x_buffer, 80, stdin) == NULL) {
        return Value::nil();
    } else {
        return Value::from_i64(atoi(x_buffer));
    }
}

static Value lamb_user_char(__attribute__((unused)) i32 arg_count,
                            __attribute__((unused)) Value *args) {
    return Value::from_char(fgetc(stdin));
}

static Value lamb_rand(i32 arg_count, Value *args) {
    if (arg_count == 1 && args->is_integer()) {
        return Value::from_i64(rand() % args->as.intn);
    }

    return Value::from_i64(rand());
}

static Value lamb_len(i32 arg_count, Value *args) {
    if (arg_count == 1 && args->kind == VkObj) {
        switch (args->as.obj->type) {
            case OtArray:
                return Value::from_i64(((LambArray *)args->as.obj)->items.len());
            case OtString:
                return Value::from_i64(((LambString *)args->as.obj)->len);
            default:
                return Value::nil();
        }
    }

    return Value::nil();
}

void set_natives(Vm& vm) {
    define_native(vm, "print", lamb_print);
    define_native(vm, "println", lamb_println);
    define_native(vm, "user_int", lamb_user_int);
    define_native(vm, "user_char", lamb_user_char);
    define_native(vm, "rand", lamb_rand);
    define_native(vm, "len", lamb_len);
    define_native(vm, "assert", lamb_NATIVE_assert);
    define_native(vm, "assert_eq", lamb_NATIVE_assert_eq);
}
