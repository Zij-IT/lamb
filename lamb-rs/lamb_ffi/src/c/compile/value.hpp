#ifndef VALUE_HEADER
#define VALUE_HEADER

#include <string>

#include "../types.hpp"

// Forward declare from object.h
struct Object;
struct LambString;
struct Vm;

enum ValueKind {
    VkBool,
    VkInt,
    VkDouble,
    VkChar,
    VkObj,
    VkNil,
};

struct Value {
    ValueKind kind;
    union {
        bool boolean;
        i64 intn;
        f64 doubn;
        char ch;
        Object *obj;
    } as;

    constexpr static Value from_bool(bool b) {
        Value val = {.kind = VkBool, .as = {.boolean = b}};
        return val;
    }

    constexpr static Value from_i64(i64 i){
        Value val = {.kind = VkInt, .as = { .intn = i }};
        return val;
    }

    constexpr static Value from_f64(f64 i) {
        Value val = {.kind = VkDouble, .as = { .doubn = i }};
        return val;
    }

    constexpr static Value from_char(char c) {
        Value val = {.kind = VkChar, .as = {.ch = c}};
        return val;
    }

    constexpr static Value from_obj(Object *obj) {
        Value val = {.kind = VkObj, .as = {.obj = obj}};
        return val;
    }

    constexpr static Value nil() {
        Value val = {
            .kind = VkNil,
            .as = {.boolean = false},
        };
        return val;
    }

    constexpr bool is_bool() const { return this->kind == VkBool; }

    constexpr bool is_double() const { return this->kind == VkDouble; }

    constexpr bool is_integer() const { return this->kind == VkInt; }

    constexpr bool is_char() const { return this->kind == VkChar; }

    constexpr bool is_object() const { return this->kind == VkObj; }

    constexpr bool is_nil() const { return this->kind == VkNil; }

    constexpr char const* kind_as_cstr() const;

    Order cmp(Value const& rhs) const;

    std::string to_string() const;
};

#endif // VALUE_HEADER
