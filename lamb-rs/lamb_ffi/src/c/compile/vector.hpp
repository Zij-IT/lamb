#ifndef VECTOR_HEADER
#define VECTOR_HEADER

#include "../types.hpp"
#include "memory.hpp"
#include <optional>

// Forward declaration. See "../vm/vm.hpp"
struct Vm;

template<typename T> 
class GcVec {
    i32 capacity;
    i32 _len;
    T*  values;

public:
    GcVec();

    void push(Vm* vm, T item);

    void destroy(Vm* vm);

    constexpr i32 len() const { return this->_len; };

    constexpr T& operator[](i32 idx) const { return this->values[idx]; };
};

#endif
