#ifndef VECTOR_HEADER
#define VECTOR_HEADER

#include "../types.hpp"
#include "memory.hpp"
#include <optional>
#include <algorithm>

// Forward declaration. See "../vm/vm.hpp"
struct Vm;

template<typename T> 
class GcVec {
    u32 capacity;
    u32 _len;
    T*  values;

public:
    GcVec();

    void push(Vm& vm, T item);

    constexpr T* as_raw() const { return this->values; };

    constexpr void truncate(u32 len) { this->_len = std::min(len, this->_len); };

    void destroy(Vm& vm);

    constexpr i32 len() const { return this->_len; };

    constexpr T& operator[](u32 idx) const { return this->values[idx]; };
};

#endif
