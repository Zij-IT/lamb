#ifndef VECTOR_HEADER
#define VECTOR_HEADER

#include "../types.hpp"
#include "memory.hpp"
#include <optional>
#include <algorithm>

// Forward declaration. See "../vm/vm.hpp"
struct Vm;

// TODO: i32 should not be used for the length
//       this should probably be `usize` or at
//       least `u32`, as currently `truncate`
//       will be the cause of UB if `len` < 0
template<typename T> 
class GcVec {
    i32 capacity;
    i32 _len;
    T*  values;

public:
    GcVec();

    void push(Vm* vm, T item);

    constexpr T* as_raw() const { return this->values; };

    constexpr void truncate(i32 len) { this->_len = std::min(len, this->_len); };

    void destroy(Vm* vm);

    constexpr i32 len() const { return this->_len; };

    constexpr T& operator[](i32 idx) const { return this->values[idx]; };
};

#endif
