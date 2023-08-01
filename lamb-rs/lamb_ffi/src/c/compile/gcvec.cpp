#include "gcvec.hpp"
#include "../types.hpp"
#include "../vm/vm.hpp"
#include "compiler.hpp"
#include "gc.hpp"
#include "value.hpp"

template <typename T> GcVec<T>::GcVec() : capacity(0), _len(0), values(nullptr) {}

template <typename T> void GcVec<T>::push(Vm &vm, T item) {
    if (this->capacity < this->_len + 1) {
        i32 old_cap = this->capacity;
        this->capacity = GROW_CAPACITY(old_cap);
        this->values = (T *)vm.gc.grow_array(vm, this->values, sizeof(T), old_cap, this->capacity);
    }

    this->values[this->_len] = item;
    this->_len += 1;
}

template <typename T> void GcVec<T>::destroy(Vm &vm) {
    vm.gc.free_array(this->values, sizeof(T), this->capacity);
    this->_len = 0;
    this->capacity = 0;
    this->values = nullptr;
}

template class GcVec<Value>;
template class GcVec<Local>;
template class GcVec<u8>;
