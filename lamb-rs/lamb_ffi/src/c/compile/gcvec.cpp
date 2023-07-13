#include "gcvec.hpp"
#include "memory.hpp"
#include "compiler.hpp"

template<typename T> 
GcVec<T>::GcVec() {
  this->_len = 0;
  this->capacity = 0;
  this->values = nullptr;
}

template<typename T>
void GcVec<T>::push(Vm& vm, T item) {
    if (this->capacity < this->_len + 1) {
        i32 old_cap = this->capacity;
        this->capacity = GROW_CAPACITY(old_cap);
        this->values = GROW_ARRAY(vm, T, this->values, old_cap, this->capacity);;
    }

    this->values[this->_len] = item;
    this->_len += 1;
}

template<typename T>
void GcVec<T>::destroy(Vm& vm) {
    FREE_ARRAY(vm, T, this->values, this->capacity);
    this->_len = 0;
    this->capacity = 0;
    this->values = nullptr;
}

template class GcVec<Value>;
template class GcVec<Local>;
template class GcVec<u8>;
