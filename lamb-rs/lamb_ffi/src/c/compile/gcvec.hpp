#ifndef VECTOR_HEADER
#define VECTOR_HEADER

#include "../types.hpp"
#include <iterator>
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


    // Safety note:
    // * Sadly it is a requirement of the caller to make sure that GcVec
    //   is not modified while incrementing through it. I want my borrow
    //   checker... :(
    struct GcVecIter 
    {
        using iterator_category = std::forward_iterator_tag;
        using difference_type   = std::ptrdiff_t;
        using value_type        = T;
        using pointer           = T*;  // or also value_type*
        using reference         = T&;  // or also value_type&

        GcVecIter(T* data): data(data) {}

        reference operator*() const { return *this->data; }
        pointer operator->() { return this->data; }
        reference operator[](difference_type n) const { return this->data[n]; }

        GcVecIter& operator++() { this->data++; return *this; }  
        GcVecIter  operator++(int) { GcVecIter tmp = *this; ++(*this); return tmp; }

        GcVecIter& operator--() { this->data--; return *this; }  
        GcVecIter  operator--(int) { GcVecIter tmp = *this; --(*this); return tmp; }

        GcVecIter& operator+=(const difference_type n) { (*this) += n; return *this; }

        GcVecIter& operator-=(const difference_type n) { (*this) -= n; return *this; }


        friend difference_type operator-(const GcVecIter& lhs, const GcVecIter& rhs) { return lhs.data - rhs.data; }

        friend GcVecIter operator+(const GcVecIter& lhs, difference_type rhs) { return GcVecIter(lhs.data + rhs); }
        friend GcVecIter operator-(const GcVecIter& lhs, difference_type rhs) { return GcVecIter(lhs.data - rhs); }
        friend GcVecIter operator+(difference_type lhs, const GcVecIter& rhs) { return GcVecIter(lhs + rhs.data); }
        friend GcVecIter operator-(difference_type lhs, const GcVecIter& rhs) { return GcVecIter(lhs - rhs.data); }

        friend bool operator==(const GcVecIter& lhs, const GcVecIter& rhs) { return lhs.data == rhs.data; };
        friend bool operator!=(const GcVecIter& lhs, const GcVecIter& rhs) { return lhs.data != rhs.data; };
        friend bool operator>=(const GcVecIter& lhs, const GcVecIter& rhs) { return lhs.data >= rhs.data; }
        friend bool operator<=(const GcVecIter& lhs, const GcVecIter& rhs) { return lhs.data <= rhs.data; }
        friend bool operator> (const GcVecIter& lhs, const GcVecIter& rhs) { return lhs.data  > rhs.data; }
        friend bool operator< (const GcVecIter& lhs, const GcVecIter& rhs) { return lhs.data  < rhs.data; }

      private:
        pointer data;
    };

    constexpr GcVecIter begin() { return GcVecIter(this->values);              }
    constexpr GcVecIter end()   { return GcVecIter(this->values + this->_len); }

    constexpr std::reverse_iterator<GcVecIter> rbegin() { return std::make_reverse_iterator(this->end());   }
    constexpr std::reverse_iterator<GcVecIter> rend()   { return std::make_reverse_iterator(this->begin()); }
};

#endif
