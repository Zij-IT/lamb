#ifndef GC_HEADER
#define GC_HEADER

#include "../types.hpp"
#include "compiler.hpp"
#include "object.hpp"
#include <vector>

class MarkAndSweep {
    void collect(Vm& vm); 

    void mark_roots(Vm& vm);

    void mark_compiler(Vm& vm, Compiler* compiler);

    void mark_table(Vm& vm, Table& table);

    template<typename T>
    void mark_gcvec(Vm& vm, GcVec<T>& vec);

    void mark_object(Vm& vm, Object* object);

    void mark_value(Vm& vm, Value* value);
  
    void trace_refs(Vm&);

    void sweep_unused(Vm&);

  public:
    void* reallocate(Vm& vm, void *ptr, size_t old_size, size_t new_size);
};

#endif
