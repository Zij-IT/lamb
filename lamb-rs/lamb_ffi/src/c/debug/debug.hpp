#ifndef COMPILING_DEBUG_HEADER
#define COMPILING_DEBUG_HEADER

#include <assert.h>

#include "../compile/object.hpp"

#define lamb_assert(msg, x) assert((((void)(msg)), (x)))

void chunk_debug(Chunk *chunk, char const* name);

void print_value(Value v);

void print_object(Object *obj);

#endif // COMPILING_DEBUG_HEADER
