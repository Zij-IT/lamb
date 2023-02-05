#ifndef COMPILING_DEBUG_HEADER
#define COMPILING_DEBUG_HEADER

#include "object.h"

void chunk_debug(Chunk *chunk, str name);

void print_value(Value v);

void print_object(Object *obj);

#endif // COMPILING_DEBUG_HEADER
