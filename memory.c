#include "memory.h"

#include <stdio.h>


// Work as specified in the table:
//
//  | old_size ----|---- new_size ----|---- operation ----|
//  |--------------|------------------|-------------------|
//  | 0            | non-zero         | allocate new block|
//  |---------------------------------|-------------------|
//  | non-zero     | smaller than old | shrink existing   |
//  |--------------|------------------|-------------------|
//  | non-zero     | larger than old  | grow exisitng     |
//  -------------------------------------------------------
void* reallocate(void* ptr, size_t old_size, size_t new_size) {
  if(new_size == 0) {
    free(ptr);
    return NULL;
  }
  
  void* result = realloc(ptr, new_size);
  if (result == NULL) {
    fprintf(stderr, "LambCompiler: Ran out of memory... sorry :(");
    exit(1);
  }

  return result;
}
