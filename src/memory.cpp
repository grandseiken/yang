//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include "memory.h"
#include <cstdlib>

namespace yang {
namespace internal {

std::unordered_set<Prefix*>& get_instrumented_heap()
{
  static std::unordered_set<Prefix*> instrumented_heap;
  return instrumented_heap;
}

void* instrumented_malloc(std::size_t size)
{
  void* ptr = malloc(size);
  get_instrumented_heap().insert((Prefix*)ptr);
  return ptr;
}

void instrumented_free(void* ptr)
{
  get_instrumented_heap().erase((Prefix*)ptr);
  free(ptr);
}

// End namespace yang::internal.
}
}
