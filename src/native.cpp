//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include <yang/native.h>

#include "memory.h"
#include "irval.h"

namespace yang {
namespace internal {

void native_function_internals_destructor(Prefix* function_internals)
{
  ((NativeFunctionInternals*)function_internals)->~NativeFunctionInternals();
}
Vtable native_function_internals_vtable(
    native_function_internals_destructor, 0, nullptr);

NativeFunctionInternals* allocate_native_function_internals()
{
  void* memory = YANG_MALLOC(sizeof(NativeFunctionInternals));
  NativeFunctionInternals* chunk = new (memory) NativeFunctionInternals;

  chunk->parent = nullptr;
  chunk->refcount = 0;
  chunk->vtable = &native_function_internals_vtable;
  return chunk;
}

// End namespace yang::internal.
}
}
