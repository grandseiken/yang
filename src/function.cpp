//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include <yang/function.h>
#include "ircommon.h"

namespace yang {
namespace internal {

bool GenericFunction::operator==(const GenericFunction& other) const
{
  if (type != other.type) {
    return false;
  }
  void* a = nullptr;
  void* b = nullptr;
  void* oa = nullptr;
  void* ob = nullptr;
  ptr->get_representation(&a, &b);
  other.ptr->get_representation(&oa, &ob);
  return a == oa && b == ob;
}

bool GenericFunction::operator!=(const GenericFunction& other) const
{
  return !operator==(other);
}

void_fp get_global_trampoline_function(const Type& type)
{
  return YangTrampolineGlobals::get_trampoline_function(type);
}

// End namespace yang::internal.
}
}
