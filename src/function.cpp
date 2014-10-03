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
  auto t = ptr->get_raw_representation();
  auto o = other.ptr->get_raw_representation();
  return t.function_ptr == o.function_ptr &&
      t.environment_ptr == o.environment_ptr;
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
