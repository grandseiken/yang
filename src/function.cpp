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
  auto t = ptr->get_yang_representation();
  auto o = other.ptr->get_yang_representation();
  return t.first == o.first && t.second == o.second;
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
