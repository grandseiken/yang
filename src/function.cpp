//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include <yang/function.h>
#include "ircommon.h"

namespace yang {
namespace internal {

ErasedFunction::ErasedFunction()
  : type(Type::void_t())
  , native_ref(nullptr)
  , env_ref(nullptr)
  , yang_function(nullptr)
{
}

bool ErasedFunction::operator==(const ErasedFunction& other) const
{
  return type == other.type && native_ref == other.native_ref &&
      env_ref == other.env_ref && yang_function == other.yang_function;
}

bool ErasedFunction::operator!=(const ErasedFunction& other) const
{
  return !operator==(other);
}

void_fp get_global_trampoline_function(const Type& type)
{
  return YangTrampolineGlobals::get_trampoline_function(type);
}

}} // ::yang::internal
