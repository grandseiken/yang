//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include <yang/context.h>
#include <yang/function.h>
#include "ircommon.h"

namespace yang {
namespace internal {

const ContextInternals& context_internals(const Context& context)
{
  return *context._internals;
}

yang::void_fp get_global_trampoline_function(const yang::Type& type)
{
  return YangTrampolineGlobals::get_trampoline_function(type);
}

// End namespace yang::internal.
}
}
