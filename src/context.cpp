//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include "context.h"

namespace yang {

const Context::type_map& Context::get_types() const
{
  return _types;
}

const Context::function_map& Context::get_functions() const
{
  return _functions;
}

// End namespace yang.
}
