//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include <yang/context.h>

namespace yang {

Context::Context()
  : _internals{new internal::ContextInternals}
{
}

void Context::check_identifier(const std::string& ident) const
{
  for (char c : ident) {
    if (!(c >= 'a' && c <= 'z') && !(c >= 'A' && c <= 'Z') &&
        !(c >= '0' && c <= '9') && c != '_') {
      throw runtime_error("invalid identifier `" + ident + "`");
    }
  }
}

// End namespace yang.
}
