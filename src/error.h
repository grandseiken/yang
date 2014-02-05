//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#ifndef YANG_SRC_ERROR_H
#define YANG_SRC_ERROR_H

#include <stdexcept>
#include <string>

namespace yang {
  class runtime_error : public std::runtime_error {
  public:
    runtime_error(const std::string& what)
      : std::runtime_error(what) {}
  };
}

#endif
